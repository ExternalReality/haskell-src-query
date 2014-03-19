;;; shm.el --- Structured Haskell Mode

;; Copyright (c) 2013 Chris Done. All rights reserved.
;; Copyright (c) 1998 Heribert Schuetz, Graeme E Moss

;; Author:    Chris Done <chrisdone@gmail.com>
;; Created:   19-Oct-2013
;; Version:   1.0.2
;; Keywords:  development, haskell, structured
;; Stability: unstable

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A minor mode for adding structured editing to Haskell.

;;; Code:

(require 'shm-customizations)
(require 'shm-ast-documentation)
(require 'shm-evaporate)
(require 'shm-node)
(require 'shm-lambda)
(require 'shm-context-menu)
(require 'hsq-cabal)
(require 'hsq-spellcheck)

(require 'cl)
(require 'json)

(defvar shm-current-node-overlay nil
  "Overlay to highlight the current node.")

(defvar shm-decl-asts nil
  "This is partly an optimization and partly for more
functionality. We could parse the whole module, but that would be
wasteful and expensive to lookup nodes every time we want a
node. So it's cheaper to have the granularity of lookup start at
the declaration's point and the node's span.

Second it's better because a module may have unparseable content
in it, but that doesn't mean we don't want structured editing to
stop working on declarations that are fine. I've found in my use
of SHM that this is a common use-case worth taking into account.")

(defvar shm-string-node nil
  "The string node that's currently being edited.")

(defvar shm-string-buffer nil
  "The buffer of the string node that's currently being edited.")

(defvar shm-lighter " SHM?"
  "The lighter for structured Haskell mode.")

(defvar shm-last-point 0
  "When moving around, the current node overlay will update
  according to where you are. But often you can shrink/expand the
  scope of the current node. This variable lets us avoid the node
  being reset by realising we haven't actually moved the point.")

(defvar shm-parsing-timer nil
  "The timer used to re-parse every so often. The idle time can
  be configured with `shm-idle-timeout'.")

(defvar shm-last-parse-start 0
  "This is used to avoid unnecessary work, if the start of the
  declaration hasn't changed, and the end (see
  `shm-last-parse-end') since we last parsed, don't bother
  re-parsing.")

(defvar shm-last-parse-end 0
  "See `shm-last-parse-start' for explanation.")

(defvar shm-last-yanked (list 0 0)
  "When yanking, some text will be inserted, when popping a
  yank (i.e. with M-y), you need to be able to erase the previous
  yank. This is simply a region.")

(defvar shm-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (if (eq window-system 'x)
        (define-key map (kbd "M-<return>") 'shm/present-actions-for-node)
      (define-key map (kbd "M-]") 'shm/present-actions-for-node))
    (define-key map (kbd "C-M-f") 'shm/forward-node)
    (define-key map (kbd "C-M-b") 'shm/backward-node)
    (define-key map (kbd "M-a") 'shm/goto-parent)
    (define-key map (kbd "M-}") 'shm/forward-paragraph)
    (define-key map (kbd "M-{") 'shm/backward-paragraph)
    (define-key map (kbd "C-M-SPC") 'shm/mark-node)
    (define-key map (kbd "C-c C-w") 'shm/goto-where)
    (define-key map (kbd "C-c C-q") 'shm/qualify-import)
    (define-key map (kbd "M-p") 'shm/walk)
    map)
  "Structural editing operations keymap. Any key bindings in this
  map are intended to be only structural operations which operate
  with the tree in mind.")

;;;###autoload
(define-minor-mode structured-haskell-mode
  "Structured editing for Haskell."
  :lighter shm-lighter
  :keymap shm-map
  (if structured-haskell-mode
      (shm-mode-start)
    (shm-mode-stop)))

(defmacro shm-with-fallback (fallback &rest body)
  "Perform the given action unless we're in a comment, in which
  case run the fallback function insteaad."
  `(if (shm-in-comment)
       (call-interactively ',fallback)
     (if debug-on-error
         (progn ,@body)
       (condition-case e
           (progn ,@body)
         (error
          (message "(SHM command failed, falling back to %S. Run M-: (setq debug-on-error t) to see the error.)"
                   ',fallback)
          (call-interactively ',fallback))))))

(defun shm-mode-start ()
  "Start the minor mode."
  (set (make-local-variable 'shm-decl-asts)
       nil)
  (set (make-local-variable 'shm-current-node-overlay)
       nil)
  (add-hook 'post-self-insert-hook 'shm-post-self-insert nil t)
  (unless shm-parsing-timer
    (setq shm-parsing-timer
          (run-with-idle-timer shm-idle-timeout t 'shm-reparsing-timer))))

(defun shm-post-self-insert ()
  "Self-insertion handler."
  (save-excursion
    (shm-appropriate-adjustment-point)
    (forward-char -1)
    (shm-adjust-dependents (point) 1)))

(defun shm-mode-stop ()
  "Stop the minor mode. Restore various settings and clean up any
state that will hopefully be garbage collected."
  ;; Kill the timer.
  (cancel-timer shm-parsing-timer)
  (setq shm-parsing-timer nil)
  ;; Delete all markers.
  (mapc (lambda (pair)
          (mapc #'shm-node-delete-markers
                (cdr pair))
          (set-marker (car pair) nil))
        shm-decl-asts)
  ;; Delete all overlays.
  (shm-delete-overlays (point-min) (point-max) 'shm-current-overlay)
  (shm-delete-overlays (point-min) (point-max) 'shm-quarantine)
  ;; Reset variables.
  (setq shm-decl-asts nil)
  (setq shm-current-node-overlay nil)
  (setq shm-last-parse-start 0)
  (setq shm-last-parse-end 0)
  (setq shm-last-point 0))

(defun shm-reparsing-timer ()
  "Re-parse the tree on the idle timer."
  (when structured-haskell-mode
    (shm/reparse)
    (hsq/spellcheck)))


(defun shm-decl-ast (&optional reparse)
  "Return the AST representing the current declaration at point.

If the AST has already been loaded, that is returned immediately,
otherwise it's regenerated. See the Internal AST section below
for more information."
  (let ((p (shm-decl-points)))
    (when p
      (shm-get-decl-ast (car p)
                        (cdr p)
                        reparse))))

(defun shm-set-decl-ast (point ast)
  "Store the given decl AST at the given POINT. If there is
already an AST for a decl at the given point then remove that one
and instate this one."
  (setq shm-decl-asts
        (cons
         (cons (set-marker (make-marker) point) ast)
         (remove-if (lambda (pair)
                      (when (= (marker-position (car pair))
                               point)
                        (set-marker (car pair) nil)
                        t))
                    shm-decl-asts)))
  ast)

(defun shm-get-decl-ast (start end &optional reparse)
  "Get the AST of the declaration starting at POINT."
  (let ((pair (car (remove-if-not (lambda (pair)
                                    (= (marker-position (car pair))
                                       start))
                                  shm-decl-asts))))
    (if (and (not reparse)
             pair)
        (cdr pair)
      (progn
        (when (or (/= start shm-last-parse-start)
                  (/= end shm-last-parse-end))
          (setq shm-last-parse-start start)
          (setq shm-last-parse-end end)
          (let ((ast (shm-get-nodes (shm-get-ast "decl"
                                                 start
                                                 end)
                                    start
                                    end)))
            (if ast
                (progn (setq shm-lighter " SHM")
                       (when pair
                         (shm-delete-markers pair))
                       (shm-set-decl-ast start ast)
                       ;; Delete only quarantine overlays.
                       (shm-delete-overlays (point-min) (point-max) 'shm-quarantine)
                       (shm/init)
                       ast)
              (progn
                (when shm-display-quarantine
                  (shm-quarantine-overlay start end))
                (setq shm-lighter " SHM!")
                nil))))))))

(defun shm-delete-markers (decl)
  "Delete the markers in DECL."
  (mapc #'shm-node-delete-markers
        (cdr decl)))

(defun shm-get-ast (type start end)
  "Get the AST for the given region at START and END. Parses with TYPE.

This currently launches a fresh process and uses this buffer
nonsense, for any parse, which sucks, but is fast enough _right
now_. Later on a possibility to make this much faster is to have
a persistent running parser server and than just send requests to
it, that should bring down the roundtrip time significantly, I'd
imagine."
  (let ((message-log-max nil)
        (buffer (current-buffer)))
    (when (> end (1+ start))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer buffer
            (condition-case e
                (call-process-region start
                                     end
                                     shm-program-name
                                     nil
                                     temp-buffer
                                     nil
                                     "parse")
              ((file-error)
               (error "Unable to find structured-haskell-mode executable! See README for help.")))))
        (json-read-from-string (buffer-string))))))

(defun shm-lint-ast (type start end)
  "Get refactor suggestions for the region of TYPE from START to END."
  (let ((message-log-max nil)
        (buffer (current-buffer)))
    (when (> end (1+ start))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer buffer
            (condition-case e
                (call-process-region start
                                     end
                                     shm-program-name
                                     nil
                                     temp-buffer
                                     nil
                                     "hlint")
              ((file-error)
               (error "Unable to find structured-haskell-mode executable! See README for help.")))))
        (read (buffer-string))))))

(defun shm-get-nodes (ast start end)
  "Get the nodes of the given AST.

We convert all the line-col numbers to Emacs points and then
create markers out of them. We also store the type of the node,
e.g. Exp, and the case of the node, e.g. Lit or Case or Let,
which is helpful for doing node-specific operations like
indentation.

Any optimizations welcome."
  (let* ((start-end (cons start end))
         (start-column (save-excursion (goto-char start)
                                       (current-column))))
    (cond ((vectorp ast)
           (save-excursion
             (map 'vector
                  (lambda (node)
                    (vector
                     (elt node 0)
                     (elt node 1)
                     (progn (goto-char (car start-end))
                            (forward-line (1- (elt node 2)))
                            ;; This trick is to ensure that the first
                            ;; line's columns are offsetted for
                            ;; regions that don't start at column
                            ;; zero.
                            (goto-char (+ (if (= (elt node 2) 1)
                                              start-column
                                            0)
                                          (1- (+ (point) (elt node 3)))))
                            (let ((marker (set-marker (make-marker) (point))))
                              marker))
                     (progn (goto-char (car start-end))
                            (forward-line (1- (elt node 4)))
                            ;; Same logic as commented above.
                            (goto-char (+ (if (= (elt node 4) 1)
                                              start-column
                                            0)
                                          (1- (+ (point) (elt node 5)))))
                            ;; This avoids the case of:
                            (while (save-excursion (goto-char (line-beginning-position))
                                                   (or (looking-at "[ ]+-- ")
                                                       (looking-at "[ ]+$")))
                              (forward-line -1)
                              (goto-char (line-end-position)))
                            (let ((marker (set-marker (make-marker) (point))))
                              (set-marker-insertion-type marker t)
                              marker))))
                  ast)))
          (t nil))))

(defun shm-decl-points (&optional use-line-comments)
  "Get the start and end position of the current
declaration. This assumes that declarations start at column zero
and that the rest is always indented by one space afterwards, so
Template Haskell uses with it all being at column zero are not
expected to work."
  (cond
   ;; If we're in a block comment spanning multiple lines then let's
   ;; see if it starts at the beginning of the line (or if any comment
   ;; is at the beginning of the line, we don't care to treat it as a
   ;; proper declaration.
   ((and (not use-line-comments)
         (shm-in-comment)
         (save-excursion (goto-char (line-beginning-position))
                         (shm-in-comment)))
    nil)
   ((save-excursion
      (goto-char (line-beginning-position))
      (or (looking-at "^-}$")
          (looking-at "^{-$")))
    nil)
   ;; Otherwise we just do our line-based hack.
   (t
    (save-excursion
      (let ((start (or (progn (goto-char (line-end-position))
                              (search-backward-regexp "^[^ \n]" nil t 1)
                              (unless (or (looking-at "^-}$")
                                          (looking-at "^{-$"))
                                (point)))
                       0))
            (end (progn (goto-char (1+ (point)))
                        (or (when (search-forward-regexp "[\n]+[^ \n]" nil t 1)
                              (forward-char -1)
                              (search-backward-regexp "[^\n ]" nil t)
                              (forward-char)
                              (point))
                            (point-max)))))
        (cons start end))))))

(defun shm-decl-node (start)
  "Get the top-level node of the declaration."
  (let* ((vector (save-excursion (goto-char start)
                                 (shm-decl-ast))))
    (elt vector 0)))

(defun shm/backward-kill-word ()
  "Kill the word backwards."
  (interactive)
  (let ((to-be-deleted (save-excursion (backward-word)
                                       (point))))
    (save-excursion
      (shm-appropriate-adjustment-point)
      (shm-adjust-dependents (point) (* -1 (- (point) to-be-deleted))))
    (backward-kill-word 1)))

(defun shm-appropriate-adjustment-point ()
  "Go to the appropriate adjustment point.

This is called before calling `shm-adjust-dependents', because some places, e.g.

zoo = do
  bar
  mu

If the point is at 'z', then we should *not* move 'bar' or 'mu',
even though we normally would. To avoid doing this, we use a very
simple but 90% effective (100% is rather hard, will not be
appearing in a beta version) heuristic. We jump to here:

zoo| = do
  bar
  mu

And use our normal adjustment test there. After all, only thing
after 'zoo' are *really* dependent."
  (let ((current (shm-current-node)))
    (when (and current
               (<= (shm-node-end current) (line-end-position)))
      (goto-char (shm-node-end current)))))



(defun shm-adjust-dependents (end-point n)
  "Adjust dependent lines by N characters that depend on this
line after END-POINT."
  (unless (= (line-beginning-position)
             (1- (point)))
    (let ((line (line-number-at-pos))
          (column (current-column)))
      (when (and (not (< column (shm-indent-spaces)))
                 (not (and (looking-back "^[ ]+")
                           (looking-at "[ ]*")))
                 (save-excursion (goto-char end-point)
                                 (forward-word)
                                 (= (line-number-at-pos) line)))
        (unless (save-excursion
                  (goto-char (line-end-position))
                  (let ((current-pair (shm-node-backwards)))
                    (when current-pair
                      (string= (shm-node-type-name (cdr current-pair))
                               "Rhs"))))
          (shm-move-dependents n
                               end-point))))))

(defun shm-move-dependents (n point)
  "Move dependent-with-respect-to POINT lines N characters forwards or backwards.

This is purely based on alignments. If anything is aligned after
the current column, then it's assumed to be a child of whatever
has recently changed at POINT, and thus we 'bring it along'
either forwards or backwards.

The algorithm isn't quite comprehensive, it needs special cases
for top-level functions and things like that."
  (save-excursion
    (let ((column (progn (goto-char point)
                         (current-column)))
          (point nil)
          (end-point nil))
      (while (and (= 0 (forward-line 1))
                  (or (not end-point)
                      (/= end-point (line-end-position))))
        (if (shm-line-indented-past (1+ column))
            (progn (unless point
                     (setq point (goto-char (line-beginning-position))))
                   (setq end-point (line-end-position)))
          (goto-char (point-max))))
      (when end-point
        (indent-rigidly point end-point n)))))

(defun shm-line-indented-past (n)
  "Is the current line indented past N?"
  (goto-char (line-beginning-position))
  (let ((column (search-forward-regexp "[^ ]" (line-end-position) t 1)))
    (if column
        (>= (1- (current-column)) n)
      t)))

(defun shm/delete ()
  "Delete the current node."
  (interactive)
  (let ((current (shm-current-node))
        (inhibit-read-only t))
    (delete-region (shm-node-start current)
                   (shm-node-end current))))

(defun shm/mark-node ()
  "Set the active mark to the current node."
  (interactive)
  (let ((current (shm-current-node)))
    (goto-char (shm-node-start current))
    (set-mark (shm-node-end current))))

(defun shm-indent-spaces ()
  "Get the number of spaces to indent."
  (if (boundp 'haskell-indent-spaces)
      haskell-indent-spaces
    shm-indent-spaces))

(defun shm/type-of-node ()
  (interactive)
  (let ((current (shm-current-node)))
    (cond
     ((or (string= (shm-node-type-name current) "Exp")
          (string= (shm-node-type-name current) "Decl")
          (string= (shm-node-type-name current) "Pat")
          (string= (shm-node-type-name current) "QOp"))
      (let ((type-info (shm-node-type-info current)))
        (if type-info
            (shm-present-type-info current type-info)
          (if (and shm-type-info-fallback-to-ghci
                   (fboundp 'haskell-process-do-type))
              (haskell-process-do-type)
            (error "Unable to get type information for that node.")))))
     ((and (string= (shm-node-type-name current) "Name")
           (let ((parent-name (shm-node-type-name (cdr (shm-node-parent (shm-current-node-pair))))))
             (or (string= parent-name "Match")
                 (string= parent-name "Decl"))))
      (let* ((node (cdr (shm-node-parent (shm-current-node-pair))))
             (type-info (shm-node-type-info node)))
        (if type-info
            (shm-present-type-info node type-info)
          (if (and shm-type-info-fallback-to-ghci
                   (fboundp 'haskell-process-do-type))
              (haskell-process-do-type)
            (error "Unable to get type information for that node (tried the whole decl, too).")))))
     (t (error "Not an expression, operator, pattern binding or declaration.")))))

(defun shm/describe-node (&optional node)
  "Present a description of the current node in the minibuffer.

Very useful for debugging and also a bit useful for newbies."
  (interactive)
  (let ((node (or node (shm-current-node))))
    (if node
        (message "%s" (shm-node-description node))
      (error "No current node."))))


(defun shm/goto-where ()
  "Either make or go to a where clause of the current right-hand-side."
  (interactive)
  (let ((node-pair (shm-current-node-pair))
        (vector (shm-decl-ast)))
    (loop for i
          downfrom (car node-pair)
          to -1
          until (or (= i -1)
                    (let ((node (elt vector i)))
                      (and (string= "Rhs"
                                    (shm-node-type-name node))
                           (<= (shm-node-start node)
                               (shm-node-start (cdr node-pair)))
                           (>= (shm-node-end node)
                               (shm-node-end (cdr node-pair))))))
          finally (return
                   (when (>= i 0)
                     (let ((rhs (elt vector i)))
                       (goto-char (shm-node-end rhs))
                       (cond
                        ((looking-at "[\n ]*where")
                         (search-forward-regexp "where[ \n]*"))
                        (t
                         (unless (= (line-beginning-position) (point))
                           (newline))
                         (indent-to
                          (+ 2
                             (shm-node-start-column
                              (cdr (shm-node-parent (cons i rhs))))))
                         (insert "where ")))))))))



(defun shm-find-furthest-parent-on-line (current)
  "Find the parent which starts nearest to column 0 on the
current line.

This is used when indenting dangling expressions."
  (let ((parent (shm-node-parent current)))
    (if parent
        (if (= (line-beginning-position)
               (save-excursion (goto-char (shm-node-start (cdr parent)))
                               (line-beginning-position)))
            (shm-find-furthest-parent-on-line parent)
          current)
      current)))


(defun shm/forward-paragraph ()
  "Go forward one declaration."
  (interactive)
  (unless (/= (point)
              (goto-char (cdr (shm-decl-points t))))
    (search-forward-regexp "[^\n ]" nil t 1)
    (backward-char)))

(defun shm/backward-paragraph ()
  "Go backward one declaration."
  (interactive)
  (unless (/= (point)
              (goto-char (car (shm-decl-points t))))
    (search-backward-regexp "[^\n ]" nil t 1)
    (forward-char)))

(defun shm/walk ()
  (interactive)
  (shm/reparse)
  (shm/goto-parent-end))

(defun shm/close-paren ()
  "Either insert a close paren or go to the end of the node."
  (interactive)
  (shm-with-fallback
   self-insert-command
   (if (shm-literal-insertion)
       (shm-insert-string ")")
     (progn (shm/reparse)
            (shm/goto-parent-end)))))

(defun shm/close-bracket ()
  "Either insert a close bracket or go to the end of the node."
  (interactive)
  (shm-with-fallback
   self-insert-command
   (if (shm-literal-insertion)
       (shm-insert-string "]")
     (progn (shm/reparse)
            (shm/goto-parent-end)))))

(defun shm/close-brace ()
  "Either insert a close brace or go to the end of the node."
  (interactive)
  (shm-with-fallback
   self-insert-command
   (if (shm-literal-insertion)
       (shm-insert-string "}")
     (progn (shm/reparse)
            (shm/goto-parent-end)))))

(defun shm/goto-parent-end ()
  "Set the current node overlay to the parent node, but go to the
  end rather than the start."
  (interactive)
  (shm/goto-parent nil 'end))

(defun shm/forward-node ()
  "Go forward by node, i.e. go to the next of the current node. If
we're already at the end of the current node, jump to the next
node."
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair)))
    (if (= (point) (shm-node-end current))
        (let ((next-pair (shm-node-next current-pair)))
          (goto-char (shm-node-start (cdr next-pair))))
      (goto-char (shm-node-end current)))))

(defun shm/backward-node ()
  "Go backward by node, i.e. go to the previous of the current node. If
we're already at the start of the current node, jump to the previous
node."
  (interactive)
  (let* ((current-pair (shm-current-node-pair))
         (current (cdr current-pair)))
    (if (= (point) (shm-node-start current))
        (let ((prev-pair (shm-node-previous current-pair)))
          (goto-char (shm-node-start (cdr prev-pair))))
      (goto-char (shm-node-start current)))))

(defun shm/goto-parent (&optional node-pair direction)
  "Set the current node overlay to the parent node-pair"
  (interactive)
  (let ((direction (or direction 'start)))
    (if shm-current-node-overlay
        (let* ((o shm-current-node-overlay)
               (parent-pair (shm-node-parent (or node-pair
                                                 (shm-current-workable-node)))))
          (when parent-pair
            (let ((parent (cdr parent-pair)))
              (if (and o
                       (overlay-buffer o)
                       (>= (shm-node-start parent)
                           (overlay-start o))
                       (<= (shm-node-end parent)
                           (overlay-end o)))
                  (shm/goto-parent parent-pair direction)
                (shm-set-node-overlay parent-pair direction)))))
      (when node-pair
        (shm-set-node-overlay node-pair direction)))))

(defun shm/reparse ()
  "Re-parse the current node.

This is used on the reparsing timer, but also on commands that
really need accurate AST information *right now*, so this will
force a reparse immediately (if necessary)."
  (interactive)
  (shm-decl-ast t)
  (when (/= shm-last-point (point))
    (shm-set-node-overlay)))

(defun shm-current-node ()
  "Return just the current node, without its index.

See `shm-current-node-pair' for what 'current' means."
  (cdr (shm-current-node-pair)))

(defun shm-actual-node ()
  "Return just the actual current node, without its index.

Normally node functions only care about the current workable
node. This function will return the *actual* node at point. See
`shm-current-node-pair' for what 'workable' means."
  (cdr (shm-node-backwards)))

(defun shm-current-node-pair ()
  "Return the current workable node at point.

Workable means that it is something that we want to be able to
parse.

For example, if we're looking at a Name,

foobar

then that is all well and good, but we don't want to edit a Name,
nor a QName (the parent), we want to edit an Exp (parent-parent)
whose constructor will be a Var."
  (let ((current (shm-node-backwards)))
    (when current
      (if (and shm-current-node-overlay
               (overlay-buffer shm-current-node-overlay)
               (or (= (shm-node-start (cdr current))
                      (overlay-start shm-current-node-overlay))
                   (= (shm-node-end (cdr current))
                      (overlay-end shm-current-node-overlay))))
          (overlay-get shm-current-node-overlay 'node-pair)
        (shm-workable-node current)))))

(defun shm-current-workable-node ()
  "Returns the same as `shm-current-node' but including the index."
  (let ((current (shm-node-backwards)))
    (when current
      (shm-workable-node current))))

(defun shm-workable-node (current-pair)
  "Assume that the given CURRENT node is not workable, and look
at the parent. If the parent has the same start/end position,
then the parent is the correct one to work with."
  (let* ((parent-pair (shm-node-parent current-pair))
         (parent (cdr parent-pair))
         (current (cdr current-pair)))
    (cond

     (t (if parent
            (if (and (= (shm-node-start current)
                        (shm-node-start parent))
                     (= (shm-node-end current)
                        (shm-node-end parent)))
                (if (string= (shm-node-type current) (shm-node-type parent))
                    current-pair
                  (shm-workable-node parent-pair))
              current-pair)
          current-pair)))))

(defun shm-node-previous (node-pair)
  "Get the previous node of NODE-PAIR."
  (let ((vector (shm-decl-ast)))
    (loop for i
          downfrom (car node-pair)
          to -1
          until (or (= i -1)
                    (let ((node (elt vector i)))
                      (<= (shm-node-end node)
                          (shm-node-start (cdr node-pair)))))
          finally (return
                   (when (>= i 0)
                     (shm-workable-node (cons i
                                              (elt vector i))))))))

(defun shm-node-delete-markers (n)
  "Set the markers to NIL, which is about the best we can do for
deletion. The markers will be garbage collected eventually."
  (set-marker (elt n 2) nil)
  (set-marker (elt n 3) nil))

(defun shm-in-comment ()
  "Are we currently in a comment?"
  (or (and (eq 'font-lock-comment-delimiter-face
               (get-text-property (point) 'face))
           ;; This is taking liberties, but I'm not too sad about it.
           (not (save-excursion (goto-char (line-beginning-position))
                                (looking-at "{-"))))
      (eq 'font-lock-doc-face
          (get-text-property (point) 'face))
      (and (eq 'font-lock-comment-face
               (get-text-property (point) 'face))
           (not (save-excursion (goto-char (line-beginning-position))
                                (looking-at "{-"))))
      (save-excursion (goto-char (line-beginning-position))
                      (looking-at "^\-\- "))))

(defun shm-in-string ()
  "Are we in a string?"
  (or (eq 'font-lock-string-face
          (get-text-property (point) 'face))))

(defun shm-find-overlay (type)
  "Find overlays at point."
  (remove-if-not (lambda (o) (overlay-get o type))
                 (overlays-in (point-min) (point-max))))

(defun shm-current-overlay (start end node-pair)
  "Make the overlay for current node at START to END, setting the
NODE-PAIR in the overlay."
  (let ((o (make-overlay start end nil nil t)))
    (overlay-put o 'shm-current-overlay t)
    (overlay-put o 'face 'shm-current-face)
    (overlay-put o 'node-pair node-pair)
    (overlay-put o 'priority 1)
    o))

(defun shm-quarantine-overlay (start end)
  "Make a quarantine from START to END."
  (let ((o (make-overlay start end nil nil t)))
    (overlay-put o 'shm-quarantine t)
    (overlay-put o 'face 'shm-quarantine-face)
    (overlay-put o 'priority 0)
    o))

(defun shm-set-node-overlay (&optional node-pair jump-direction)
  "Set the current overlay for the current node. Optionally pass
NODE-PAIR to use the specific node-pair (index + node)."
  (setq shm-current-node-overlay nil)
  (shm-delete-overlays (point-min)
                       (point-max)
                       'shm-current-overlay)
  (let* ((node-pair (or node-pair
                        (shm-current-node-pair)))
         (node (cdr node-pair)))
    (when jump-direction
      (if (eq jump-direction 'end)
          (goto-char (shm-node-end node))
        (goto-char (shm-node-start node))))
    (setq shm-last-point (point))
    (setq shm-current-node-overlay
          (when node
            (shm-current-overlay (shm-node-start node)
                                 (shm-node-end node)
                                 node-pair)))))

(defun shm-delete-overlays (start end type)
  "Delete overlays of the given type. This is used for both
current overlay and quarantines."
  (mapc (lambda (o)
          (when (overlay-get o type)
            (delete-overlay o)))
        (overlays-in start end)))

(defun shm/init (&optional force-renew)
  "Initialize the current node overlay at point.

FORCE-RENEW would be used when the buffer has changed and
therefore the current overlay should be re-initialized."
  (interactive)
  (when force-renew
    (setq shm-current-node-overlay nil))
  (shm-set-node-overlay))

(defun shm-type-of-region (beg end)
  "Get a type for the region."
  (let ((types (shm-types-at-point beg)))
    (loop for type
          in types
          do (when (and (= (elt type 0) beg)
                        (= (elt type 1)
                           end))
               (return (elt type 2))))))

(defun shm-types-at-point (point)
  "Get a list of spans and types for the current point."
  (save-excursion
    (goto-char point)
    (let ((line (line-number-at-pos))
          (col (1+ (current-column)))
          (file-name (buffer-file-name)))
      (cond
       (shm-use-hdevtools
        (shm-parse-hdevtools-type-info
         (with-temp-buffer
           (call-process "hdevtools" nil t nil "type" "-g" "-fdefer-type-errors"
                         file-name
                         (number-to-string line)
                         (number-to-string col))
           (buffer-string))))))))

(defun shm-parse-hdevtools-type-info (string)
  "Parse type information from the output of hdevtools."
  (let ((lines (split-string string "\n+")))
    (loop for line
          in lines
          while (string-match "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \"\\(.+\\)\"$"
                              line)
          do (goto-char (point-min))
          collect
          (let ((start-line (string-to-number (match-string 1 line)))
                (end-line (string-to-number (match-string 3 line))))
            (vector (progn (forward-line (1- start-line))
                           (+ (line-beginning-position)
                              (1- (string-to-number (match-string 2 line)))))
                    (progn (when (/= start-line end-line)
                             (forward-line (1- (- start-line end-line))))
                           (+ (line-beginning-position)
                              (1- (string-to-number (match-string 4 line)))))
                    (match-string 5 line))))))


(defun shm/qualify-import ()
  "Toggle the qualification of the import at point."
  (interactive)
  (save-excursion
    (let ((points (shm-decl-points)))
      (goto-char (car points))
      (shm/reparse)
      (let ((current (shm-current-node)))
        (when (and current
                   (string= "ImportDecl"
                            (shm-node-type-name current)))
          (cond
           ((looking-at "import[\n ]+qualified[ \n]+")
            (search-forward-regexp "qualified" (shm-node-end current) t 1)
            (delete-region (point)
                           (search-backward-regexp "qualified"))
            (just-one-space 1))
           (t
            (search-forward-regexp "import")
            (shm-insert-string " qualified")
            (just-one-space 1))))))))

(defun shm/modify-type-constraint ()
  "Modify a type signatures constraint"
  (interactive)
  (let* ((pair (shm-current-node-pair))
         (current-node (cdr pair)))         
    (if (shm-type-signature-with-constraint-p pair)
        (shm-add-additional-type-constraint current-node)
      (add-initial-type-constraint current-node))))

(provide 'shm)

;;; shm.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

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

(defun shm-node-pp (n)
  "Pretty print the node."
  (format "%s: %S: %d—%d"
          (shm-node-type-name n)
          (shm-node-cons n)
          (shm-node-start n)
          (shm-node-end n)))

(defun shm-node-next (node-pair)
  "Get the next node of NODE-PAIR."
  (let ((vector (shm-decl-ast)))
    (loop for i
          from 0
          to (length vector)
          until (or (= i (length vector))
                    (let ((node (elt vector i)))
                      (>= (shm-node-start node)
                          (shm-node-end (cdr node-pair)))))
          finally (return
                   (when (< i (length vector))
                     (shm-workable-node (cons i
                                              (elt vector i))))))))

(defun shm-node-type (n)
  "Get the AST type of N."
  (elt n 0))

(defun shm-node-type-name (n)
  "Get just the constructor name part of N.

This doesn't always return the correct thing, e.g. [Foo Bar] will
return [Foo. It's just a convenience function to get things like
Case or whatnot"
  (nth 0 (split-string (elt n 0) " ")))

(defun shm-node-parent (node-pair &optional type bound)
  "Return the direct parent of the given node-pair.

The start and end point of the parent can be the same as the
child, and in fact is common."
  (save-excursion
    (goto-char (shm-node-start (cdr node-pair)))
    (let* ((actual-parent-pair (shm-node-backwards (1- (car node-pair))
                                                   type
                                                   bound))
           (maybe-parent-parent-pair (when (car actual-parent-pair)
                                       (shm-node-backwards (1- (car actual-parent-pair)))))
           (actual-parent (cdr actual-parent-pair))
           (maybe-parent-parent (cdr maybe-parent-parent-pair)))
      (cond ((and actual-parent-pair
                  maybe-parent-parent-pair
                  (string= (shm-node-type-name actual-parent)
                           (shm-node-type-name maybe-parent-parent))
                  (and shm-skip-applications
                       (or (eq (shm-node-cons actual-parent) 'App)
                           (eq (shm-node-cons actual-parent) 'InfixApp)
                           (eq (shm-node-cons actual-parent) 'TyApp)))
                  (eq (shm-node-cons actual-parent)
                      (shm-node-cons maybe-parent-parent)))
             (shm-node-parent actual-parent-pair))
            (t actual-parent-pair)))))


(defun shm-node-lambda-p (node)
  (string= (shm-node-cons node) "Lambda"))

(defun shm-import-decl-p (node-cons)
  (string= "ImportDecl" node-cons))

(defun shm-node-cons (n)
  "Get the constructor name of N."
  (elt n 1))

(defun shm-node-start (n)
  "Get the start position of N in its buffer."
  (marker-position (elt n 2)))

(defun shm-node-end (n)
  "Get the end position of N in its buffer."
  (marker-position (elt n 3)))

(defun shm-node-set-start (n x)
  "Set the start position of N."
  (set-marker (elt n 2) x))

(defun shm-node-set-end (n x)
  "Set the end position of N."
  (set-marker (elt n 3) x))

(defun shm-node-start-column (n)
  "Get the starting column of N."
  (save-excursion (goto-char (shm-node-start n))
                  (current-column)))

(defun shm-node-end-column (n)
  "Get the end column of N."
  (save-excursion (goto-char (shm-node-end n))
                  (current-column)))

(defun shm-node-empty (n)
  "Is the node empty of any text?"
  (= (shm-node-start n)
     (shm-node-end n)))

(defun shm-has-parent-with-matching-type-p (node-pair)
 "Does node have a parent with the same type?"
  (let* ((current (cdr node-pair))
         (parent-pair (shm-node-parent node-pair (shm-node-type current)))
         (parent (cdr parent-pair)))
    (if parent
        (if (string= (shm-node-type current)
                     (shm-node-type parent)) t))))

(defun shm-concrete-syntax-for-node (node)
  "Get the text representing this node."
  (buffer-substring-no-properties 
   (shm-node-start (shm-current-node))
   (shm-node-end (shm-current-node))))

(defun shm-node-syntax-contains-regex (regex node)
  "Check the syntax of a node for an occurrence of pattern."
  (let ((node-concrete-syntax (shm-concrete-syntax-for-node node)))
    (string-match-p regex node-concrete-syntax)))

(defun shm-type-signature-with-constraint-p (pair)
  "Is the node a type signiture with a constraint?"
  (let ((current-node (cdr pair)))
    (and (shm-top-level-type-decl-p pair)
         (shm-node-syntax-contains-regex "=>" current-node))))

(defun shm-node-type-info (node)
  "Get the type of the given node."
  (shm-type-of-region (shm-node-start node)
                      (shm-node-end node)))

(defun shm-top-level-type-decl-p (node-pair)
  (let ((current-node (cdr node-pair)))
    (if (and (not (shm-has-parent-with-matching-type-p node-pair))
             (string= "Type" (shm-node-type current-node))) t)))

(defun shm-node-description (node)
  "Generate a description of the given node suitable to be put in
  the minibuffer. If no documentation can be found, it generates
  a reasonable string instead."
  (let* ((type-doc (assoc (shm-node-type-name node)
                          shm-ast-documentation))
         (con-doc (assoc (symbol-name (shm-node-cons node))
                         (cddr type-doc))))
    (if type-doc
        (format "Node type: “%s”: %s, case: %s\n%s"
                (nth 0 type-doc)
                (nth 1 type-doc)
                (if con-doc
                    (format "“%s”: %s"
                            (nth 0 con-doc)
                            (nth 1 con-doc))
                  (format "“%s” (no more info)"
                          (shm-node-cons node)))
                (save-excursion
                  (shm-kill-node 'buffer-substring-no-properties
                                 node
                                 nil
                                 t)))
      (format "Node type: “%s” (no more info)"
              (shm-node-type-name node)))))

(defun shm-node-child-pair (node-pair)
  "Return the immediate child-pair of the given parent."
  (let ((vector (shm-decl-ast))
        (i (car node-pair)))
    (when (< i (1- (length vector)))
      (cons (1+ i)
            (elt vector (1+ i))))))

(defun shm-pvar-p (node)
  (string= (shm-node-cons node) "PVar"))

(defun shm-module-name-p (node-cons)
 "Is the node a module name"
  (string= "ModuleName" node-cons))

(defun shm-node-child (node-pair)
  "Return the immediate child of the given parent."
  (cdr (shm-node-child-pair node-pair)))

(defun shm-get-parent-top-level-decl (node-pair)
  (shm-node-parent node-pair "Decl"))

(defun shm-node-ancestor-at-point (node-pair point)
  "Find the highest up ancestor that still starts at this point."
  (let ((parent-pair (shm-node-parent node-pair)))
    (if parent-pair
        (if (= (shm-node-start (cdr parent-pair))
               point)
            (shm-node-ancestor-at-point parent-pair point)
          node-pair)
      node-pair)))

(defun shm-node-backwards (&optional start type bound)
  "Get the current node searching bottom up starting from START,
and optionally just searching for nodes of type TYPE. BOUND
restricts how far to look back.

This is the fundamental way to look for a node in the declaration
vector.

Backwards means we go from the last node in the list and go
backwards up the list, it doesn't mean backwards as in up the
tree."
  (let* ((vector (shm-decl-ast))
         (point (point)))
    (loop for i
          downfrom (if start
                       (max -1 start)
                     (1- (length vector)))
          to -1
          until (or (= i -1)
                    (let ((node (elt vector i)))
                      (or (and bound
                               (< (shm-node-start node)
                                  bound))
                          (and (>= point (shm-node-start node))
                               (<= point (shm-node-end node))
                               (or (not type)
                                   (string= type
                                            (shm-node-type node)))))))
          finally (return
                   (when (and (>= i 0)
                              (not (and bound
                                        (< (shm-node-start (elt vector i))
                                           bound))))
                     (cons i
                           (elt vector i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Specific node type queries
(defun shm-constraint-has-parens-p (node)  
   (let* ((syntax (shm-concrete-syntax-for-node node))
          (constraint-syntax (car (split-string syntax "=>"))))
     (string-match-p ")" constraint-syntax)))

(defun shm-goto-end-of-constraint (node)
  "Set point to the first white-space character between the end of the type constraint and the '=>'"
  (goto-char (+ (shm-node-start node)
                (shm-node-syntax-contains-regex "=>" node)))
  (re-search-backward "^\\|[^[:space:]]") (goto-char (+ (point) 1)))

(defun add-initial-type-constraint (node)
  (goto-char (shm-node-start node))
  (insert " => ") (backward-char 4))

(defun shm-add-additional-type-constraint (node)
  (if (shm-constraint-has-parens-p node)
      (progn
        (shm-goto-end-of-constraint node)
        (backward-char 1)
        (insert ", "))
    (goto-char (shm-node-start node))
    (insert "(")
    (shm-goto-end-of-constraint node)
    (insert ", )")            
    (backward-char 1)))

;;; Code:
(defun shm-get-refactors (node)
  "Get a vector of possible refactorings for the (CURRENT-NODE)."
  (shm-lint-ast "decl"
                (shm-node-start node)
                (shm-node-end node)))

(provide 'shm-node)

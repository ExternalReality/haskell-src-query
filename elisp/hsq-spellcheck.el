(require 'json)
(require 'cl)

(defface hsq-misspelling
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "SlateGray"))
    (t
     :underline t :inherit warning))
  "hsq face for misspellings."
  :group 'hsq-faces)

(defun hsq/spellcheck ()
  (remove-overlays nil nil 'name 'hsq-misspelling)
  (hsq-haskell-spellcheck-top-level-decl))

(defun hsq/spellsuggestions ()
  (let (current-node (shm-current-node))
    (hsq-spellsuggestions (current-node))))

(defun hsq-spellsuggest-node (node)
  (let* ((misspellings (hsq-spellcheck-top-level-decl))
         (misspelled-names (cl-map 'list 'hsq-name-misspelling misspellings))
         (name (shm-concrete-syntax-for-node node)))
    (when (member name misspelled-names)
      (hsq-spellsuggest (shm-node-start node)
                        (shm-node-end node)))))

(defun hsq-spellcheck-top-level-decl ()
  (let* ((points (shm-decl-points))
         (start (car points))
         (end (cdr points)))
    (hsq-spellcheck start end)))

(defun hsq-haskell-spellcheck-top-level-decl ()
  (let* ((points (shm-decl-points))
         (start (car points))
         (end (cdr points))
         (misspellings (hsq-spellcheck start end)))
    (hsq-underline-misspellings misspellings)))

(defun hsq-spellquery (start end)
  (hsq-spellquery start end "spellcheck"))

(defun hsq-spellsuggest (start end)
  (hsq-spellquery start end "spellsuggest"))

(defun hsq-spellquery (start end query)
  "Spell check the current top level declaration."
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
                                     query)
              ((file-error)
               (error "Unable to find structured-haskell-mode executable! See README for help.")))))
        (json-read-from-string (buffer-string))))))

(defun hsq-underline-misspellings (misspellings)
  (cl-mapc 'hsq-underline-misspelling misspellings))

(defun hsq-underline-misspelling (misspelling)
  (let ((start (hsq-misspelling-start-location misspelling))
        (end (hsq-misspelling-end-location misspelling)))
    (hsq-make-mispelling-overlay start end)))

(defun hsq-make-mispelling-overlay (start end)
  (let ((misspelling-overlay (make-overlay start end)))
    (overlay-put misspelling-overlay 'face 'hsq-misspelling)
    (overlay-put misspelling-overlay 'name 'hsq-misspelling)))

(defun hsq-misspelling-start-location (misspelling)
  "Get the starting position of the (MISSPELLING) relative to its top level declaration."
  (let ((start (car (shm-decl-points)))
        (rsl (hsq-start-line-misspelling misspelling))
        (rsc (hsq-start-column-misspelling misspelling)))
    (save-excursion
      (goto-char start)
      (when (> rsl 0) (forward-line rsl))
      (forward-char rsc)
      (point))))

(defun hsq-misspelling-end-location (misspelling)
  "Get the end position of the (MISSPELLING) relative to its top level declaration."
  (let ((start (car (shm-decl-points)))
        (rel (hsq-end-line-misspelling misspelling))
        (rec (hsq-end-column-misspelling misspelling)))
    (save-excursion
      (goto-char start)
      (when (> rel 0) (forward-line rel))
      (forward-char rec)
      (point))))

(defun hsq-name-misspelling (mispelling)
  (elt mispelling 0))

(defun hsq-start-line-misspelling (misspelling)
  (1- (string-to-number (elt misspelling 1))))

(defun hsq-start-column-misspelling (misspelling)
  (1- (string-to-number (elt misspelling 2))))

(defun hsq-end-line-misspelling (misspelling)
  (1- (string-to-number (elt misspelling 3))))

(defun hsq-end-column-misspelling (misspelling)
  (1- (string-to-number (elt misspelling 4))))

(provide 'hsq-spellcheck)

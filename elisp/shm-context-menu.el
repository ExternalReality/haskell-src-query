;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'shm-node)
(require 'shm-refactor)
(require 'shm-lambda)
(require 'popup)

(defconst qualify-import "qualify import")
(defconst visit-module-definition "visit module definition")
(defconst hlint-suggestion "hlint suggestion")
(defconst create-top-level-function-from-lambda "create top level function from lambda")
(defconst add-type-constraint "add type constraint")

(defun shm/collapse-nested-lambda ()
  (let* ((current-node (shm-current-node))
         (refactors (shm-get-refactors current-node))
         (refactor (shm-find-refactor-by-name refactors "collapse nested lambdas")))
    (when refactor (shm-invoke-hlint-suggestion refactor))))
     
(defun shm/present-actions-for-node ()
  "Display menu of possible actions for node."
  (interactive)
  (let* ((pair (shm-current-node-pair))
         (current (cdr pair))
         (cons (shm-node-cons current))
         (refactors (shm-get-refactors current))
         (menu nil))
    (when (shm-node-lambda-p current)
      (add-to-list 'menu (shm-item-for-lambda)))
    (when (shm-refactors-available-p refactors)
      (setq menu (append menu (shm-items-for-refactors refactors))))
    (when (shm-top-level-type-decl-p pair)
      (add-to-list 'menu (shm-item-for-top-level-type-decl)))
    (when (shm-import-decl-p cons) 
      (add-to-list 'menu (shm-item-for-import-decl)))
    (when (and (shm-module-name-p cons)
               (fboundp (quote haskell-mode-tag-find)))
      (add-to-list 'menu (shm-item-for-module-name)))
    (if menu 
        (progn 
          (cancel-timer shm-parsing-timer)
          (unwind-protect 
              (shm-invoke-action-for-menu-item (popup-menu* menu))
            (setq shm-parsing-timer
                  (run-with-idle-timer shm-idle-timeout t 'shm-reparsing-timer)))))))

;; collapse any nested lambdas
;; compare arg list to variables in defintion
;; turn free variables into application
;; insert application surrounded by parens
;; check parent node for redundant parens


(defun shm/move-lambda-to-top-level ()
  (let* ((function-name (read-from-minibuffer "function name: "))
         (current-node-pair (shm-current-node-pair))
         (current-node (cdr current-node-pair))
         (free-variables (shm-lambda-free-vars current-node))
         (lambda-args (shm-lambda-args current-node))
         (lambda-body (shm-query-lambda-body current-node))
         (current-top-level-node (cdr (shm-get-parent-top-level-decl current-node-pair)))
         (replacement (concat function-name " " free-variables)))
    (shm-replace-node-syntax current-node (chomp replacement))
    (goto-char (shm-node-end current-top-level-node))
    (insert ?\n?\n)
    (insert function-name " " free-variables 
                          " " lambda-args 
                          " = " lambda-body )
    (insert ?\n)))

(defun shm-replace-node-syntax (node replacement-syntax)
  (let ((start (shm-node-start node))
        (end (shm-node-end node)))
    (save-excursion
      (delete-region start end)
      (goto-char start)      
      (insert replacement-syntax))))

(defun shm-get-lambda-args (node)
  (car (shm-split-lambda node)))

(defun shm-get-lambda-body (node)
  (cdr (shm-split-lambda node)))

(defun shm-split-lambda (node)
  (let ((syntax (shm-concrete-syntax-for-node node)))          
    (string-match "\\\\\\(.*?\\)->\\(.*\\)" syntax)
    (let ((lambda-args (match-string 1 syntax))
          (lambda-body (match-string 2 syntax)))
      (cons (chomp lambda-args) (chomp lambda-body)))))

(defun shm-items-for-refactors (refactors)
  "Create a popup menu items from (REFACTORS)."
  (mapcar 'shm-item-for-refactor refactors))

(defun shm-invoke-hlint-suggestion (refactor)
  "Replace the current node with the suggestion from the (REFACTOR)."
  (let* ((current-node (shm-current-node))
         (start (shm-refactor-start current-node refactor))
         (end (shm-refactor-end current-node refactor)))
    (save-excursion
      (delete-region start end)
      (goto-char start)      
      (insert (elt refactor 3)))))

(defun shm-start-refactor-line (refactor)
  "Get the starting line of (REFACTOR) relative to the context in which it was found."
  (elt refactor 4))

(defun shm-item-for-refactor (refactor)
  "Create the menu item for a particular (REFACTOR)."
  (popup-make-item (concat "⚒ " (refactor-name refactor)) :value (cons hlint-suggestion refactor)))

(defun shm-item-for-import-decl ()
  (popup-make-item "✎ qualify import" :value qualify-import))

(defun shm-item-for-module-name ()
  (popup-make-item "✈ visit module" :value visit-module-definition))

(defun shm-item-for-top-level-type-decl ()
  (popup-make-item "✎ add type constraint" :value add-type-constraint))

(defun shm-item-for-lambda ()
  (popup-make-item (concat "⚒ " "create top-level function from lambda") :value create-top-level-function-from-lambda))

(defun shm-invoke-action-for-menu-item (item-value)
  "Invoke function on (ITEM-VALUE) chosen from the context menu."
  (cond ((selected-item-value-p item-value qualify-import) (invoke-with-suggestion 'shm/qualify-import))
        ((selected-item-value-p item-value visit-module-definition) (invoke-with-suggestion 'haskell-mode-tag-find))
        ((selected-item-value-p item-value hlint-suggestion) (invoke-with-suggestion 'shm-invoke-hlint-suggestion (cdr item-value)))
        ((selected-item-value-p item-value create-top-level-function-from-lambda) (invoke-with-suggestion 'shm/move-lambda-to-top-level))
        ((selected-item-value-p item-value add-type-constraint) (invoke-with-suggestion 'shm/modify-type-constraint))))

(defun selected-item-value-p (value match)
  ;;Basically check to see if the value selected by the menu matches a given string
  "Extract String from (VALUE) and check for string equality against (MATCH)."
  (or (and (stringp value) (string= value match))
      (and (listp value) (string= (car value) match))))

(defun invoke-with-suggestion (function &optional arg)
  "Invoke (FUNCTION) with on (ARG) and show its key binding in mini buffer if it has one."
  (if arg (funcall function arg) (funcall function))
  (let ((binding (where-is-internal function shm-map t)))
    (when binding
      (with-temp-message
          (format "You can run the command `%s' with %s"
                  function (key-description binding))
        (sit-for (if (numberp suggest-key-bindings)
                     suggest-key-bindings
                   2))))))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(provide 'shm-context-menu)

;;; shm-context-menu.el ends here

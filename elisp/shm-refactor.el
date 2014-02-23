;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'shm-node)

(defun shm-find-refactor-by-name (refactors name)
  (setq refactor nil)
  (setq num 0)
  (while (and (< num (length refactors))
              (not (string= (refactor-name refactor) name)))
    (setq refactor (elt refactors num))
    (setq num (1+ num)))
  (if (string= (refactor-name refactor) name) refactor nil))


(defun shm-refactor-start (current-node refactor)
  "Get the starting position of the (REFACTOR) relative to the currently selected node."
  (let ((start (shm-node-start current-node))
        (rsl (shm-start-line-refactor refactor))                
        (rsc (shm-start-column-refactor refactor)))
    (save-excursion
      (goto-char start)
      (when (> rsl 0) (forward-line rsl))
      (forward-char rsc)
      (point))))

(defun shm-refactor-end (current-node refactor)
  "Get the end position of the (REFACTOR) relative to the currently selected node."
  (let ((start (shm-node-start current-node))
        (rel (shm-end-line-refactor refactor))                
        (rec (shm-end-column-refactor refactor)))
    (save-excursion
      (goto-char start)
      (when (> rel 0) (forward-line rel))
      (forward-char rec)
      (point))))

(defun shm-refactors-available-p (refactors)
  "Check to see if the (REFACTORS) vector is populated."
  (if (> (length refactors) 0) t))

(defun refactor-name (refactor)
  "Get the name of (REFACTOR)."
  (elt refactor 1))

(defun shm-start-line-refactor (refactor)
  "Get the starting line of the (REFACTOR) which is relative to the context in which it was found."
  (- (string-to-number (elt refactor 4)) 1))

(defun shm-start-column-refactor (refactor)
  "Get the starting column of the (REFACTOR) which is relative to the context in which it was found."
  (- (string-to-number (elt refactor 5)) 1))

(defun shm-end-line-refactor (refactor)
  "Get the end line of the (REFACTOR) which is relative to the context in which it was found."
  (- (string-to-number (elt refactor 6)) 1))

(defun shm-end-column-refactor (refactor)
  "Get the end column of the (REFACTOR) which is relative to the context in which it was found."
  (- (string-to-number (elt refactor 7)) 1))


(provide 'shm-refactor)

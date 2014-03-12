;;; package --- Summary

;;; Commentary:

(require 'ido)
(require 'json)

(defun haskell-src-query-build-targets ()
  (let ((message-log-max nil)
        (buffer (current-buffer))
        (cabal-file (haskell-cabal-find-file)))
    (with-temp-buffer
      (let ((temp-buffer (current-buffer)))
	(with-current-buffer buffer
	  (condition-case e
	      (call-process shm-program-name
			    nil
			    temp-buffer
			    nil
			    "targets"
			    "Emacs"
			    (format "--cabal-file=%s" cabal-file))
	    ((file-error)
	     (error "cannot find haskell-src-query executable")))))
      (json-read-from-string (buffer-string)))))


 (defun haskell-session-select-build-target()
  (interactive)
   (haskell-session-set-target (haskell-session)
    (ido-completing-read "Build Target:" 
			 (append (haskell-src-query-build-targets) nil))))


(provide 'hsq-cabal)

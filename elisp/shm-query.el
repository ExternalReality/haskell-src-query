;;; package --- Summary

;;; Commentary:
(require 'shm-node)

(defconst query-program-name "haskell-src-query")

(defun haskell-src-query (query node &optional filePath)
  (let ((message-log-max nil)
        (end (shm-node-end node))
        (start (shm-node-start node))
        (buffer (current-buffer))
        (cabal-file (haskell-cabal-find-file))
        (target  (haskell-session-target (haskell-session)))
        (srcPath (if filePath filePath "")))
    (when (> end (1+ start))
      (with-temp-buffer
        (let ((temp-buffer (current-buffer)))
          (with-current-buffer buffer
            (condition-case e
                (call-process-region start end
                                     query-program-name
                                     nil
                                     temp-buffer
                                     nil
                                     query
                                     "Emacs"
                                     (format "--source-file=%s" srcPath)
                                     (format "--package-conf=%s" shm-packages-config-directory)
                                     (format "--cabal-file=%s" cabal-file)
                                     (format "--build-target=%s" target))
              ((file-error)
               (error "cannot find haskell-src-query executable")))))
        (read (buffer-string))))))

(defmacro shm-query (name query)
  `(defun ,name (node &optional filePath)
     (haskell-src-query ,query node filePath)))

(provide 'shm-query)

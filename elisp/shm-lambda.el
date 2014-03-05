;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'shm-node)
(require 'shm-query)

(shm-query shm-query-lambda-args "lambdaArgs")
(shm-query shm-query-lambda-body "lambdaBody")
(shm-query shm-query-lambda-free-vars "freeVariables")

(defun shm-lambda-args (node)
  (shm-query-lambda-args node))

(defun shm-lambda-free-vars (node)
  (vector-to-string (shm-query-lambda-free-vars
                     node
                     (buffer-file-name (current-buffer)))))
                 
(defun vector-to-string (vec)
  (mapconcat 'identity vec " "))
                     
(provide 'shm-lambda)


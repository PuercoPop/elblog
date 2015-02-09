;; -*- lexical-binding: t -*-

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'htmlize)
(require 'elnode)

(defgroup elblog nil
  "Turn Emacs into a blog plataform, literally."
  :group 'comm)

(defvar elblog-host "*")

(defvar elblog-port 8080
  "The port for published buffers to be served on.")

(defvar elblog-post-directory nil
  "The directory where to look for posts.")

(defvar elblog-post-regexp
  (rx (and bol
           (1+ anything)
           ".org"))
  "Is file a post? By default it matches org files.")

(defvar elblog-routes
  '(("^/posts/.*/$" . elblog-post-handler)
    ("^/$" . elblog-index)))

(defvar elblog--memoized-posts nil
  "An plist containing the buffers already htmlized. In the form
  of '((buffer-name . buffer)).")

(defun elblog-index (httpcon)
  "List all the published buffers.
Argument HTTPCON http connection."
  (elnode-send-html httpcon
                    (concat "<!DOCTYPE html><html><head><title>Elblog index</title></head><body><ul>"
                            (mapconcat
                             (lambda (pair)
                               (format "<li><a href='/posts/%s/'>%s</a></li>"
                                       (car pair)
                                       (car pair)))
                             elblog-published-buffers
                             "\n")
                            "</ul></body></html>")))

(defun elblog-post-handler (httpcon)
  "Render the HTMLized buffer."
  (let* ((path (elnode-http-pathinfo httpcon))
         (buffer-name (caddr (split-string path "/"))))
    (message "Buffer name: %s" buffer-name)
    (elnode-send-html httpcon
                      (with-current-buffer
                          (htmlize-buffer (get-buffer buffer-name))
                        (buffer-string)))))

(defun elblog-root (httpcon)
  (elnode-dispatcher httpcon elblog-routes))

(defun elblog-start ()
  "Start listening for requests for published buffers."
  (interactive)
  (when (y-or-n-p "Start publishing buffers? ")
    (elnode-start 'elblog-root :port elblog-port :host elblog-host)))

(defun elblog-stop ()
  "Stop listening for requests for published buffers."
  (interactive)
  (when (y-or-n-p "Stop publishing buffers? ")
    (elnode-stop elblog-port)))

(provide 'elblog)
;;; elblog.el ends here

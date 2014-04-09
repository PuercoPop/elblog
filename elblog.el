;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'htmlize)
(require 'elnode)
(require 'elblog-helpers)

(defgroup elblog nil
  "Turn Emacs into a blog plataform, literally."
  :group 'comm)

(defvar elblog-host "*")

(defvar elblog-port 8080
  "The port for published buffers to be served on.")

(defvar elblog-published-buffers
  (list (cons (buffer-name (current-buffer)) (current-buffer)))
  "An plist of buffers to publish. In the form of '((buffer-name . buffer))")

(defvar elblog-routes
  '(("^/posts/.*/$" . elblog-post-handler)
    ("^/$" . elblog-index)))

(defun elblog-publish-buffer (buffer)
  "Publish a buffer through Elnode."
  (interactive "bBuffer name: ")
  (setq elblog-published-buffers
        (append elblog-published-buffers
                (with-current-buffer buffer
                  (list  (cons (buffer-name (current-buffer))
                               (current-buffer)))))))

(defun elblog-unpublish-buffer (buffer)
  "Unpublish a buffer through Elnode."
  (interactive "bBuffer name: ")
  (assoc-delete-all buffer elblog-published-buffers))

(assq "elblog.el" elblog-published-buffers)


(defun elblog-index (httpcon)
  "List all the published buffers."
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
  (let* ((path (elnode-http-pathinfo httpcon))
         (buffer-name (caddr (split-string path "/")))
         (pair (assoc buffer-name elblog-published-buffers)))

    (elnode-send-html httpcon
                      (with-current-buffer
                          (htmlize-buffer (cdr pair))
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
    (elnode-stop mak/elnode-publish-port)))

(provide 'elblog)

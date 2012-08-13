(require 'simple-httpd)

;; serve up the shim that will do the long polling
(defvar imp-shim-root "~/.emacs.d/imp/")
(defvar imp-current-buffer nil)

(defun httpd/imp-shim (proc path &rest args)
  (let* ((file (file-name-nondirectory path))
         (clean (expand-file-name file imp-shim-root)))
    (httpd-send-file proc clean)))

(defservlet imp text/html (path)
  (if imp-current-buffer
      (insert-buffer imp-current-buffer)
    (insert "run imp-set-current-buffer with the buffer you want to monitor")))

(defun imp-set-current-buffer (buffer)
  (interactive "bbuffer:")
  (setq imp-current-buffer buffer))

(provide 'imp)

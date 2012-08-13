(require 'simple-httpd)

(defvar imp-shim-root (file-name-directory (locate-library "imp")))
(defvar imp-current-buffer nil)

(defun httpd/imp-shim (proc path &rest args)
  (let* ((file (file-name-nondirectory path))
         (clean (expand-file-name file imp-shim-root))
         (index (expand-file-name "index.html" imp-shim-root)))
    (cond
     ((file-directory-p clean) (httpd-send-file proc index))
     (t (httpd-send-file proc clean)))))

(defservlet imp text/html (path)
  (if imp-current-buffer
      (insert-buffer imp-current-buffer)
    (insert "run imp-set-current-buffer with the buffer you want to monitor")))

(defun imp-set-current-buffer (buffer)
  "sets BUFFER to be the buffer watched for changes by imp"
  (interactive "bbuffer:")
  (setq imp-current-buffer buffer))

(provide 'imp)

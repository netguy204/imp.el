;;; impatient-mode.el --- Serve buffers live over HTTP

;; This is free and unencumbered software released into the public domain.

;; Author: Brian Taylor <el.wubo@gmail.com>
;; Version: 0.1
;; URL: https://github.com/netguy204/imp.el
;; Package-Requires: ((simple-httpd "1.1") (htmlize "1.40"))

;;; Commentary:

;; impatient-mode is a minor mode that publishes the live buffer
;; through the local simple-httpd server under /imp/<buffer-name>. To
;; unpublish a buffer, toggle impatient-mode off.

;; Start the simple-httpd server (`httpd-start') and visit /imp/ on
;; the local server. There will be a listing of all the buffers that
;; currently have impatient-mode enabled. This is likely to be found
;; here:

;;   http://localhost:8080/imp/

;; Except for html-mode buffers, buffers will be prettied up with
;; htmlize before being sent to clients. This can be toggled at any
;; time with `imp-toggle-htmlize'.

;; Because html-mode buffers are sent raw, you can use impatient-mode
;; see your edits to an HTML document live! This is perhaps the
;; primary motivation of this mode.

;; To receive updates the browser issues a "long-pull" on the client
;; waiting for the buffer to change, so no polling is required. The
;; response happens in an `after-change-functions' hook. Buffers that
;; do not run these hooks will not be displayed live to clients.

;;; Code:

(require 'url-util)
(require 'simple-httpd)
(require 'htmlize)

(defgroup impatient-mode nil
  "Serve buffers live over HTTP."
  :group 'comm)

(defvar impatient-mode-map (make-sparse-keymap)
  "Keymap for impatient-mode.")

(defvar imp-htmlize-filter t
  "If true, htmlize this buffer before serving.")

(defvar imp-client-list ()
  "List of client processes watching the current buffer.")

(defvar imp-last-state 0
  "State sequence number.")

(define-minor-mode impatient-mode
  "Serves the buffer live over HTTP."
  :group 'impatient-mode
  :lighter " imp"
  :keymap impatient-mode-map
  (make-local-variable 'imp-htmlize-filter)
  (make-local-variable 'imp-client-list)
  (make-local-variable 'imp-last-state)
  (setq imp-htmlize-filter (not (eq major-mode 'html-mode)))
  (if impatient-mode
      (add-hook 'after-change-functions 'imp--on-change)
    (remove-hook 'after-change-functions 'imp--on-change)))

(defvar imp-shim-root (file-name-directory load-file-name)
  "Location of data files needed by impatient-mode.")

(defun imp-toggle-htmlize ()
  "Toggle htmlization of this buffer before sending to clients."
  (interactive)
  (setq imp-htmlize-filter (not imp-htmlize-filter)))

(defun imp-buffer-enabled-p (buffer)
  "Return t if buffer has impatient-mode enabled."
  (and buffer (with-current-buffer (get-buffer buffer) impatient-mode)))

(defun httpd/imp/static (proc path &rest args)
  "Serve up static files."
  (let* ((file (file-name-nondirectory path))
         (clean (expand-file-name file imp-shim-root)))
    (if (file-exists-p clean)
        (httpd-send-file proc clean)
      (httpd-error proc 404))))

(defun imp-serve-buffer-list (proc)
  "Serve a list of published buffers."
  (with-httpd-buffer proc "text/html"
    (insert "<html><head>\n")
    (insert "<title>impatient-mode buffer list</title>\n")
    (insert "</head><body>\n")
    (insert "<h1>Public Buffers</h1>\n<hr/>")
    (insert "<ul>\n")
    (dolist (buffer (buffer-list))
      (when (imp-buffer-enabled-p buffer)
        (insert (format "<li><a href=\"%s\">%s</a></li>\n"
                        (url-hexify-string (buffer-name buffer))
                        (url-insert-entities-in-string (buffer-name buffer))))))
    (insert "</ul>\n<hr/>")
    (insert "Enable <code>impatient-mode</code> in buffers to publish them.")
    (insert "</body></html>")))

(defun httpd/imp (proc path &rest args)
  "Serve up the main buffer access page."
  (let* ((index (expand-file-name "index.html" imp-shim-root))
         (buffer-name (file-name-nondirectory path))
         (buffer (get-buffer buffer-name)))
    (if (equal (directory-file-name path) "/imp")
        (imp-serve-buffer-list proc)
      (if (imp-buffer-enabled-p buffer)
          (httpd-send-file proc index)
        (httpd-error proc 403 "Buffer is private or doesn't exist.")))))

(defun imp--send-state (proc)
  (let ((id (number-to-string imp-last-state))
        (htmlize imp-htmlize-filter)
        (buffer (current-buffer)))
    (with-temp-buffer
      (insert id " ")
      (if htmlize
          (let ((pretty-buffer (htmlize-buffer buffer)))
            (insert-buffer-substring pretty-buffer)
            (kill-buffer pretty-buffer))
        (insert-buffer-substring buffer))
      (httpd-send-header proc "text/plain" 200 '("Cache-Control" "no-cache"))
      (httpd-send-buffer proc (current-buffer)))))

(defun imp--send-state-ignore-errors (proc)
  (condition-case error-case
      (imp--send-state proc)
    (error nil)))

(defun imp--on-change (&rest args)
  "Hook for after-change-functions."
  (incf imp-last-state)
  (while imp-client-list
    (imp--send-state-ignore-errors (pop imp-client-list))))

(defun httpd/imp/buffer (proc path query &rest args)
  "Servlet that accepts long-pull requests."
  (let* ((buffer (get-buffer (file-name-nondirectory path)))
         (req-last-id (string-to-number (or (cadr (assoc "id" query)) "0"))))
    (if (imp-buffer-enabled-p buffer)
        (with-current-buffer buffer
          (if (equal req-last-id imp-last-state)
              (push proc imp-client-list)         ; this client is sync'd
            (imp--send-state-ignore-errors proc))) ; this client is behind
      (httpd-error proc 403 "Buffer is private or doesn't exist."))))

(provide 'impatient-mode)

;;; impatient-mode.el ends here

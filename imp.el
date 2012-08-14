(require 'simple-httpd)
(require 'htmlize)

(defvar imp-shim-root (file-name-directory (locate-library "imp")))
(defvar imp-current-buffer nil)
(defvar imp-htmlize-filter nil)
(defvar imp-client-list nil)
(defvar imp-last-state 0)

(defun httpd/imp-shim (proc path &rest args)
  (let* ((file (file-name-nondirectory path))
         (clean (expand-file-name file imp-shim-root))
         (index (expand-file-name "index.html" imp-shim-root))
         (buffer-name (when imp-current-buffer 
                        (buffer-file-name (get-buffer imp-current-buffer))))
         (buffer-dir (when buffer-name
                       (file-name-directory buffer-name)))
         (buffer-clean (when buffer-dir
                         (expand-file-name file buffer-dir))))
    (cond
     ((file-directory-p clean) (httpd-send-file proc index))
     ((file-exists-p clean) (httpd-send-file proc clean))

     ;; try to serve from the buffer directory if we could determine that
     (buffer-clean (httpd-send-file proc buffer-clean))
     
     ;; no luck. it's an error
     (t (httpd-error proc 403)))))

(defun imp--send-state (proc)
  (with-temp-buffer
    (insert (number-to-string imp-last-state))
    (insert " ")

    ;; render the real contents
    (cond
     ((and imp-current-buffer imp-htmlize-filter)
      (let ((pretty-buffer (htmlize-buffer imp-current-buffer)))
        (insert-buffer-substring pretty-buffer)
        (kill-buffer pretty-buffer)))

     (imp-current-buffer (insert-buffer-substring imp-current-buffer))
   
     (t
      (insert "run imp-set-current-buffer with the buffer you want to monitor")))

    (httpd-send-header proc "text/plain" 200)
    (httpd-send-buffer proc (current-buffer))))

(defun imp--send-state-ignore-errors (proc)
  (condition-case error-case
      (imp--send-state proc)
    (error nil)))

(defun imp--on-change (&rest args)
  (incf imp-last-state)
  (while imp-client-list
    (imp--send-state-ignore-errors (pop imp-client-list))))

(defun httpd/imp (proc path query &rest args)
  (let* ((req-last-id (string-to-number (cadr (assoc "id" query)))))
    (if (equal req-last-id imp-last-state)
        ;; this client is sync'd up, store in waitlist
        (push proc imp-client-list)
      ;; this client is behind, respond immediately
      (imp--send-state-ignore-errors proc))))

(defun imp--move-active-buffer (buffer)
  (when imp-current-buffer
    (condition-case error-case
        (with-current-buffer imp-current-buffer
          (remove-hook 'after-change-functions 'imp--on-change t))
      (error nil)))

  (with-current-buffer buffer
    (add-hook 'after-change-functions 'imp--on-change nil t))

  ;; wake up any listeners
  (setq imp-current-buffer buffer)
  (imp--on-change))

(defun imp-set-current-buffer (buffer)
  "sets BUFFER to be the buffer watched for changes by imp"
  (interactive "bbuffer:")
  (imp--move-active-buffer buffer)
  (setq imp-htmlize-filter nil))

(defun imp-set-current-buffer-htmlize (buffer)
  "sets BUFFER to be the buffer watched for changes by imp"
  (interactive "bbuffer:")
  (imp--move-active-buffer buffer)
  (setq imp-htmlize-filter t))

(provide 'imp)

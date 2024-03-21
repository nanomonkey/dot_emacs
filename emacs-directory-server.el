(defvar my/file-server nil "Is the file server running? Holds an instance if so.")

(defalias 'my/send-directory-list 'ws-send-directory-list)

(defun my/ws-start (handlers port &optional log-buffer &rest network-args)
  "Like `ws-start', but unbroken for Emacs 25+."
  (let ((server (make-instance 'ws-server :handlers handlers :port port))
        (log (when log-buffer (get-buffer-create log-buffer))))
    (setf (process server)
          (apply
           #'make-network-process
           :name "ws-server"
           :service (port server)
           :filter 'ws-filter
           :server t
           :nowait nil
           :family 'ipv4
           :coding 'no-conversion
           :plist (append (list :server server)
                          (when log (list :log-buffer log)))
           :log (when log
                  (lambda (proc request message)
                    (let ((c (process-contact request))
                          (buf (plist-get (process-plist proc) :log-buffer)))
                      (with-current-buffer buf
                        (goto-char (point-max))
                        (insert (format "%s\t%s\t%s\t%s"
                                        (format-time-string ws-log-time-format)
                                        (first c) (second c) message))))))
           network-args))
    (push server ws-servers)
    server))

(defun my/serve-directory (directory port)
  (interactive "DDirectory: \nnPort: ")
  ;; Based on http://eschulte.github.io/emacs-web-server/File-Server.html#File-Server.
  (if my/file-server
      (message "File server is already running!")
    (progn
      (setf my/file-server
            (lexical-let ((docroot directory))
              (ws-start
               (lambda (request)
                 (with-slots (process headers) request
                   (let* ((path (substring (cdr (assoc :GET headers)) 1))
                          (expanded (ws-in-directory-p docroot path)))
                     (if (and expanded
                              (file-exists-p expanded))
                         (if (file-directory-p expanded)
                             (my/send-directory-list process expanded)
                           (ws-send-file process (expand-file-name path docroot)))
                       (ws-send-404 process)))))
               port
               nil                      ;no log buffer
               :host "0.0.0.0")))
      (message "Serving directory %s on port %d" directory port))))

(defun my/stop-server ()
  "Stop the file server if running."
  (interactive)
  (if my/file-server
      (progn
        (ws-stop my/file-server)
        (setf my/file-server nil)
        (message "Stopped the file server."))
    (message "No file server is running.")))

(defun my/send-directory-list (proc directory &optional match)
  "Send a listing of files in DIRECTORY to PROC.
Optional argument MATCH is passed to `directory-files' and may be
used to limit the files sent."
  (ws-response-header proc 200 (cons "Content-type" "text/html"))
  (process-send-string proc
                       (concat
                        ;; header
                        "<!DOCTYPE html>\n"
                        "<html><head><title>Directory listing for "
                        directory
                        "</title></head><body>"
                        "<h2>Directory listing for <tt>"
                        directory
                        "</tt></h2>"
                        "<table>"
                        ;; "Up one level" link
                        "<tr><td></td><td></td><td><a href='../'>Up one level</a></td></tr>"
                        ;; Actual directory listing
                        (mapconcat (lambda (f-and-attr)
                                     (let* ((name (first f-and-attr))
                                            (attr (rest f-and-attr))
                                            (full (expand-file-name name directory))
                                            (end (if (file-directory-p full) "/" ""))
                                            (url (url-encode-url (concat name end)))
                                            (modtime (format-time-string "%Y-%m-%d %T %z" (file-attribute-modification-time attr)))
                                            (size (file-attribute-size attr)))
                                       (format "<tr><td><small>%s</small></td><td><small>%s</small></td><td><a href='%s'>%s%s</a></td></tr>" modtime size url name end)))
                                   (remove-if (lambda (entry)
                                                (or (string= (car entry) ".")
                                                    (string= (car entry) "..")))
                                              (directory-files-and-attributes directory nil match))
                                   "\n")
                        ;; Footer
                        "</table></body></html>")))

(defun my/serve-this (port)
  "Start a file server on a `PORT', serving the content of directory
associated with the current buffer's file."
  (interactive "nPort: ")
  (my/serve-directory (if (buffer-file-name)
                          (file-name-directory (buffer-file-name))
                        (expand-file-name default-directory))
                      port))

;; Eshell utilities.
(defconst my/default-directory-server-port 8123)

(defun eshell/serve-this (&optional port)
  (my/serve-this (or port my/default-directory-server-port)))

(defalias 'eshell/stop-server 'my/stop-server)

(autoload 'htmlfontify-buffer "htmlfontify" nil t)

(defun print-to-html ()
  (interactive)
  (unless (executable-find "xdg-open")
    (error "`print-to-html' requires that you install `xdg-utils' "))
  (if (and mark-active
	   transient-mark-mode)
    (region-to-html (point)
		    (mark))
    (region-to-html)))

;; WAIT: Fiks at andre prosesser (eg firefox) lukkes når emacs-daemon
;; lukkes når startet fra emacs
(defun region-to-html (&optional beg end)
  (let ((fname (concat "/tmp/" (buffer-name) ".html"))
	(buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf beg end)
      (htmlfontify-buffer)
      (write-file fname)
      ;; (shell-command (concat "xdg-open " fname))
      (browse-url fname)
      (kill-buffer (buffer-name))
      (kill-buffer "*Shell Command Output*"))))



(defun print-to-pdf ()
  (interactive)
  (unless (executable-find "xdg-open")
    (error "`print-to-pdf' requires that you install `xdg-utils' "))
  (unless (executable-find "ps2pdf14")
    (error "`print-to-pdf' requires that you install `ghostscript' "))
  (if (and mark-active
	   transient-mark-mode)
    (ps-spool-region-with-faces (point)
				(mark))
    (ps-spool-buffer-with-faces))
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (shell-command (concat "ps2pdf14 /tmp/tmp.ps " "/tmp/" (buffer-name) ".pdf"))
  (shell-command "rm /tmp/tmp.ps")
  (shell-command (concat "xdg-open " "/tmp/" (buffer-name) ".pdf"))
  (kill-buffer "*Shell Command Output*"))

;; TODO: Menynavn: Print to HTML og Print to PDF
;; (define-key-after global-map [menu-bar file print-buffer]
  ;; (cons "Remove tabs" '("Remove tabs" . untabify-buffer))'Indent)

;; (define-key-after menu-bar-file-menu [delete-current-file]
  ;; '(menu-item "Delete..." delete-current-file
              ;; ;; :enable (menu-bar-non-minibuffer-window-p)
	      ;; :keys "Ctrl+D"))

(define-key-after menu-bar-file-menu [new-directory]
  '(menu-item "New Directory..." menu-bar-create-directory
	      :enable (or (not (fboundp 'menu-bar-non-minibuffer-window-p))
			  (menu-bar-non-minibuffer-window-p))
	      :keys "Ctrl+D"
	      :help "Create a directory")
  'new-file)

(define-key menu-bar-file-menu [ps-print-buffer-faces] nil)
(define-key menu-bar-file-menu [print-region] nil)
(define-key menu-bar-file-menu [ps-print-region-faces] nil)
(define-key menu-bar-file-menu [ps-print-buffer] nil)
(define-key menu-bar-file-menu [ps-print-region] nil)
(define-key menu-bar-file-menu [print-buffer] nil)

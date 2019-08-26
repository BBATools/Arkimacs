;;; +modeline.el -*- lexical-binding: t; -*-

(require 'advice)

(defface feebleline-time-face '((t :inherit 'font-lock-comment-face))
  "Feebleline timestamp face."
  :group 'feebleline)
(defface feebleline-linum-face '((t :inherit 'default))
  "Feebleline linum face."
  :group 'feebleline)
(defface feebleline-bufname-face '((t :inherit 'font-lock-function-name-face))
  "Feebleline filename face."
  :group 'feebleline)
(defface feebleline-asterisk-face '((t :foreground "salmon"))
  "Feebleline file modified asterisk face."
  :group 'feebleline)
(defface feebleline-previous-buffer-face '((t :foreground "#7e7e7e"))
  "Feebleline filename face."
  :group 'feebleline)
(defface feebleline-dir-face '((t :inherit 'font-lock-variable-name-face))
  "Feebleline filename face."
  :group 'feebleline)

;; Customizations
(defcustom feebleline-show-time nil
  "Set this if you want to show the time in the modeline proxy."
  :group 'feebleline)
(defcustom feebleline-show-previous-buffer nil
  "Set this if you want to show the previous 'buffer-name' in the modeline proxy."
  :group 'feebleline)
(defcustom feebleline-show-directory t
  "Set this if you want to show the direcory path as well as the file-name in the modeline proxy."
  :group 'feebleline)
(defcustom feebleline-show-linenum t
  "Set this if you want to show line number and column number in the modeline proxy."
  :group 'feebleline)

(defun feebleline-previous-buffer-name ()
  "Get name of previous buffer."
  (buffer-name (other-buffer (current-buffer) 1)))

(defvar feebleline-mode-line-text nil
  "Each element is a list with the following format:

    (FORMAT-STRING FORMAT-ARGS PROPS)

FORMAT-STRING will be used as the first argument to `format', and
FORMAT-ARGS (a list) will be expanded as the rest of `format'
arguments.  If PROPS is given, it should be a list which will be
sent to `add-text-properties'.")

(defvar feebleline--home-dir nil)

(setq
 feebleline-mode-line-text
 '(
   ("%s" ((if feebleline-show-time (format-time-string "[%H:%M:%S] ") ""))
    (face feebleline-time-face))
   ("%s"
    ((if feebleline-show-linenum
         (format "%5s:%-2s" (format-mode-line "%l") (current-column))
       ""))
    (face feebleline-linum-face))
   (" %s" ((if (and feebleline-show-directory (buffer-file-name))
               (replace-regexp-in-string
                feebleline--home-dir "~"
                (file-name-directory (buffer-file-name)))
             ""))
    (face feebleline-dir-face))
   ("%s" ((if (buffer-file-name) (file-name-nondirectory (buffer-file-name))
            (buffer-name)))
    (face feebleline-bufname-face))
   ("%s" ((if (and (buffer-file-name) (buffer-modified-p)) "*"
            "" ))
    (face feebleline-asterisk-face))
   ("%s" ((if feebleline-show-previous-buffer (concat " | " (feebleline-previous-buffer-name))
            ""))
   (face feebleline-previous-buffer-face)))
 )


(defun feebleline-default-settings-on ()
  "Some default settings that works well with feebleline."
  (setq window-divider-default-bottom-width 1
        window-divider-default-places (quote bottom-only))
  (window-divider-mode t)
  (setq-default mode-line-format nil)
  (setq mode-line-format nil))

(defvar feebleline/timer)
(defvar feebleline/mode-line-format-previous)

;;;###autoload
(define-minor-mode feebleline-mode
  "Replace modeline with a slimmer proxy."
  :require 'feebleline
  :global t
  (if feebleline-mode
      ;; Activation:
      (progn
        (setq feebleline--home-dir (expand-file-name "~"))
        (setq feebleline/mode-line-format-previous mode-line-format)
        (setq feebleline/timer
              (run-with-timer 0 0.5 'feebleline-mode-line-proxy-fn))
        ;; (ad-activate 'handle-switch-frame)
        (add-hook 'focus-in-hook 'feebleline-mode-line-proxy-fn))

    ;; Deactivation:
    (set-face-attribute 'mode-line nil :height 1.0)
    (setq-default mode-line-format feebleline/mode-line-format-previous)
    (setq mode-line-format feebleline/mode-line-format-previous)
    (cancel-timer feebleline/timer)
    ;; (ad-deactivate 'handle-switch-frame)
    (remove-hook 'focus-in-hook 'feebleline-mode-line-proxy-fn)
    (force-mode-line-update)
    (redraw-display)
    (with-current-buffer " *Minibuf-0*"
      (erase-buffer))))

(defun feebleline--mode-line-part (part)
  "Return a PART (an element) of `feebleline-mode-line-text` as a propertized string."
  (let ((text (apply #'format (append (list (car part))
                                      (mapcar #'eval (cadr part)))))
        (props (elt part 2)))
    (when props
      (add-text-properties 0 (length text) props text))
    text))

(defvar feebleline-placeholder)
(defun feebleline-write-buffer-name-maybe ()
  "Replace echo-area message with mode-line proxy."
  (progn (setq feebleline-placeholder (mapconcat #'feebleline--mode-line-part
                                                 feebleline-mode-line-text ""))
         (with-current-buffer " *Minibuf-0*"
           (erase-buffer)
           (insert feebleline-placeholder))))

(defun feebleline-mode-line-proxy-fn ()
  "Put a mode-line proxy in the echo area *if* echo area is empty."
  (unless (current-message)
    (feebleline-write-buffer-name-maybe)))

(provide '+modeline)
;;; +modeline.el ends here

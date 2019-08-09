;;; ark-ui.el --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(use-package emacs
  :custom
  (cursor-in-non-selected-windows nil)
  (custom-unlispify-names nil)
  (custom-unlispify-menu-entries nil)
  (truncate-lines t)
  (indicate-empty-lines nil)
  (auto-window-vscroll nil)
  (save-silently t)
  :config
  (blink-cursor-mode -1)
  (add-to-list 'default-frame-alist '(font . "Ubuntu Mono-12"))
  (defalias 'yes-or-no-p 'ark-yes-or-no-p)
  (defalias 'y-or-n-p 'ark-yes-or-no-p)

  (defvar ark-theme 'doom-spacegrey)
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme ark-theme t)))
    (load-theme ark-theme t))

  (defun ark-yes-or-no-p (prompt)
    "Ask user a yes-or-no question using ivy."
    (let* ((yes-or-no-prompt (concat prompt " "))
           (choices '("yes" "no"))
           (answer (ivy-completing-read yes-or-no-prompt choices nil 'require-match)))
      (string= answer "yes")))
  )

(use-package doom-themes
  :ensure t)

(use-package smart-mode-line
  :ensure t
  ;; :disabled t
  :init (sml/setup)
  :config (setq sml/theme 'respectful)
  )

(use-package mini-modeline
  :straight (:host github :repo "kiennq/emacs-mini-modeline" :files ("*.el"))
  ;; :disabled t
  ;; :after mood-line
  :after smart-mode-line
  :config
  (mini-modeline-mode t))

;; (use-package mood-line
;;   :ensure t
;;   :disabled t
;;   :init (mood-line-mode)
;;   :config
;;   (setq-default mode-line-format
;;                 (mood-line-format
;;                  '((:eval
;;                     ;; Left
;;                     (format-mode-line
;;                      '((:eval (mood-line-segment-modified))
;;                        (:eval (mood-line-segment-buffer-name))
;;                        (:eval (mood-line-segment-position))
;;                        (:eval  (if (boundp 'phi-search--selection)
;;                                    (let ((total (length phi-search--overlays))
;;                                          (selection phi-search--selection))
;;                                      (when selection
;;                                        (format "(Match: %d/%d )" (1+ selection) total)))
;;                                  nil))
;;                        ;; (:eval (mood-line-segment-multiple-cursors))
;;                        ))

;;                     ;; Right
;;                     (format-mode-line
;;                      '((:eval (mood-line-segment-vc))
;;                        (:eval (mood-line-segment-major-mode))
;;                        (:eval (mood-line-segment-global-mode-string))
;;                        (:eval (mood-line-segment-flycheck))
;;                        " "))))))
;;   )

(use-package solaire-mode
  :ensure t
  :init
  (solaire-mode)
  :config
  (progn
    (add-hook 'after-change-major-mode-hook 'turn-on-solaire-mode))
  (solaire-mode-swap-bg))

(use-package bar-cursor
  :ensure t
  :config (bar-cursor-mode 1))

;; TODO: Denne virker ikke etter siste endringer
(use-package cursor-chg
  :load-path "elisp"
  :commands
  change-cursor-mode
  toggle-cursor-type-when-idle
  :config
  (change-cursor-mode 1)
  (toggle-cursor-type-when-idle 1))

(use-package yascroll
  :ensure t
  :hook ((prog-mode . yascroll-bar-mode)
         (text-mode . (lambda ()
                        (when (and (< (buffer-size) large-file-warning-threshold)
                                   (not (equal major-mode 'nxml-mode)))
                          (yascroll-bar-mode))))))


(provide 'ark-ui)
;;; ark-ui.el ends here

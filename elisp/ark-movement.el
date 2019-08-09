;;; ark-movement.el --- description -*- lexical-binding: t; -*-

(use-package expand-line
  :ensure t
  :bind (("M-l" . turn-on-expand-line-mode)))

(use-package saveplace
  :init
  (defvar cache-dir)
  (setq save-place-file (concat cache-dir "/places"))
  :config
  (save-place-mode 1)
  (setq save-place-forget-unreadable-files nil))

(use-package move-text
  :ensure t
  :bind* (("M-<up>" . move-text-up)
          ("M-<down>" . move-text-down)))

(use-package goto-chg
  :ensure t
  :bind (("C-l" . goto-last-change)
         ("C-S-l" . goto-last-change-reverse)))

(use-package goto-line-preview
  :ensure t
  :bind* ("C-g" . goto-line-preview))

(use-package mwim
  :ensure t
  :bind
  ("<home>" . mwim-beginning)
  ("<end>" . mwim-end))

(use-package dumb-jump
  :ensure t
  :bind ("M-d" . dumb-jump-go-prefer-external)
  :config (setq dumb-jump-selector 'ivy))

(provide 'ark-movement)

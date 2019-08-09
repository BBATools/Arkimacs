;;; ark-code.el --- description -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t)
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))

(use-package sh-script
  :mode "\\.sh\\'"
  :config (setq sh-basic-offset 2 sh-basic-offset 2))

(use-package elisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("Cask" . emacs-lisp-mode)))

(use-package python-mode
  :mode "\\.py\\'"
  :custom (python-indent-offset 2))

(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'\\|\\.gradle\\'")

(use-package kotlin-mode
  :ensure t
  :mode "\\.kt\\'")


;; TODO: Legg inn at escape avslutter (exit?) completion
(use-package company :ensure t
  :ensure t
  :bind
  ;; "C-SPC"
  (("C-SPC" . company-complete-common)

   )
  ;; :map company-mode-map
  ;; ("<tab>" . company-complete-common))
  ;; (:map company-active-map
  ;; ("<tab>" . company-complete-common-or-cycle))
  ;; (:map company-template-nav-map
  ;; ("<tab>" . company-complete-common)
  ;; ("<C-return>" . company-template-forward-field))

  :init
  (global-company-mode)
  (setq-default company-require-match nil
                company-idle-delay nil))

(provide 'ark-code)
;;; ark-code.el ends here

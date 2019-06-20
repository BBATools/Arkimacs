;;; ark-outline.el --- description -*- lexical-binding: t; -*-

;; TODO: Endre bakgrunnsfarge på heading
;; TODO: Endre så lisp-mode også bruker stjerner
(use-package outshine
  :ensure t
  :bind (:map outshine-mode-map
              ("<S-iso-lefttab>" . outshine-cycle) ;; TODO: Finn mer behagelig binding
              ;; ("<backtab>"       . outshine-cycle-buffer) ;; shift-tab
              )
  :hook (prog-mode . outshine-mode)
  :config (setq outshine-fontify-whole-heading-line t)
  )

(use-package backline
  :ensure t
  :after outshine
  :config (advice-add 'outline-flag-region :after 'backline-update))


(provide 'ark-outline)

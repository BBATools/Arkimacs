;;; ark-search.el --- description -*- lexical-binding: t; -*-

;; TODO: Binding for counsel-rg -> kombinere med swiper i en hydra på M-g?

(use-package swiper
  :ensure t
  :bind ("M-g" . swiper-isearch)
  )

;; (use-package lazy-search
;;   :load-path "elisp"
;;   :bind ("C-f" . lazy-search-edit-object)
;;   )


(use-package phi-search
  :ensure t
  ;; :after mood-line
  :bind
  ("C-f" . phi-search)
  ("C-S-f" . phi-search-backward)
  ("<f3>" . ark-phi-search-repeat)
  (:map phi-search-default-map
        ("<escape>" . phi-search-abort )
        ("<f3>" . phi-search-again-or-next)
        )
  :config
  (setq phi-search-mode-line-format mode-line-format) ;; Too early if in custom
  (defun ark-phi-search-repeat() ;; TODO: fiks så ikke må trykke f3 to ganger
    (interactive)
    (progn
      (phi-search)
      (phi-search-again-or-previous)
      )))



;; WAIT: Se her for backward-versjon mm av aquamacs sine som er knabbet under: https://searchcode.com/codesearch/view/2173409/
;; WAIT: Fiks at må bruke esc 2 ganger av og til (bare når restatet søk med F3?)
;; (use-package isearch
;;   ;; :bind
;;   ;; ("C-f" . ark-isearch-forward)
;;   ;; ("<f3>" . ark-repeat-isearch)
;;   ;; (:map isearch-mode-map
;;   ;;       ("<f3>" . ark-repeat-isearch)
;;   ;;       ("<escape>" . isearch-abort)
;;   ;;       ("<up>" . isearch-ring-retreat)
;;   ;;       ("<down>" . isearch-ring-advance)
;;   ;;       ("<left>" . isearch-repeat-backward)
;;   ;;       ("<right>" . isearch-repeat-forward)
;;   ;;       )
;;   ;; (:map minibuffer-local-isearch-map
;;   ;;       ("<left>" . isearch-reverse-exit-minibuffer)
;;   ;;       ("<right>" . isearch-forward-exit-minibuffer)
;;   ;;       )
;;   :config
;;   ;; TODO: Flytt noen av de under fra config til bind
;;   ;; (define-key query-replace-map [return] 'act)
;;   (define-key query-replace-map [?\C-m] 'act) ;; TODO: Bare denne som virker for enter men ønsker å bruke C-m til annet
;;   (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

;;   (defadvice isearch-search (after isearch-no-fail activate)
;;     (unless isearch-success
;;       (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
;;       (ad-activate 'isearch-search)
;;       (isearch-repeat (if isearch-forward 'forward))
;;       (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
;;       (ad-activate 'isearch-search)))

;;   (defvar set-region-to-isearch-match t)

;;   (defun ark-set-region-to-search-match ()
;;     (when (and set-region-to-isearch-match
;;                (not isearch-mode-end-hook-quit)
;;                ;; (or aquamacs-isearching
;;                (eq set-region-to-isearch-match 'always)
;;                ;; )
;;                transient-mark-mode
;;                isearch-other-end)
;;       (goto-char (point))
;;       (push-mark isearch-other-end t t)
;;       (setq transient-mark-mode
;;             (if (eq transient-mark-mode 'lambda)
;;                 '(only)
;;               (if (consp transient-mark-mode)
;;                   transient-mark-mode
;;                 (cons 'only transient-mark-mode)))))
;;     (when isearch-mode-end-hook-quit
;;       (deactivate-mark)))

;;   (defun ark-isearch-forward ()
;;     (interactive)
;;     (if set-region-to-isearch-match
;;         (deactivate-mark))
;;     (call-interactively 'isearch-forward))

;;   (defun ark-repeat-isearch (&optional dir)
;;     (interactive)
;;     (if set-region-to-isearch-match
;;         (progn
;;           (deactivate-mark)
;;           (if (not isearch-mode)
;;               (isearch-mode t isearch-regexp))
;;           (isearch-repeat (or dir 'forward))
;;           (ark-set-region-to-search-match))
;;       (isearch-repeat (or dir 'forward))))
;;   )



(provide 'ark-search)

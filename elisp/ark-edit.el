;;; ark-edit.el --- description -*- lexical-binding: t; -*-

(use-package emacs
  :config
  (global-eldoc-mode -1)
  (delete-selection-mode)

  (dolist (cmd '(upcase-region
                 downcase-region
                 erase-buffer
                 eval-expression
                 dired-find-alternate-file
                 set-goal-column))
    (put cmd 'disabled nil))

  (defadvice kill-ring-save (before smart-copy activate compile)
    "When called interactively with no active region, copy a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-end-position)))))

  (defadvice kill-region (before smart-cut activate compile)
    "When called interactively with no active region, kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2)))))
  )

;; TODO: Legg inn for xml og xsl-filer også
(use-package fic-mode
  :straight (:host github :repo "lewang/fic-mode" :files ("*.el"))
  :hook ((prog-mode) . fic-mode)
  :config
  (setq fic-highlighted-words '("TODO" "TODO:" "WAIT" "WAIT:"))
  (dolist (face '(fic-face fic-author-face))
    (set-face-foreground face "#d0bf8f")
    (set-face-background face "gray40"))
  )

(use-package undo-redo
  :straight (:host github :repo "clemera/undo-redo" :files ("*.el"))
  :bind* (("C-y"  . redo)
          ("C-z" . undo-modern)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode clojure-mode) . rainbow-delimiters-mode))

(use-package paren
  :config
  (remove-hook 'post-self-insert-hook
               #'blink-paren-post-self-insert-function)
  (setq blink-matching-paren 'show)
  (let ((ov nil)) ; keep track of the overlay
    (advice-add
     #'show-paren-function
     :after
     (defun show-paren--off-screen+ (&rest _args)
       "Display matching line for off-screen paren."
       (when (overlayp ov)
         (delete-overlay ov))
       ;; check if it's appropriate to show match info,
       ;; see `blink-paren-post-self-insert-function'
       (when (and (overlay-buffer show-paren--overlay)
                  (not (or cursor-in-echo-area
                           executing-kbd-macro
                           noninteractive
                           (minibufferp)
                           this-command))
                  (and (not (bobp))
                       (memq (char-syntax (char-before)) '(?\) ?\$)))
                  (= 1 (logand 1 (- (point)
                                    (save-excursion
                                      (forward-char -1)
                                      (skip-syntax-backward "/\\")
                                      (point))))))
         ;; rebind `minibuffer-message' called by
         ;; `blink-matching-open' to handle the overlay display
         (cl-letf (((symbol-function #'minibuffer-message)
                    (lambda (msg &rest args)
                      (let ((msg (apply #'format-message msg args)))
                        (setq ov (display-line-overlay+
                                  (window-start) msg ))))))
           (blink-matching-open))))))
  (defun display-line-overlay+ (pos str &optional face)
    "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
    (let ((ol (save-excursion
                (goto-char pos)
                (make-overlay (line-beginning-position)
                              (line-end-position)))))
      (overlay-put ol 'display str)
      (overlay-put ol 'face
                   (or face '(:inherit default :inherit highlight)))
      ol))
  (setq show-paren-style 'paren
        show-paren-delay 0.03
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren nil
        show-paren-when-point-in-periphery t)
  (show-paren-mode))

(use-package electric
  :hook (prog-mode . electric-indent-mode))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-<" . mc/mark-next-like-this)
         ;; ("C-<" . mc/mark-previous-like-this)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-0" . mc/mark-all-like-this)
         ("<escape>" . mc/keyboard-quit)
         ("M-<down-mouse-1>" . mc/add-cursor-on-click)))

(use-package smart-hungry-delete
  :ensure t
  :bind (:map prog-mode-map
              ("<backspace>" . smart-hungry-delete-backward-char)
              ("C-d" . smart-hungry-delete-forward-char))
  :hook
  (prog-mode . smart-hungry-delete-default-c-mode-common-hook)
  :defer nil) ;; dont defer so we can add our functions to hooks

(use-package whitespace-cleanup-mode
  :ensure t
  :hook (prog-mode . whitespace-cleanup-mode))

(use-package autorevert
  :config (global-auto-revert-mode))

(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode) . aggressive-indent-mode)
  :config
  (setq aggressive-indent-sit-for-time 0.5))


;; TODO: Ikke for store tekstfiler?
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

(provide 'ark-edit)

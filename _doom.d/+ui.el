;;; +ui.el -*- lexical-binding: t; -*-

;; TODO: Hvilken endring førte til at kode under sluttet å virke?
(if (daemonp)
    (add-hook 'after-make-frame-functions
              '(lambda (f)
                 (with-selected-frame f
                   (when (window-system f)
                     (setq hl-todo-keyword-faces
                           `(("WAIT"  . ,(face-foreground 'warning))
                             ("TODO"  . ,(face-foreground 'error))
                             ("NOTE"  . ,(face-foreground 'success))))
                     )))))

;;; Frame:
(tool-bar-mode -1) ;; Earlier setting ignored when started with emacsclient
(setq-default frame-title-format '("Arkimacs"))
(setq-default icon-title-format  '("Arkimacs"))
(menu-bar-mode 1)
(setq doom-theme 'doom-nord)

;;; Fonts
(when (member "Source Code Pro" (font-family-list))
  (setq doom-font (font-spec :family "Source Code Pro" :size 14)
        doom-variable-pitch-font (font-spec :family "Source Code Pro")
        doom-unicode-font (font-spec :family "Source Code Pro")
        doom-big-font (font-spec :family "Source Code Pro" :size 15)))

;;; Hide modeline
(setq-default mode-line-format nil)
(add-hook 'window-configuration-change-hook
          (lambda ()
			(feebleline-mode t)
			(setq feebleline-msg-functions
			      '((feebleline-line-number         ((post . "") (fmt . "%5s")))
			        (feebleline-column-number       ((pre . ":") (fmt . "%-2s")))
			        (feebleline-file-directory      ((face . feebleline-dir-face)    (post . "")))
			        (feebleline-file-or-buffer-name ((face . font-lock-keyword-face) (post . "")))
			        (feebleline-file-modified-star  ((face . font-lock-warning-face) (post . "")))
                    ))))

;;; Cursor:
(bar-cursor-mode 1)

;;; Tabs:
;; (awesome-tab-mode t)
(def-package! awesome-tab
  :init
  ;; (defface awesome-tab-unselected
    ;; '((t (:inherit font-lock-string-face)))
    ;; "Face used for unselected tabs." :group 'awesome-tab)
  ;; (defface awesome-tab-selected
    ;; '((t (:inherit font-lock-type-face :weight ultra-bold :width semi-expanded
                 ;; :foreground "green3" :overline "green3")))
    ;; "Face used for the selected tab." :group 'awesome-tab)
  ;; TODO: Juster farger under (se på frame-tabs config)
  (setq awesome-tab-cycle-scope 'tabs)
  :config
  (set-face-attribute
   'awesome-tab-default nil
   :height 1.1)
  (set-face-attribute
   'awesome-tab-selected nil
   :foreground "#839496"
   :overline "#002b36")
  (set-face-attribute
   'awesome-tab-unselected nil
   :foreground "#002b36"
   ;; :background "#839496"
   :background "#d9d9d9"
   :overline "#839496")
  (awesome-tab-mode t))


;; TODO: 
;; https://github.com/manateelazycat/awesome-tab/issues/11
;; https://github.com/manateelazycat/awesome-tab/issues/10
;; (defvar ivy-source-awesome-tab-group nil)

;; (face-spec-set
;; 'frame-tabs-buffer-tab
;; '((t :inherit variable-pitch
;; :box (:line-width 2 :color "#3B4252")
;; :foreground "#DEDFB5"
      ;;;;:foreground (face-attribute 'tooltip  :background)
      ;;;;:background (face-attribute 'bg  :background)
      ;;;;:background modeline-bg ;;WAIT: Hvorfor virker ikke denne (virker i theme-kode)
;; :background "#3B4252"))
;; 'face-defface-spec)

;; (face-spec-set
;; 'frame-tabs-selected-tab
;; '((t :inherit frame-tabs-buffer-tab
;; :foreground "#E1B269"))
;; 'face-defface-spec)


;; (face-spec-set
;; 'frame-tabs-higlight-tab
;; '((t :inherit frame-tabs-buffer-tab
;; :foreground "white"
;; :background "#434C5E"))
;; 'face-defface-spec)

;; (frame-tabs-mode)
;; (setq frame-tabs-delay 0.1) ;; Doesn't work with a value of zero
;; (setq frame-tabs-buffer-list 'ark-frame-tabs-buffer-list)

;;; Scrollbar:
(add-hook 'window-configuration-change-hook
          (lambda ()
			(global-yascroll-bar-mode 1)
			(setq yascroll:delay-to-hide nil)
			(fringe-mode '(4 . 6))
			(setq yascroll:disabled-modes '(shell-mode eshell-mode))
            (set-face-background 'yascroll:thumb-text-area (face-attribute 'highlight  :background))
            (set-face-foreground 'yascroll:thumb-fringe (face-attribute 'highlight  :background))
            (set-face-background 'yascroll:thumb-fringe (face-attribute 'highlight  :background))
            ))

(defun ark-untitled-buffer-kill-query-function ()
  "Don't kill untitled buffer without asking if modified"
  (if (and (not buffer-file-name)
           (buffer-modified-p)
           (boundp 'ark-untitled-buffer))
      (if 'ark-untitled-buffer
          (yes-or-no-p "Untitled buffer modified. Kill it anyway? "))
;;; TODO: endre så tekst lik til midtklikk mus. Evt. endre begge til å bruke ivy
    t))
(add-to-list 'kill-buffer-query-functions 'ark-untitled-buffer-kill-query-function)

(defadvice switch-to-buffer (after ark-kill-empty-untitled activate)
  (let ((-buf (current-buffer)))
    (let ((temp-buffer-list (ark-buffer-list)))
      (loop for buf in temp-buffer-list
            do (if (and (string-prefix-p "untitled" (buffer-name buf))
                        (not (buffer-modified-p buf))
                        (not (buffer-file-name buf))
                        (not (eq buf -buf)))
                   (kill-buffer buf))))))

;; Remove the request from killing emacs:
(setq confirm-kill-emacs nil)

;; Do not ask for confirm when killing processes:
(setq confirm-kill-processes nil)

(def-package! highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode))

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive t)
  (setq highlight-indent-guides-character ?\┆)
  (setq highlight-indent-guides-auto-enabled 'top)
  (set-face-attribute 'highlight-indent-guides-odd-face nil :inherit 'highlight-indentation-odd-face)
  (set-face-attribute 'highlight-indent-guides-even-face nil :inherit 'highlight-indentation-even-face)
  (set-face-foreground 'highlight-indent-guides-character-face (doom-color 'base5)))

(provide '+ui)
;;; +ui.el ends here

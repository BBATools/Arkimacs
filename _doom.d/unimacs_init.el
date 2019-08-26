;;; init ---  Unimacs

;;; Commentary:
;;; Fully customized Emacs configurations

;;; Code:
(setq redisplay-dont-pause t)           ; redraw the display before it processes queued input events

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(defconst unimacs-home (concat "/home/" (user-login-name)))     ; Set variable home
(defconst unimacs-lisp (concat user-emacs-directory "lisp"))         ; Subdirectory with lisp-files
(setq temporary-file-directory (concat user-emacs-directory "/tmp"))
(setq small-temporary-file-directory (concat user-emacs-directory "/tmp"))
;; (setq tramp-persistency-file-name (concat temporary-file-directory "/places"))
(unless (file-directory-p temporary-file-directory) ; Make directory if not exists
  (make-directory temporary-file-directory t))
(setq custom-theme-directory (concat user-emacs-directory "/themes"))
(let ((default-directory (file-name-as-directory unimacs-lisp))) ; Add unimacs-lisp and subdirs to path
  (add-to-list 'load-path unimacs-lisp)
  (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)

;; TODO: gjør win line ending default i emacs

;; WAIT: Lag det under som egen pakke "uni-untitled" og kall med use-package

(load-theme 'subatomic t)

(with-current-buffer (get-buffer "*scratch*") ; Untitled buffer, not scratch on start-up
  (rename-buffer "Untitled")
  (text-mode)
  (setq indent-line-function 'insert-tab))

(defun uni-buffer-untitled-p (buffer)
  "Check if untitled-buffer"
  (string-prefix-p "Untitled" (buffer-name buffer)))

(defun uni-buffers-untitled-p ()
  "Check if one or more untitled-buffers in buffer-list"
  (let (buffer)
    (let ((buffers (cdr (buffer-list))))
      (while buffers
        (when (uni-buffer-untitled-p buffer)
          t)
        (setq buffers (cdr buffers))))))

(defun uni-untitled-buffer ()
  "Opens a new empty buffer"
  (interactive)
  (let (untitled)
    (let ((buffers (buffer-list)))
      (while (and buffers (not untitled))
        (let  ((buffer (car buffers)))
          (when (and (uni-buffer-untitled-p buffer)
                     (not (buffer-modified-p buffer)))
            (setq untitled (buffer-name buffer)))
          (setq buffers (cdr buffers))))
      (if untitled
          (switch-to-buffer untitled)
        (let ((buf (generate-new-buffer "Untitled")))
          (switch-to-buffer buf)
          (text-mode))))))

(defadvice switch-to-buffer (after remove-unmodified-untitled-on-switch activate)
  (let ((buffer (other-buffer)))
    (when (and (uni-buffer-untitled-p buffer)
               (not (buffer-modified-p buffer)))
      (kill-buffer buffer))))

;;; shortcuts
;; miscellaneous


(bind-key "C-g" 'goto-line)
(setq-default tab-width 4)

(setq-default indent-tabs-mode nil)     ; No tab characters
(add-hook 'text-mode-hook               ; Change standard indentation for text-mode
          '(lambda ()
             (setq indent-line-function 'insert-tab)))

(auto-compression-mode 1)               ; Transparently open compressed files
(setq next-line-add-newlines 1)         ; Add newline when pressing down arrow at end of buffer

(use-package autorevert                 ; Reload buffer if changed by external program
  :commands auto-revert-mode
  :diminish auto-revert-mode
  :init
  (mapc (lambda (hook)
          (add-hook hook (lambda ()
                           (auto-revert-mode 1))))
        '(find-file-hook
          dired-mode-hook))
  :config
  (progn
    (setq auto-revert-verbose nil)      ; Avoid message when reverting
    ))

(bind-key "C-n" 'uni-untitled-buffer)

(use-package undo-tree                      ; A better undo/redo system
  :commands
  (progn
    undo-tree-undo
    undo-tree-redo)
  :init
  (progn
    (bind-key "C-z" 'undo-tree-undo)
    (bind-key "C-y" 'undo-tree-redo)))

(set-face-attribute 'default nil :height 105) ; font size
(tool-bar-mode 0)                       ; disable toolbar
(scroll-bar-mode 0)                     ; disable scrollbar
(setq column-number-mode t)             ; column numbers as well as line numbers in mode-line
(blink-cursor-mode 0)                   ; No blinking cursor

;; WAIT: Sjekk om mulig å disable for enkelte modes - shell, minibuffer, dired mm
(use-package cursor-chg                 ; Change cursor dynamically, depending on the context
  :init
  (progn
    (setq curchg-default-cursor-color "#F07746") ; TODO: Sett andre farger når theme avklart
    (setq curchg-input-method-cursor-color "#F07746")
    (change-cursor-mode 1)              ; On for overwrite/read-only/input mode
    (toggle-cursor-type-when-idle 1)))  ; On when idle

;; visually wrap lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; matching parentheses
(show-paren-mode 1)                     ; Highlight matching delimiters

(require 'highlight-fixmes-mode)        ; Highlight fixme, todo, bug and other warning comments
(setq fixme-words '("FIXME:" "TODO:" "WAIT:"))
(set-face-foreground 'fixme-face (face-foreground 'show-paren-mismatch)) ; TODO: Denne gir ikke samme
(eval-after-load "highlight-fixmes-mode"                                  ; Remove from modeline
  '(diminish 'highlight-fixmes-mode))
(define-globalized-minor-mode global-highlight-fixmes-mode highlight-fixmes-mode
  (lambda ()
    (highlight-fixmes-mode 1)))
(global-highlight-fixmes-mode 1)        ; Activate for all modes

(add-hook 'after-save-hook              ; Make the file executable if it is a script
          'executable-make-buffer-file-executable-if-script-p)

(setq mouse-yank-at-point t)            ; Insert text at cursor position

(setq mouse-wheel-scroll-amount '(5 ((shift) . 5))) ; Five lines at a time
(setq mouse-wheel-progressive-speed nil)            ; Don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't)       ; Scroll window under mouse

(bind-key "<mouse-3>" 'uni-context-menu)
(defun uni-context-menu (event)
  "Pop up a context menu."
  (interactive "e")
  (popup-menu uni-popup-menu))
(defvar uni-popup-menu
  '(keymap
    (undo menu-item "Undo" undo
          :enable (and
                   (not buffer-read-only)
                   (not
                    (eq t buffer-undo-list))
                   (if
                       (eq last-command 'undo)
                       (listp pending-undo-list)
                     (consp buffer-undo-list)))
          :help "Undo last operation"
          :keys "Ctrl+Z")
    (separator-undo menu-item "--")
    (cut menu-item "Cut" clipboard-kill-region
         :help "Delete text in region and copy it to the clipboard"
         :keys "Ctrl+X")
    (copy menu-item "Copy" clipboard-kill-ring-save
          :help "Copy text in region to the clipboard"
          :keys "Ctrl+C")
    (paste menu-item "Paste" clipboard-yank
           :help "Paste text from clipboard"
           :keys "Ctrl+V")
    (paste-from-menu menu-item "Paste from Kill Menu" yank-menu
                     :enable (and
                              (cdr yank-menu)
                              (not buffer-read-only))
                     :help "Choose a string from the kill ring and paste it"
                     :keys "Alt+K")
    (clear menu-item "Clear" delete-region
           :enable (and mark-active (not buffer-read-only))
           :help "Delete the text in region between mark and current position"
           :keys "Del")
    (separator-select-all menu-item "--")
    (mark-whole-buffer menu-item "Select All" mark-whole-buffer
                       :help "Mark the whole buffer for a subsequent cut/copy"
                       :keys "Ctrl+A")))

(setq custom-file (expand-file-name (concat user-emacs-directory "unimacs-custom.el")))
(load custom-file 'noerror)
;; Make customize display real variable names:
(setq custom-unlispify-tag-names nil)
(setq custom-unlispify-menu-entries nil)

;; Enable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(bind-key "C-u" 'uni-upcase)
(defun uni-upcase ()
  (interactive)
  (if (use-region-p)
      (upcase-region (region-beginning)
                     (region-end))
    (upcase-word)))

(bind-key "C-l" 'uni-downcase)
(defun uni-downcase ()
  (interactive)
  (if (use-region-p)
      (upcase-region (region-beginning)
                     (region-end))
      (downcase-word)))

(defvar uni-map (make-sparse-keymap) ; Own keymap for overriding other modes
  "Unimacs keymap.")
(defvar uni-map-alist `((t . ,uni-map))
  "Unimacs keymap alist.")
(add-to-ordered-list 'emulation-mode-map-alists 'uni-map-alist 100)

;; WAIT: M og s (super) for "vindus-kommandoer" og C for "edit-kommandoer" som hovedregel -> fiks under og andre steder
;; Unntak for b shift-text da dette er keys fra bl.a. eclipse

;; WAIT: Overstyr keybindings for dired-mode og shell mode
(use-package shift-text                 ; Move the line(s) spanned by the active region
  :commands
  (progn
    shift-text-up
    shift-text-down
    shift-text-left
    shift-text-right)
  :init
  (progn
    (require 'cl-lib)
    ;; WAIT: I conifg heller -> annen mer elegant måte ? (skulle ha vært i shift-pakke direkte)
    ;; -> vil ikke en require her selv når i config bli lastet direkte ? -> test
    (bind-key "M-<up>" 'shift-text-up)
    (bind-key "M-<kp-up>" 'shift-text-up)
    (bind-key "M-<kp-8>" 'shift-text-up)
    (bind-key "M-<down>" 'shift-text-down)
    (bind-key "M-<kp-down>" 'shift-text-down)
    (bind-key "M-<kp-2>" 'shift-text-down)
    (bind-key "M-<left>" 'shift-text-left)
    (bind-key "M-<kp-left>" 'shift-text-left)
    (bind-key "M-<kp-4>" 'shift-text-left)
    (bind-key "M-<right>" 'shift-text-right)
    (bind-key "M-<kp-right>" 'shift-text-right)
    (bind-key "M-<kp-6>" 'shift-text-right)))

;; WAIT: Hvorfor gikk jeg vekk fra C-< for defun under ? Fordi slik i annen editor ?
(keyboard-translate ?\C-m ?\H-m)
(global-set-key [?\H-m] 'uni-goto-match-paren)
;; WAIT: Nødvendig med "arg" og "p" her ?
(defun uni-goto-match-paren (arg)
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1))
        ((looking-back "[])}]") (backward-sexp 1))
        (t
         (while (not (looking-at "[([{]"))
           (backward-char 1)))))

(bind-key "C-s" 'save-buffer)
(bind-key "C-v" 'yank)
(bind-key "C-x" 'uni-kill-line-or-region uni-map)
(bind-key "C-c" 'uni-copy-line-or-region uni-map)

;; WAIT: Også håndtere popwin med denne ? popwin:popup-last-buffer eller
;; popwin:switch-to-last-buffer eller popwin:original-pop-to-last-buffer ?
;; TODO: Den under fanges ikke opp av switch med helm
(defun uni-previous-buffer ()
  (interactive)
  (if popwin:focus-window
      (popwin:close-popup-window)
    (let ((buffers (cdr (buffer-list))))
      (while buffers
        (when (with-current-buffer (car buffers)
                (let ((buffer (current-buffer)))
                  (or
                   (buffer-file-name buffer)
                   (uni-buffer-untitled-p buffer))))
          (switch-to-buffer (car buffers))
          (setq buffers nil))
        (setq buffers (cdr buffers))))))

(defun uni-copy-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (progn
      (kill-ring-save (line-beginning-position) (line-end-position))
      (message "Copied line"))))

(defun uni-kill-line-or-region ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-whole-line)))

(bind-key "C-a" 'mark-whole-buffer)

(bind-key "C-w" 'uni-kill-buffer)
(defun uni-kill-buffer (&optional buffer)
  (interactive)
  (if popwin:focus-window
      (popwin:close-popup-window)
    (let ((buffer (or buffer (current-buffer))))
        (if (and (buffer-modified-p)
                 (or
                  (buffer-file-name buffer)
                  (uni-buffer-untitled-p buffer)))
            (if (y-or-n-p "Buffer modified; Do you want to save? ")
                (save-buffer))))
    (let ((buffername (buffer-name (current-buffer))))
      (uni-previous-buffer)
      (if (not (eq nil (get-buffer buffername)))
          (kill-buffer buffername)))
    (if (and (not (delq nil (mapcar 'buffer-file-name (buffer-list))))
             (not (uni-buffers-untitled-p)))
        (uni-untitled-buffer))))

(bind-key "C-<home>" 'beginning-of-buffer)
(bind-key "C-<kp-7>" 'beginning-of-buffer)
(bind-key "C-<kp-home>" 'beginning-of-buffer)

(bind-key "C-<end>" 'end-of-buffer)
(bind-key "C-<kp-1>" 'end-of-buffer)
(bind-key "C-<kp-end>" 'end-of-buffer)

(global-set-key [(home)] 'uni-beginning-of-line)
(global-set-key [(kp-7)] 'uni-beginning-of-line)
(global-set-key [(kp-home)] 'uni-beginning-of-line)
(defun uni-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line"
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [(end)] 'uni-end-of-line)
(global-set-key [(kp-1)] 'uni-end-of-line)
(global-set-key [(kp-end)] 'uni-end-of-line)

(defun uni-end-of-line ()
  "Move to EOL. If already there, to EOL sans comments.
    That is, the end of the code, ignoring any trailing comment
    or whitespace.  Note this does not handle 2 character
    comment starters like // or /*.  Such will not be skipped."
  (interactive "^")
  (if (not (eolp))
      (end-of-line)
    (skip-chars-backward " \t")
    (let ((pt (point))
          (lbp (line-beginning-position))
          (lim))
      (when (re-search-backward "\\s<" lbp t)
        (setq lim (point))
        (if (re-search-forward "\\s>" (1- pt) t)
            (goto-char pt)
          (goto-char lim)
          (while (looking-back "\\s<" (1- (point)))
            (backward-char))
          (skip-chars-backward " \t"))))))

;; Imenu:
(set-default 'imenu-auto-rescan t)      ; Update imenu-index automatically
(setq imenu-sort-function 'imenu--sort-by-name) ; Sort by name
(setq imenu-max-items 40)               ; Items per menu
(global-set-key (kbd "M-i") 'imenu-anywhere)

(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index) ; Add imenu to menubar

(bind-key "<escape>" 'keyboard-escape-quit minibuffer-local-map) ; Single escape
(bind-key "C-S-<kp-down>" 'uni-comment-and-go-down)
(bind-key "C-S-<down>" 'uni-comment-and-go-down)
(bind-key "C-S-<kp-2>" 'uni-comment-and-go-down)
(defun uni-comment-and-go-down ()
  "Uncomments the current line and goes to the next one for 'programming-modes'"
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (condition-case nil
        (comment-region (point-at-bol)
                        (point-at-eol))
      (error nil))
    (next-logical-line 1)))

(bind-key "C-S-<kp-up>" 'uni-uncomment-and-go-up)
(bind-key "C-S-<up>" 'uni-uncomment-and-go-up)
(bind-key "C-S-<kp-8>" 'uni-uncomment-and-go-up)
(defun uni-uncomment-and-go-up ()
  "Uncomments the current line and goes to the previous one for 'programming-modes'"
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (condition-case nil
        (uncomment-region (point-at-bol)
                          (point-at-eol))
      (error nil))
    (next-logical-line -1)))

(bind-key "C-S-<return>" 'uni-comment-dwim-line)
(bind-key "C-S-<kp-enter>" 'uni-comment-dwim-line)
(bind-key "C-S-<enter>" 'uni-comment-dwim-line)
(defun uni-comment-dwim-line            ; WAIT: Er nok stjålet -> gjør til egen og juster kommentar-tekst
  (&optional arg)
  "For 'programming-modes'. If no region is selected and current line is not blank and we are not at the end of the line,
   then comment current line. Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (when (derived-mode-p 'prog-mode)
    (comment-normalize-vars)
    (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
        (comment-or-uncomment-region (line-beginning-position) (line-end-position))
      (comment-dwim arg))))

(bind-key "C-q" 'save-buffers-kill-terminal)

;; (use-package recentf
  ;; :if (not noninteractive)
  ;; :init
  ;; (progn
    ;; (setq recentf-save-file (concat temporary-file-directory "/recentf-" system-name)) ; Enable recently-opened files menu
    ;; (setq recentf-menu-before "Close")
    ;; (recentf-mode 1))
  ;; :config
  ;; (defadvice recentf-track-closed-file (after push-beginning activate)
    ;; "Move current buffer to the beginning of the recent list after killed."
    ;; (recentf-track-opened-file)))

;;; settings
;; enable all commands
;; (setq disabled-command-function nil)
;; kill whole line (including newline)
(setq kill-whole-line t)
;; initial text mode
;; (setq initial-major-mode 'text-mode)
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; disable bell
;; (setq ring-bell-function 'ignore)
;; subword navigation
;; (global-subword-mode t)
;; increase garbage collection threshold
(setq gc-cons-threshold 20000000)
;; inhibit startup message
(setq inhibit-startup-message t)
;; remove selected region if typing
(pending-delete-mode 1)
;; prefer UTF8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; set terminfo
;; (setq system-uses-terminfo nil)
;; open empty files quietly
;; (setq confirm-nonexistent-file-or-buffer nil)
;; fix tramp
(eval-after-load 'tramp
  '(progn (setenv "TMPDIR" "/tmp")))

;;; files
;; auto-save
(setq auto-save-timeout 60)
;; backups
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq backup-directory-alist `(("." . ,(concat
					user-emacs-directory "backups"))))
;; symlink version-control follow
(setq vc-follow-symlinks t)
;; set arduino *.ino files to c-mode
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))
;; set Emacs Lisp files mode
(add-to-list 'magic-mode-alist '(";;; " . emacs-lisp-mode))

;; company "complete anything"
(use-package company
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :init
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)))

;; popwin
(use-package popwin
  :init
  (progn
    (popwin-mode 1)
    ;; cannot use :bind for keymap
    ;; (global-set-key (kbd "C-z") popwin:keymap)
    ))

(eval-after-load "popwin"
  `(progn
     (setq popwin:popup-window-height 0.2)
     (setq popwin:special-display-config
           (append '(
                     ("^\*helm .+\*$" :regexp t)
                     ("^\*helm-.+\*$" :regexp t)
                     ("^\*helm*" :regexp t)
                     ) ; WAIT: Lag bedre regex for å fange opp alle helm men ikke annet -> sjekke mot mode heller ?
                   popwin:special-display-config))))

;; WAIT: Se denne for yttrerligere muligheter for justering av helm:
;; https://stackoverflow.com/questions/19949212/emacs-helm-completion-how-to-turn-off-persistent-help-line
(use-package helm-config
  :diminish helm-mode
  :init
  (progn
    ;; TODO: Feil når disse brukes når allerede åpnet helm for noe-> FM: Error: Trying to run helm within a running helm session
    (bind-key "C-o" 'helm-find-files)    
    (bind-key "M-k" 'helm-show-kill-ring)
    (bind-key "M-i" 'helm-imenu)

    ;; TODO: Denne virker bra men blir ikke fanget opp av popwin -> fiks -> test først bind-key (er installert) som alternativ
    (use-package helm-descbinds
      :commands helm-descbinds
      :init
      (fset 'describe-bindings 'helm-descbinds))
    ;; (bind-key "C-h b" 'helm-descbinds) ;; TODO: Lag bedre key
    )
  :config
  (progn
    (helm-mode 1)))

(set-face-background 'helm-source-header (face-background 'default))
(set-face-background 'helm-match (face-background 'default))
(set-face-foreground 'helm-match (face-foreground 'font-lock-function-name-face))
(set-face-background 'helm-match (face-background 'default))
(set-face-foreground 'helm-grep-match (face-foreground 'font-lock-function-name-face))
(set-face-background 'helm-selection (face-background 'default))
(set-face-foreground 'helm-selection (face-foreground 'font-lock-keyword-face))

(let ((faces                            ; Some nicer colors, mainly from ambience-theme
       '(
         ;; (font-lock-comment-face :foreground "#4d7a70" :slant italic)
         ;; (helm-candidate-number :background (face-background 'modeline-highlight))
         ;; (default :background "#FAFAF9")
         ;; (fringe :background "#FAFAF9")
         (helm-selection :underline nil)
         (helm-source-header :height 1.0)
         ;; (region :background "#E5E3E1") ; WAIT: Hent farge fra theme "radiance"
         ;; (modeline :background "#DEDBDA")
         )))
  (dolist (face faces)
    (apply 'set-face-attribute (car face) nil (cdr face))))

(set-face-background 'mode-line (face-background 'mode-line-inactive))
(set-face-attribute 'mode-line nil :weight 'light)

;;; save kill ring
(use-package savekill)
;; WAIT: Høyreklikkemeny fanger ikke opp de som er lagret fra forrige sesjon men M-k gjør -> sjekk kommando brukt i meny

;;; saveplace
(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq save-place-file (concat user-emacs-directory "saved-places"))))

;; activate smartparens
(use-package smartparens
  :diminish smartparens-mode
  :init
  (progn (smartparens-global-mode t)
         (show-smartparens-global-mode t)
         (use-package smartparens-config)))

;;; uniquify
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; WAIT: Se om mulig å gjøre høyden mindre -> eget buffernavn som kan fanges opp av popwin ?
;; WAIT: MUlig å fjerne duplisering av tekst som vises i minibuffer -> disable visning av minibuffer et øyeblikk bare ?
(defun uni-yes-or-no-p (prompt)
  "Ask user a yes-or-no question using helm"
  (let* ((yes-or-no-prompt (concat prompt " "))
         (choices '("yes" "no"))
         (answer (helm-comp-read
                  yes-or-no-prompt
                  choices
                  :must-match t
                  :name yes-or-no-prompt
                  )))
    (string= answer "yes")))

(defalias 'yes-or-no-p 'uni-yes-or-no-p)
(defalias 'y-or-n-p 'uni-yes-or-no-p)

;; TODO: Lag sjekk om på linux før kode under kjøres
;; --> legg inn at denne brukes for windows: https://github.com/d5884/w32-resume-frame/blob/master/w32-resume-frame.el
(defun uni-save-frame-size ()           ; Save frame-size between sessions
  (uni-de-maximize)
  (call-process-shell-command "nohup grep -q 'emacs.geometry:*' ~/.Xresources 2> /dev/null || echo 'emacs.geometry: 80x35' >> ~/.Xresources &")
  (call-process-shell-command (concat "nohup sed -i 's/^emacs.geometry: .*/emacs.geometry: " (number-to-string (frame-width)) "x" (number-to-string (frame-height)) "/g' ~/.Xresources &"))
  (call-process-shell-command "nohup xrdb -merge ~/.Xresources &"))
(defun uni-de-maximize ()
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(0 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(0 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
(add-hook 'kill-emacs-hook 'uni-save-frame-size)

(setq-default truncate-lines t)                      ; Truncate as default
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ; Except for text-mode
(bind-key "<f10>" 'visual-line-mode)                 ; Toggle Visual Line Mode

(bind-key "<escape>" 'uni-escape-dwim)
(defun uni-escape-dwim ()
  (interactive)
  (if popwin:focus-window
      (popwin:close-popup-window)
    (if (and mark-active
             transient-mark-mode)
        (deactivate-mark)
      (if (mark)
          (pop-to-mark-command)))))

(global-nlinum-mode 1)                   ; Show line numbers
(setq x-gtk-file-dialog-help-text nil)  ; Remove stupid text in file-dialog

(setq frame-title-format
      (setq icon-title-format
            '((:eval (if (buffer-file-name)
                         (abbreviate-file-name (buffer-file-name))
                       "%b")))))
(setq-default mode-line-buffer-identification nil) ; Remove buffer-name from modeline

(setq overwrite-mode-binary nil)        ; Remove modeline-text for overwrite-mode
(setq overwrite-mode-textual nil)

;; helm-candidate-number
 ;; helm-selection

; Make customize display real variable names:
(setq custom-unlispify-tag-names nil)
(setq custom-unlispify-menu-entries nil)

;; TODO: Tester disse:
;; helm-completion-window-scroll-margin
;; helm-external-programs-associations
;; helm-ff-file-name-history-use-recentf

(eval-after-load "startup" '(fset 'display-startup-echo-area-message (lambda () ; Disable startup message

;; WAIT: Fix så det under hentes auto fra mode-line-inactive heller
(set-face-attribute 'mode-line nil
    :box '(:line-width -1 :color "grey40" :style None)
    :weight 'light)

(defadvice helm-display-mode-line (after undisplay-header activate) ; Remove header and mode-line when in helm buffer
(progn
  (setq mode-line-format nil)
  (setq header-line-format nil)))

;; TODO: Lag keybinding for helm-descbinds -> få den til å åpne som andre helm-buffere i popwin også ?


;; -> test silver searcher ført for en løsning som virker også på windows -> ag virket ikke så bra -> test andre
;; --> ag og pinot ingen suksess -> test ack -> deb ack-grep er installert

;; WAIT: Bare for linux -> bruk denne for win: https://github.com/filsinger/helm-everything
(helm-recoll-create-source "main" "~/.recoll")
(defalias 'helm-recoll 'helm-recoll-main)

(setq enable-recursive-minibuffers t)   ; Stack  minibuffers

(setq savehist-additional-variables               ; Save minibuffer-history
      '(search-ring regexp-search-ring kill-ring)) ; Also save...
(setq savehist-file (expand-file-name (concat temporary-file-directory "/savehist-" system-name)))
(savehist-mode 1)

(bind-key "M-x" 'helm-M-x)

;; TODO: Test å bruke "run-with-idle-timer" for å lukke ido-windu, evt. velge markert buffer hvis stopper opp lenge nok

;;; GUI:
                                                                       (message " "))))

(bind-key "C-<tab>" 'uni-helm-buffers)
(bind-key "C-<tab>" 'helm-next-line helm-map)
(bind-key "<tab>" 'helm-next-line helm-map)

;; TODO: Endre fra blåfarge til hvitt på andre buffere som vises i liste
(setq ido-use-virtual-buffers t)
;; TODO: Sett opp lagring av file-name-history mellom sessions -> kan det gjøres i helm direkte eller med std emacs ?
(defun uni-helm-buffers ()
  (interactive)
  ;; (helm :sources 'helm-c-source-buffers-list)) ; TODO: Se hvordan denne sourcen er laget ift recentf-source og gjør lik
  ;; (helm :sources 'helm-c-source-file-cache)) ;; cmd-t gjør det samme hakket bedre
  ;; (helm :sources 'helm-c-source-findutils))
  ;; (helm :sources 'helm-c-source-ido-virtual-buffers)) ;; TODO: Denne er bedre når en kaller ido-switch-buffer direkte (har med åpne bufffer først)
  ;; ;; (helm :sources 'helm-c-source-tracker-search))
  (helm :sources '(helm-c-source-buffers-list helm-c-source-file-name-history helm-source-recoll-main)))

;; TODO: Bruk liste under til å fjerne helm-versjon på noe -> hvordan få ido eller annen da ? -> defalias ?
;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))

;; TODO: Legg inn keys for å søke bakover også
(use-package uni-search
  :diminish isearch-mode
  :bind
  (("C-f" . lawlist-isearch-forward)
   ("<f3>" . lawlist-isearch-repeat-forward))
  :init
  (progn
    (define-key isearch-mode-map [f3] 'isearch-repeat-forward)
    (define-key minibuffer-local-isearch-map [f3] 'isearch-forward-exit-minibuffer)))

(setq helm-for-files-preferred-list
      '(
        ;; helm-c-source-ido-virtual-buffers ;; Denne virker ikke her
        ;; helm-c-source-recentf
        helm-source-recoll-main
        ))

;; TODO: helm-c-source-tracker-search ? -> finnes i helm som standard

(require 'helm-cmd-t) 
(require 'helm-C-x-b)
(setq helm-ff-lynx-style-map nil helm-input-idle-delay 0.1 helm-idle-delay 0.1)

(setq projects-source (helm-cmd-t-get-create-source-dir "~/Dropbox/projects"))
;; (setq projects-source (helm-cmd-t-get-create-source-dir (concat unimacs-home "/Dropbox/projects/")))
;; (concat unimacs-home "code/dotfiles")
(setq downloads-source (helm-cmd-t-get-create-source-dir "~/Downloads"))
(setq emacs-source (helm-cmd-t-get-create-source-dir "~/.emacs.d"))

;; (setq helm-cmd-t-default-repo (getenv projects-source)) 

;; (global-set-key (kbd "M-t") 'helm-cmd-t)
;; TODO: Denne virker bra for å åpne filer -> kan gjøre std gui-variant tilgjengelig når en skal ha filer utenfor disse
(global-set-key (kbd "M-t") 'uni-helm-cmd-t)
(defun uni-helm-cmd-t ()
  (interactive)
  (helm :sources (list projects-source emacs-source downloads-source)))







(require 'cl)

(defun head (list n)
  "Return a copy of list with the first n elements"
  (butlast list (- (length list) n)))

(defun sort-files-by-date (a b)
  (let ((ta (nth 5 (file-attributes a)))
        (tb (nth 5 (file-attributes b))))
    (if (= (nth 0 ta) (nth 0 tb))
        (> (nth 1 ta) (nth 1 tb))
      (> (nth 0 ta) (nth 0 tb)))))

(defun canonical-file-path (path)
  (file-truename (expand-file-name path)))

;;;###autoload
(defun canonical-file-paths (paths)
  (mapcar 'canonical-file-path paths))

;; removes duplicates - from http://www.emacswiki.org/emacs/timid.el - modified
;;;###autoload
(defun rwd-add-file-hook ()
  "Add the name of the file just opened or written to
`file-name-history'"
  (and buffer-file-name
       (let ((expanded-path (canonical-file-path buffer-file-name)))
         (setq file-name-history (delete expanded-path file-name-history))
         (push expanded-path file-name-history)))
  nil)

;;;###autoload
(defun canonicalize-file-name-history ()
  "Set `file-name-history' to the first `history-length' - 100
elements of `file-name-history' after sorting by mtime,
canonicalizing, removing duplicates and filtering out all TRAMP
paths, directories, backups, and non-existent files."
  (interactive)
  (require 'tramp)
  (let ((rwd-tramp-method-regexp
         (concat "^/" (regexp-opt (mapcar 'car tramp-methods) t) ":")))
    (setq file-name-history
          (head (sort (remove-duplicates
                       (mapcar 'canonical-file-path
                               (remove-if
                                (lambda (path)
                                  (or
                                   (string-match "^/.+::" path)
                                   (string-match rwd-tramp-method-regexp path)
                                   (file-directory-p path)
                                   (string-match "~$" path)
                                   (not (file-exists-p path))))
                                file-name-history)) :test #'equal)
                      'sort-files-by-date)
                (- history-length 100)))))

;;;###autoload
(progn
  ;; TODO: Legg inn at fjernes fra liste når åpnes + skrives til liste i det fil lukkes heller
  ;; --> eller beholde som er og legge inn untitled + dired mm og ha egen farge på de som er åpne til en hver tid ?
  (add-hook 'find-file-hooks 'rwd-add-file-hook)
  (add-hook 'write-file-hooks 'rwd-add-file-hook)
  (add-hook 'savehist-save-hook 'canonicalize-file-name-history))

;;;###autoload
(defun repopulate-file-name-history ()
  (interactive)
  (setq file-name-history (rwd-find-project-files))
  (canonicalize-file-name-history))


;;; start server
(use-package server                     ; Start server to reuse existing emacs-process and frame
  :init
  (unless (server-running-p)
    (server-start)))

;;; provide init package
(provide 'init)

;;; init.el ends here
















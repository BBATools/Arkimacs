;;; init.el --- description -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Package:
(require 'package)

(setq package-archives
      '(("elpa" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ;; ("marmalade" . "https://marmalade-repo.org/packages/")
        ))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  ;; (not free-vars)
  (require 'use-package))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Tabs:
(use-package ark-tabs
  :load-path "elisp"
  :init)

;;; UI:
(use-package ark-ui
  :load-path "elisp"
  :init)

;;; System:
(use-package ark-system
  :load-path "elisp"
  :init)

;;; Search:
(use-package ark-search
  :load-path "elisp"
  :init)

;;; Edit:
(use-package ark-edit
  :load-path "elisp"
  :init)

;;; Movement:
(use-package ark-movement
  :load-path "elisp"
  :init)

;;; Minibuffer:
(use-package ark-minibuffer
  :load-path "elisp"
  :init)

;;; Outlines:
(use-package ark-outline
  :load-path "elisp"
  :init)

;;; Data:
(use-package ark-data
  :load-path "elisp"
  :init)

;;; Code:
(use-package ark-code
  :load-path "elisp"
  :init)

;;; Frame:
(use-package ark-frame
  :load-path "elisp"
  :init)

;;; Window:
(use-package ark-window
  :load-path "elisp"
  :init)


(use-package highlight-indent-guides
  :ensure t
  :hook (python-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-enabled nil)
  (highlight-indent-guides-delay 0.7)
  (highlight-indent-guides-highlighter-function 'ark-highlighter)
  :config
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (defun ark-highlighter (level responsive display)
    (if (> 1 level)
        nil
      (highlight-indent-guides--highlighter-default level responsive display)))
  )


(use-package pretty-hydra
  :ensure t
  :bind ("M-z" . hydra-zoom/body)
  :config
  (pretty-hydra-define hydra-zoom (:exit nil :hint nil)
    ("Zoom Functions:"
     (("<up>    " text-scale-increase "Zoom in")
      ("<down>  " text-scale-decrease "Zoom out")
      ("<escape>" nil "Exit" :exit t)
      ("<return>" nil "" :exit t)))))

(use-package pretty-hydra
  :ensure t
  :bind ("M-f" . hydra-files/body)
  :config
  (pretty-hydra-define hydra-files (:exit t :hint nil)
    ("Files"
     (("r" ark-rename-file-and-buffer "Rename current")
      ("d" ark-delete-file-and-buffer "Delete current")
      ("b" hexl-find-file "Open binary")
      ("l" find-file-literally "Open literally")
      ("<escape>" nil "Exit")
      ("<return>" nil ""))))
  ;; Rename:
  (defun ark-rename-file-and-buffer ()
    "Rename current buffer and if the buffer is visiting a file, rename it too."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))
  ;; Delete:
  (defun ark-delete-file-and-buffer ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (when filename
        (if (vc-backend filename)
            (vc-delete-file filename)
          (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
            (delete-file filename delete-by-moving-to-trash)
            (message "Deleted file %s" filename)
            (kill-buffer))))))
  )





;;; Version Control:


;;; Shell:






;;; Bindings:
;; TODO: Hva er riktigste stedet og ha dolist under for at ikke skal overstyre noe jeg ikke ønsker?
(dolist (key '("\C-h" "\C-j" "\C-k"
               "\M-h" "\M-j"
               "\C-\M-h" "\C-\M-j" "\C-\M-k" "\C-\M-l"
               "\M-u" "\M-i" "\C-u" "\C-i"))
  (global-unset-key key))


(bind-keys*
 ("C-n" . ark-new-untitled-buffer)
 ("TAB" . indent-for-tab-command)
 ;; ("C-w" . ark-kill-this-buffer) ;; TODO: Endre til denne så fikset inkl virker med frame-tabs
 ("C-w" . kill-this-buffer)
 ("C-M-;" . comment-or-uncomment-region)
 ("C-<tab>" . ark-switch-to-previous-buffer)
 ("C-c" . kill-ring-save)
 ("C-x" . kill-region)
 ("C-a" . mark-whole-buffer)
 ;; ("C-q" . ark-delete-frame) ;; TODO: Flytt denne mm til ark-frame
 ;; ("C-q" . make-frame-invisible)
 ("C-q" . ark-delete-frame-or-kill-emacs)
 ;; ("C-q" . delete-frame-or-kill-emacs)
 ("C-s" . save-buffer)
 ("C-v" . yank)
 ("M-c" . comment-line)
 ("M-q" . kill-emacs)
 ("M-s" . write-file)
 ("<s-up>" . windmove-up)
 ("<s-down>" . windmove-down)
 ("<s-right>" . windmove-right)
 ("<s-left>" . windmove-left)
 )
(bind-keys
 ("<escape>" . keyboard-escape-quit)
 )

(defun ark-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun ark-delete-frame ()
  (interactive)
  (progn
    (ark-save-frameg)
    ;; (make-frame-invisible)
    (x-focus-frame nil)
    ;; (delete-frame)
    ))

(defun ark-delete-frame-or-kill-emacs ()
  (interactive)
  (if  (daemonp)
      (progn
        (ark-save-frameg)
        (delete-frame))
    (kill-emacs))) ;; TODO: Kill-emacs spør ikke om lagre når endringer i untitled buffer


;;; Buffers and Files:
;; TODO: Feil ved detektering av "empty" i denne?
;; (defadvice switch-to-buffer (after ark-kill-empty-untitled activate)
;; (let ((-buf (current-buffer)))
;; (let ((temp-buffer-list (ark-buffer-list)))
;; (loop for buf in temp-buffer-list
;; do (if (and (string-prefix-p "untitled" (buffer-name buf))
;; (not (buffer-modified-p buf))
;; (not (buffer-file-name buf))
;; (not (eq buf -buf)))
;; (kill-buffer buf))))))

(use-package find-file-in-repository
  :bind ("C-o" . find-file-in-repository)
  :ensure t)

;; TODO: Fiks så riktig også for siste ikke *-buffer -> feil her eller i ark-buffer list under?
(defun ark-kill-this-buffer ()
  (interactive)
  (let* ((-buf (current-buffer)))
    (let ((temp-buffer-list (ark-buffer-list)))
      (if (> (length temp-buffer-list) 1)
          (progn
            (kill-buffer -buf)
            (switch-to-buffer
             (nth 1 temp-buffer-list)))
        (if (not (string-prefix-p "untitled" (buffer-name (current-buffer))))
            (progn
              (kill-buffer -buf)
              (if (= (length (ark-buffer-list)) 0)
                  (ark-new-untitled-buffer))))))))

;; TODO: Bruke denne fra awesome-tab også?
(defun ark-buffer-list (&optional buffer-list)
  (cl-loop for buf in (or buffer-list (buffer-list (selected-frame)))
           if (not (string-equal "*" (substring (buffer-name buf) 0 1)))
           collect buf))

(defun ark-initial-buffer()
  (let ((-buf (get-buffer-create "untitled")))
    (set-buffer -buf)
    (text-mode)
    (setq buffer-offer-save t)
    (set (make-local-variable 'ark-untitled-buffer) t)
    -buf))

(defun ark-new-untitled-buffer()
  (interactive)
  (let* ((buf (current-buffer)))
    (if (not (and (string-prefix-p "untitled" (buffer-name buf))
                  (not (buffer-modified-p buf))))
        (let ((-buf (generate-new-buffer "untitled")))
          (switch-to-buffer -buf)
          (text-mode)
          (setq buffer-offer-save t)
          (set (make-local-variable 'ark-untitled-buffer) t)
          -buf))))
(defun ark-untitled-buffer-kill-query-function ()
  "Don't kill untitled buffer without asking if modified"
  (if (and (not buffer-file-name)
           (buffer-modified-p)
           (boundp 'ark-untitled-buffer))
      (if 'ark-untitled-buffer
          (yes-or-no-p "Untitled buffer modified. Kill it anyway? "))
    t))
(add-to-list 'kill-buffer-query-functions 'ark-untitled-buffer-kill-query-function)

;; TODO: Se mer her:https://stackoverflow.com/questions/2662655/automatically-closing-the-scratch-buffer/2662728
(defun ark-kill-untitled-buffers ()
  (if (get-buffer "untitled")
      (ark-kill-buffers-with-prefix "untitled")))
;; (add-hook 'find-file-hook 'ark-kill-untitled-buffers) ;; TODO: Virker ikke sammen med ark-untitled-buffer-kill-query-function

;; TODO: Endre så denne er bare for untitled som ikke har blitt modifisert
(defun ark-kill-buffers-with-prefix (prefix)
  "Kill buffers whose names start with the given prefix"
  (interactive "sPrefix to kill: ")
  (loop for buffer in (buffer-list)
        do (if (string-prefix-p prefix (buffer-name buffer))
               (kill-buffer buffer))))

(defun ark-kill-scratch ()
  (kill-buffer "*scratch*"))
(add-hook 'emacs-startup-hook  #'ark-kill-scratch) ;; TODO: Denne virker ikke lenger (eller kun når daemon?)

(use-package recentf
  :custom
  (recentf-save-file (concat cache-dir "/recentf"))
  (recentf-max-saved-items 100)
  (recentf-exclude         '("/tmp/" "/ssh:"))
  :init
  (recentf-mode +1))

(use-package neotree
  :ensure t
  :bind (("<f1>" . neotree-toggle))
  :custom
  (neo-theme 'arrow)
  (neo-smart-open t)
  :hook (neotree-mode-hook . hide-mode-line-mode))

(use-package ediff
  :config
  (defvar ctl-period-equals-map)
  (define-prefix-command 'ctl-period-equals-map)
  (bind-key "C-. =" #'ctl-period-equals-map)

  (setq ediff-diff-options "-w")

  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = c" . compare-windows)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise)))

;;; Completion:
(use-package amx
  :ensure t
  :custom (amx-save-file (concat cache-dir "/amx-items"))
  ;; :init
  ;; (add-to-list 'amx-ignored-command-matchers ;; WAIT: Virker ikke via ivy -> fix
  ;; "\\`ido-display-file\\'")
  ;; :init (amx-mode)
  ;; :config
  ;; (setq amx-save-file (concat cache-dir "/amx-items"))
  )

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("s-o" . counsel-outline)
         )
  :config (counsel-mode))

(use-package ivy
  :ensure t
  :defer 0.1
  :bind
  (
   ("M-o" . ivy-switch-buffer)
   ;; ("C-c C-r" . ivy-resume)
   ;; ("C-x B" . ivy-switch-buffer-other-window)
   (:map ivy-minibuffer-map
         ("<return>" . ivy-alt-done))
   )
  :custom
  (ivy-initial-inputs-alist nil)
  ;; (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (defun d/ignore-dired-buffers (str)
    "Return non-nil if STR names a Dired buffer.
  This function is intended for use with `ivy-ignore-buffers'."
    (let ((buf (get-buffer str)))
      (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))
  (add-to-list 'ivy-ignore-buffers #'d/ignore-dired-buffers)
  (add-to-list 'ivy-ignore-buffers "^\*")
  (ivy-mode))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package posframe
  :ensure t
  :disabled t
  :config (setq posframe-mouse-banish nil)
  )


(use-package ivy-posframe
  :ensure t
  :disabled t
  :after ivy
  ;; TODO: Top center virker ikke etter oppdatering
  :custom   (ivy-display-function #'ivy-posframe-display-at-frame-top-center)
  :init
  (defun ivy-posframe-display-at-frame-top-center (str)
    (ivy-posframe--display str #'posframe-poshandler-frame-top-center))

  ;; (defun ivy-posframe-display-at-frame-top-center (str)
  ;;   (ivy-posframe--display str #'posframe-poshandler-frame-top-center))
  ;; (defun posframe-poshandler-frame-top-center (info)
  ;;   (cons (/ (- (plist-get info :parent-frame-width)
  ;;               (plist-get info :posframe-width))
  ;;            2)
  ;;         (round (* 0.02 (x-display-pixel-height)))))
  ;; (ivy-posframe-enable)
  :config
  (dolist (cmd '(counsel-yank-pop
                 ;; ivy-imenu-anywhere
                 ;; ivy-todo
                 ivy-switch-buffer
                 counsel-find-file
                 counsel-M-x
                 ;; counsel-projectile-switch-to-buffer
                 ))
    (push `(,cmd . ivy-posframe-display) ivy-display-functions-alist))
  )

;; TODO: https://github.com/jixiuf/vterm-toggle
;; TODO: https://github.com/akermu/emacs-libvterm/issues/24
(use-package vterm
  :ensure t
  :commands (vterm vterm-other-window)
  :custom (vterm-install t)
  :bind* (("C-t" . vterm)
          (:map vterm-mode-map
                ("C-z" . vterm-send-ctrl-c) ;; TODO: Virker ikke->https://github.com/akermu/emacs-libvterm/issues/25
                ("C-v" . vterm-yank)
                )))

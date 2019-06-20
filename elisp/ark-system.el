;;; ark-system.el --- description -*- lexical-binding: t; -*-

(use-package emacs
  :custom
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (custom-file (make-temp-file "emacs-custom-"))
  (custom-buffer-done-kill nil)
  (custom-buffer-verbose-help nil)
  (indent-tabs-mode nil)
  (tab-width 2) ;; WAIT: Endre denne?
  (auto-save-default nil)
  (initial-major-mode 'text-mode)
  (auto-save-list-file-prefix nil)
  (delete-by-moving-to-trash t)
  (backup-by-copying t)
  (make-backup-files nil)
  (major-mode 'text-mode)
  (bidi-display-reordering nil)
  (jit-lock-defer-time 0.05)
  (jit-lock-context-time 0.5)
  (jit-lock-chunk-size 10000)
  (jit-lock-stealth-nice 0.1)
  (jit-lock-stealth-time 0.5)
  (jit-lock-stealth-load nil)
  (create-lockfiles nil)
  (initial-buffer-choice (lambda () (ark-initial-buffer)))
  (custom-safe-themes t)
  (make-pointer-invisible t)
  (enable-local-variables t)
  (large-file-warning-threshold (* 50 1024 1024)) ;; 50 mb
  (load-prefer-newer t)
  (require-final-newline t)
  (confirm-kill-emacs nil)
  (confirm-kill-processes nil)
  (echo-keystrokes 0.2)
  (ring-bell-function #'ignore)
  (set-mark-command-repeat-pop t)
  (inhibit-default-init t)
  (inhibit-startup-screen t)
  (redisplay-dont-pause t)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (mouse-wheel-progressive-speed nil)
  (fast-but-imprecise-scrolling t)
  (scroll-conservatively 10000)
  (scroll-step 1)
  (scroll-preserve-screen-position nil)
  :config
  (defconst cache-dir (concat user-emacs-directory "cache/"))
  (setq-default nsm-settings-file (concat cache-dir "/network-security.data")) ;; Doesn't work in custom
  (put 'var 'safe-local-variable (lambda [& rest] true))
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  (dolist (r `((?i . (file . ,(expand-file-name "init.el" user-emacs-directory)))))
    (set-register (car r) (cdr r)))
  )

(use-package server
  :config
  (unless (server-running-p) (server-start)))


(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "ZSH" "MANPATH"
                                    "SSH_AUTH_SOCK"
                                    "SSH_AGENT_PID"
                                    "GPG_AGENT_INFO"
                                    "RUST_SRC_PATH"
                                    "GNOME_KEYRING_CONTROL"
                                    "GNOME_KEYRING_PID"))
  :config
  (exec-path-from-shell-initialize))


(provide 'ark-system)

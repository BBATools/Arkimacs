;;; config.el --- description -*- lexical-binding: t; -*-

;; (setq debug-on-error t)


(load! "+ui")
(load! "+completion")
(load! "+python")
(load! "+ivy")
(load! "+menus")
(load! "+mouse")
(load! "+shell")
(load! "+edit")
(load! "+bindings")
(load! "+modeline")

(setq projectile-require-project-root nil)

;; Load packages at startup
;; (doom-load-packages-incrementally
;; '(nose pip-requirements pythonic anaconda-mode company-anaconda))

(require 'server)
(unless (server-running-p)
  (server-start))

(setq default-directory "~/Projects/")

;;(set-default bidi-paragraph-direction nil)

;(bury-buffer "*scratch*")
;; (ark-new-untitled-buffer)     ; Create untitled buffer on startup

;; (setq tramp-gvfs-enabled nil)
;; (setq shell-prompt-pattern '"^[^#$%>\n]*~?[#$%>] *")

;; TODO: Div testing under for å fjerne treghet med win-share

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(setq remote-file-name-inhibit-cache nil)
(setq tramp-completion-reread-directory-timeout nil)
;; (setq projectile-mode nil)

(defvar disable-tramp-backups '(all))

(eval-after-load "tramp"
  '(progn
     ;; Modified from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
     (setq tramp-verbose 6)
     (setq backup-enable-predicate
           (lambda (name)
             (and (normal-backup-enable-predicate name)
              ;; Disable all tramp backups
              (and disable-tramp-backups
                   (member 'all disable-tramp-backups)
                   (not (file-remote-p name 'method)))
              (not ;; disable backup for tramp with the listed methods
               (let ((method (file-remote-p name 'method)))
                 (when (stringp method)
                   (member method disable-tramp-backups)))))))

     (defun tramp-set-auto-save--check (original)
       (if (funcall backup-enable-predicate (buffer-file-name))
           (funcall original)
         (auto-save-mode -1)))

     (advice-add 'tramp-set-auto-save :around #'tramp-set-auto-save--check)
     (setq tramp-ssh-controlmaster-options "")
     )
  )


(defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!

;;; ark-minibuffer.el --- description -*- lexical-binding: t; -*-

;; TODO: Mulig å justere så linjenummer er nede til venstre?
(use-package awesome-tray
  :straight (:host github :repo "manateelazycat/awesome-tray" :files ("*.el"))
  ;; :after doom-themes
  :init
  (setq awesome-tray-active-modules '("buffer-name" "mode-name" "location"))
  :config
  ;; TODO: Får ikke til å sette farge til en variabel heller enn hardkoding -> fiks
  ;; (set-face-foreground 'awesome-tray-module-mode-name-face modeline-fg)
  ;; (set-face-foreground 'awesome-tray-module-mode-name-face (face-attribute 'numbers :foreground))
  (defun awesome-tray-module-location-info ()
    (format "%s:%s" (string-to-number (format-mode-line "%l")) (current-column)))
  (defun awesome-tray-module-buffer-name-info ()
    (let ((home (getenv "HOME")))
      (format "%s%s%s"
              (if (buffer-file-name)
                  (replace-regexp-in-string (concat "^" home) "~" default-directory)
                "")
              (if (buffer-file-name)
                  (file-name-nondirectory (buffer-file-name))
                (buffer-name))
              (if (and (buffer-file-name) (buffer-modified-p))
                  "*" ""))))
  ;; (awesome-tray-mode 1)
  )

(defun suppress-messages (old-fun &rest args)
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply old-fun args)
      (advice-remove 'message #'silence))))

;; TODO: Legg til flere samt fjern duplisering i kode
;; TODO: Legg til for "mark saved..."
(advice-add 'end-of-buffer :around #'suppress-messages)
(advice-add 'beginning-of-buffer :around #'suppress-messages)
(advice-add 'ivy-posframe-enable :around #'suppress-messages)
(advice-add 'server-execute :around #'suppress-messages)
(advice-add 'save-buffer :around #'suppress-messages)

(defadvice previous-line (around silencer activate)
  (condition-case nil
      ad-do-it
    ((beginning-of-buffer))))

(defadvice next-line (around silencer activate)
  (condition-case nil
      ad-do-it
    ((end-of-buffer))))

(provide 'ark-minibuffer)

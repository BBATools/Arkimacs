;;; ark-frame.el --- description -*- lexical-binding: t; -*-


(use-package emacs
  :custom
  (icon-title-format '("" "Arkimacs "))
  ;; (frame-title-format '("" "<1@Arkimacs> "))
  (frame-title-format
   '( "[1] " (:eval (if (buffer-file-name)
                        (abbreviate-file-name (buffer-file-name))
                      "%b"))))
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  )



(defun ark-save-frameg ()
  "Gets the current frame's geometry and saves to ~/.Xresources"
  (interactive)
  (let ((xfile "~/.Xresources")
        (frameg-width (frame-parameter (selected-frame) 'width))
        (frameg-height (frame-parameter (selected-frame) 'height)))
    (progn
      (with-temp-buffer (insert-file-contents xfile)
                        (goto-char (point-min))
                        (while (not (eobp))
                          ;; TODO: Endre så ikke sletter alt annet, men kun linjen som starter med "emacs.geometry:"
                          (delete-region (line-beginning-position)
                                         (line-end-position))
                          (forward-line 1))
                        (write-region 1 (point-max) xfile))
      (write-region (format "%s%s%s%s" "emacs.geometry: " frameg-width "x" frameg-height) nil xfile 'append)
      (shell-command "xrdb -merge ~/.Xresources"))))

(add-hook 'kill-emacs-hook #'ark-save-frameg)

(defadvice delete-frame (before ark-save-geometry activate)
  (ark-save-frameg))


;; TODO: Endre heller slik at amacs-script leser xresources og bruker det som arg til emacsclient
;; TODO: Endre så høyde og bredde leses fra xresources-fil
;; emacs-command uses xresources while emacsclient uses this:
(add-hook 'before-make-frame-hook
          #'(lambda ()
              ;; (let ((res-geometry (x-get-resource "geometry" "Geometry")))
              ;;   (if res-geometry
              ;;       (setq default-frame-alist (append default-frame-alist
              ;;                                         (x-parse-geometry res-geometry)))))
              ;; (message "test")
              (let ((res-geometry (x-get-resource "geometry" "Geometry"))
                    parsed)
                ;; (message res-geometry)
                ;; (message (substring res-geometry 0 -3))
                (if res-geometry
                    ;; (let ((geo-list (split-string "xy_007_cat" "_")
                    (add-to-list 'default-frame-alist '(width  . 100))
                  ;; (add-to-list 'default-frame-alist '(width  . (string-to-number (substring res-geometry 0 -3))))
                  ;; (add-to-list 'default-frame-alist '(width  . (replace-regexp-in-string "\\x*" "" res-geometry)))
                  (add-to-list 'default-frame-alist '(height . 50))
                  ))))
;; ))




(provide 'ark-frame)

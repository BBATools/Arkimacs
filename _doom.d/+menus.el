;;; +menus.el --- description -*- lexical-binding: t; -*-

;; TODO: Lag ny menu item for recentf som åpner ivy-versjon

;;; recentf:
(with-eval-after-load 'recentf
  (recentf-hide-menu)
  (setq recentf-max-menu-items 15))

;;; File-menu:
(define-key menu-bar-file-menu [new-file]
  `(menu-item ,(purecopy "New Buffer") ark-new-untitled-buffer
	      :keys "Ctrl+N"
	      :help ,(purecopy "New empty buffer")))
(define-key menu-bar-file-menu [open-file]
  `(menu-item ,(purecopy "Open File...") find-file
	      :keys "Ctrl+O"
	      :help ,(purecopy "Open existing file")))
(define-key-after global-map [menu-bar file recent-buffer]
  '(menu-item "Open Recent..." ivy-switch-buffer
              :keys "Ctrl+Shift+O"
              :help "Open recent file or buffer")
  'open-file) ;; Add after open-file menu-item
(define-key menu-bar-file-menu [kill-buffer]
  `(menu-item ,(purecopy "Close") ark-kill-this-buffer
	      :keys "Ctrl+W"
	      :help ,(purecopy "Close current buffer")))
(define-key menu-bar-file-menu [save-buffer]
  `(menu-item ,(purecopy "Save File") save-buffer
	      :enable (and (buffer-modified-p)
			   (buffer-file-name)
			   (menu-bar-non-minibuffer-window-p))
	      :keys "Ctrl+S"
	      :help ,(purecopy "Save current file")))
(define-key menu-bar-file-menu [write-file]
  `(menu-item ,(purecopy "Save As...") write-file
	      :enable (and (menu-bar-menu-frame-live-and-visible-p)
			   (menu-bar-non-minibuffer-window-p))
	      :keys "Ctrl+Shift+S"
	      :help ,(purecopy "Save as a new file")))
(define-key menu-bar-file-menu [revert-buffer] nil)
(define-key menu-bar-file-menu [dired] nil)
(define-key menu-bar-file-menu [insert-file] nil)
(define-key menu-bar-file-menu [ps-print-buffer-faces] nil)
(define-key menu-bar-file-menu [print-region] nil)
(define-key menu-bar-file-menu [ps-print-region-faces] nil)
(define-key menu-bar-file-menu [ps-print-buffer] nil)
(define-key menu-bar-file-menu [ps-print-region] nil)
(define-key menu-bar-file-menu [make-frame] nil)
(define-key menu-bar-file-menu [make-frame-on-display] nil)
(define-key menu-bar-file-menu [delete-this-frame] nil)
(define-key menu-bar-file-menu [separator-exit] nil)
(define-key menu-bar-file-menu [new-window-below]
      '(menu-item "New Window Below" split-window-below
                  :enable (and (menu-bar-menu-frame-live-and-visible-p)
                               (menu-bar-non-minibuffer-window-p))
		  :keys "Ctrl+Win+-"
                  :help "Make new window below selected one"))
(define-key menu-bar-file-menu [new-window-on-right]
      '(menu-item "New Window on Right" split-window-right
                  :enable (and (menu-bar-menu-frame-live-and-visible-p)
                               (menu-bar-non-minibuffer-window-p))
		   :keys "Ctrl+Win+I"
                   :help "Make new window on right of selected one"))
(define-key menu-bar-file-menu [one-window]
      '(menu-item "Toggle Window Split" toggle-window-split ;;TODO: Endre til annen defun når laget bedre
		  :keys "Ctrl+Win+Enter"
                  :help "Toggle between one window and split of windows"))

(provide '+menus)
;;; +menus.el ends here

;;; +edit.el --- description -*- lexical-binding: t; -*-

(electric-indent-mode 1)
(delete-selection-mode)
(setq select-enable-primary nil)

(after! undo-tree
  (setq undo-tree-auto-save-history nil)) ;Unsafe and confusing

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

;; TODO: Denne er installert: https://github.com/remyferre/comment-dwim-2/blob/master/comment-dwim-2.el
;; ---> se om kan bruke deler derfra (er ikke god nok til å bare bruke den helt heller)
;; TODO: Flytt til autoloads når fornøyd med den
;;; ark-comment-or-uncomment-region-or-line
(defun ark-comment-or-uncomment-region-or-line ()
  (interactive)
  (if (derived-mode-p 'prog-mode 'nxml-mode)
      (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
          (setq beg (line-beginning-position) end (line-end-position)))
        (if (and (not (region-active-p))
                 (= (current-indentation) (- (line-beginning-position) (line-end-position))))
            (comment-dwim nil)
          (comment-or-uncomment-region beg end)
          (next-line)))))

;;; Autosave:
(setq auto-save-default nil) ; Disable built-in
(super-save-mode +1)         ; Enable better auto-save

(def-package! hungry-delete
  :config (global-hungry-delete-mode))


(defun private/super-save/config-hook ()
  (require 'super-save)
  (add-to-list 'super-save-triggers #'evil-window-next)
  (super-save-mode +1)
  (setq super-save-remote-files nil)
  (setq auto-save-default nil))
(add-hook 'after-init-hook 'private/super-save/config-hook)

(provide '+edit)
;;; +edit.el ends here

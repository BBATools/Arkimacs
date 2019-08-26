;;; custom.el --- description -*- no-byte-compile: t; -*-

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun ark-move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
	    (exchange-point-and-mark))
    (let ((column (current-column))
 	      (text (delete-and-extract-region (point) (mark))))
	  (forward-line arg)
	  (move-to-column column t)
	  (set-mark (point))
	  (insert text)
	  (exchange-point-and-mark)
	  (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
	  (forward-line)
	  (when (or (< arg 0) (not (eobp)))
	    (transpose-lines arg))
	  (forward-line -1)))))

;;;###autoload
(defun ark-move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
 arg lines down."
  (interactive "*p")
  (ark-move-text-internal arg))

;;;###autoload
(defun ark-move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
 arg lines up."
  (interactive "*p")
  (ark-move-text-internal (- arg)))

;;;###autoload
(defun ark-shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

;;;###autoload
(defun ark-shift-right (count)
  (interactive "p")
  (ark-shift-text count))

;;;###autoload
(defun ark-shift-left (count)
  (interactive "p")
  (ark-shift-text (- count)))

;;;###autoload
(defun d/ignore-dired-buffers (str)
  "Return non-nil if STR names a Dired buffer.
This function is intended for use with `ivy-ignore-buffers'."
  (let ((buf (get-buffer str)))
    (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))

;;;###autoload
(defun ark-buffer-list (&optional buffer-list)
  (cl-loop for buf in (or buffer-list (buffer-list (selected-frame)))
           if (or (doom-real-buffer-p buf) (string-prefix-p "untitled" (buffer-name buf)))
           collect buf))

;;;###autoload
(defun ark-frame-tabs-buffer-list (frame)
  (ark-buffer-list))

;;;###autoload
(defun ark-bury-buffer (&optional n)
  (interactive)
  (unless n
    (setq n 1))
    (let ((temp-buffer-list (ark-buffer-list)))
      (if (> (length temp-buffer-list) 1)
          (switch-to-buffer
           (if (< n 0)
               (nth (+ (length temp-buffer-list) n) ;; C-left (unbury) n=-1
                    temp-buffer-list)
             (bury-buffer) ;; C-right (bury) n=1
             (nth n temp-buffer-list))))))

;;;###autoload
(defun ark-unbury-buffer ()
  (interactive)
  (ark-bury-buffer -1))

;;;###autoload
(defun ark-swiper ()
  (interactive)
  (if (and (buffer-file-name)
           (not (ignore-errors
                  (file-remote-p (buffer-file-name))))
           (if (eq major-mode 'org-mode)
               (> (buffer-size) 600000)
             (> (buffer-size) 3000000)))
      (progn
        (save-buffer)
        (counsel-grep))
    (swiper--ivy (swiper--candidates))))

;;;###autoload
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
          (ark-kill-this-buffer))))))

(provide 'custom)
;;; packages.el ends here

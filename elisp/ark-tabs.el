;;; ark-tabs.el --- description -*- lexical-binding: t; -*-

;; TODO: Må justere denne koden:
;; (defcustom frame-tabs-buffer-list 'frame-tabs-default-buffer-list
;;   "Function for returning a buffer list for frame tabs.
;; This is a function that takes one argument - a frame - and
;; returns a buffer list for that frame.  The default is to call
;; `buffer-list' for that frame which means to return the frame's
;; local buffer list.  Customizing this option allows, for example,
;; to return the fundamental buffer list or a list of buffer in
;; alphabetical order of their names instead."
;;   :type 'function
;;   :version "26.1"
;;   :group 'frame-tabs)


(use-package eyebrowse
  :ensure t
  :diminish eyebrowse-mode
  :custom (eyebrowse-new-workspace t)
  :config (eyebrowse-mode))


(add-to-list 'default-frame-alist '(close-inner-windows . nil))

(defun my/close-frame-function(frame)
  (if (frame-parameter nil 'close-inner-windows)
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (< (length (get-buffer-window-list buffer nil t)) 2)
            (kill-buffer buffer))))))

(add-hook 'delete-frame-functions 'my/close-frame-function)

(defun my/set-eyebrowse-frame-title()
  (interactive)
  (let*
      ((window-configs (eyebrowse--get 'window-configs))
       (current-config (assq (eyebrowse--get 'current-slot) window-configs))
       (window-tag (car (last current-config)))
       title
       (index (-elem-index (assq (eyebrowse--get 'current-slot) window-configs) window-configs))
       (curr-count (car (nth index window-configs)))
       (first-count (caar window-configs))
       (last-count (caar (last window-configs))))
    (if (= (length window-tag) 0)
        (setq title (concat (number-to-string (car current-config)) "@Arkimacs"))
      (setq title (concat (car (last current-config)) "@Arkimacs")))
    (if (equal curr-count first-count)
        (set-frame-name (concat title " >"))
      (if (equal curr-count last-count)
          (set-frame-name (concat "< " title))
        (set-frame-name (concat "< " title " >")))))
  (force-mode-line-update))

(add-hook 'eyebrowse-post-window-switch-hook 'my/set-eyebrowse-frame-title)


(defun my/move-ws-left()
  (interactive)
  (let*
      ((window-configs (eyebrowse--get 'window-configs))
       (index (-elem-index (assq (eyebrowse--get 'current-slot) window-configs) window-configs))
       (curr-count (car (nth index window-configs)))
       (first-count (caar window-configs)))
    (if (equal curr-count first-count)
        (eyebrowse-switch-to-window-config (1- first-count))
      (eyebrowse-prev-window-config nil))))

(defun rename-ws(tag)
  (interactive "MWorkspace Name: ")
  (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) tag)
  (my/set-eyebrowse-frame-title))

(defun my/move-ws-right ()
  (interactive)
  (let*
      ((window-configs (eyebrowse--get 'window-configs))
       (index (-elem-index (assq (eyebrowse--get 'current-slot) window-configs) window-configs))
       (curr-count (car (nth index window-configs)))
       (last-count (caar (last window-configs))))
    (if (equal curr-count last-count)
        (eyebrowse-switch-to-window-config (1+ last-count))
      (eyebrowse-next-window-config nil))))

(define-key eyebrowse-mode-map (kbd "M-<left>") 'my/move-ws-left)
(define-key eyebrowse-mode-map (kbd "M-<right>") 'my/move-ws-right)










;; TODO: Test igjen senere: https://github.com/matthias-margush/spacebar
(use-package spacebar
  :load-path "elisp"
  :disabled t
  :after eyebrowse
  :config
  (spacebar-mode)
  :init)


;; TODO: Test å endre til å bruke buffer-list som definert i nerd-tab heller: https://github.com/casouri/nerdtab/blob/master/nerdtab.el
(use-package frame-tabs
  ;; :disabled t
  :load-path "elisp"
  :bind (("C-<right>" . ark-bury-buffer)
         ("C-<left>" . ark-unbury-buffer))
  :custom
  (frame-tabs-buffer-list 'ark-frame-tabs-buffer-list)
  (frame-tabs-delay 0.1) ;; Doesn't work with a value of zero
  :init
  (defun ark-bury-buffer (&optional n)
    (interactive)
    (unless n
      (setq n 1))
    (let ((temp-buffer-list (awesome-tab-buffer-list)))
      (if (> (length temp-buffer-list) 1)
          (switch-to-buffer
           (if (< n 0)
               (nth (+ (length temp-buffer-list) n) ;; C-left (unbury) n=-1
                    temp-buffer-list)
             (bury-buffer) ;; C-right (bury) n=1
             (nth n temp-buffer-list))))))

  (defun ark-unbury-buffer ()
    (interactive)
    (ark-bury-buffer -1))

  (face-spec-set
   'frame-tabs-buffer-tab
   '((t :inherit variable-pitch
        :box (:line-width 2 :color "#2F3841") ;; TODO: Endre denne til vanlige buffere sin bakgrunn
        ;; :box (:line-width 2 :color "#3B4252")
        ;; :foreground "#DEDFB5"
        :foreground "grey50"
        ;; :foreground (face-attribute 'tooltip  :background)
        ;; :background (face-attribute 'bg  :background)
        ;; :background modeline-bg ;;WAIT: Hvorfor virker ikke denne (virker i theme-kode)
        ;; :background "#3B4252"
        :background "#2F3841"
        ))
   'face-defface-spec)

  (face-spec-set
   'frame-tabs-selected-tab
   '((t :inherit frame-tabs-buffer-tab
        :foreground "white"
        ;; :foreground "#E1B269"
        :background "#343D46"
        ))
   'face-defface-spec)

  (face-spec-set
   'frame-tabs-higlight-tab
   '((t :inherit frame-tabs-buffer-tab
        :foreground "white"
        :background "#434C5E")
     ) ;; TODO: Endre denne til vanlige buffere sin bakgrunn
   'face-defface-spec)

  ;; (require 'awesome-tab)
  (defun awesome-tab-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))

  (defvar awesome-tab-hide-tab-function 'awesome-tab-hide-tab
    "Function to hide tab.
This fucntion accepet tab name, tab will hide if this function return ni.")

  (defun awesome-tab-filter-out (condp lst)
    (delq nil
          (mapcar (lambda (x) (if (funcall condp x) nil x)) lst)))

  (defvar awesome-tab-hide-hash (make-hash-table :test 'equal))
  (defun awesome-tab-hide-tab-cached (buf)
    (let ((hide (gethash buf awesome-tab-hide-hash 'not-found)))
      (when (eq hide 'not-found)
        (setq hide (funcall awesome-tab-hide-tab-function buf))
        (puthash buf hide awesome-tab-hide-hash)
        )
      hide))

  (defun awesome-tab-buffer-list ()
    "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a *, when they are not
visiting a file.  The current buffer is always included."
    (awesome-tab-filter-out
     'awesome-tab-hide-tab-cached
     (delq nil
           (mapcar #'(lambda (b)
                       (cond
                        ;; Always include the current buffer.
                        ((eq (current-buffer) b) b)
                        ((buffer-file-name b) b)
                        ((char-equal ?*  (aref (buffer-name b) 0)) nil)
                        ((buffer-live-p b) b)))
                   (buffer-list)))))

  ;; (setq frame-tabs-min-size 0.5) ;; Virker ikke

  ;; (defun ark-frame-tabs-buffer-list (frame)
  ;; (nswbuff-projectile-buffer-list
  ;; ))
  ;; (defun ark-buffer-list (&optional buffer-list)
  ;; (cl-loop for buf in nswbuff-projectile-buffer-list
  ;; collect buf))

  (defun ark-frame-tabs-buffer-list (frame)
    (awesome-tab-buffer-list))


  (frame-tabs-mode)

  ;; (setq frame-tabs-buffer-list 'ark-frame-tabs-buffer-list)
  ;; :config (setq frame-tabs-min-size 0.5)
  )

;; (use-package focus-on-editable-buffers
;; :load-path "elisp"
;; :bind (("C-<right>" . foeb/switch-to-next-buffer)
;; ("C-<left>" . foeb/switch-to-prev-buffer))
;; )











(provide 'ark-tabs)

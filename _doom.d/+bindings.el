;;; +bindings.el -*- lexical-binding: t; -*-

;; WAIT: Ctrl UP og down for å bevege mellom outlines?

;; TODO: Se her: https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/%2Bemacs-bindings.el

;; https://stackoverflow.com/questions/1024374/how-can-i-make-c-p-an-emacs-prefix-key-for-develperlysense
;; https://emacs.stackexchange.com/questions/707/how-to-use-global-prefix-command-key-in-define-mode-map-keybinding
;; https://www.reddit.com/r/emacs/comments/3ytb6n/a_better_way_to_define_a_new_prefix/
;; http://ergoemacs.org/emacs/emacs_keybinding_power_of_keys_sequence.html
;; https://www.emacswiki.org/emacs/PrefixKey


;; TODO: Aktiver linjen under når lagt inn litt flere keys for Ivy, M-x mm
;;(use-global-map (make-sparse-keymap))



;; Force override:
(bind-key* (kbd "C-c") 'kill-ring-save)
(bind-key* (kbd "C-x") 'kill-region)
(bind-key* (kbd "M-c") 'ark-comment-or-uncomment-region-or-line)

;; TODO: C-S virket bare med rene bokstaver som siste når i map!
(global-set-key [C-S-left] 'awesome-tab-move-current-tab-to-left)
(global-set-key [C-S-next] 'awesome-tab-move-current-tab-to-right)

;;TODO: Trengerindent-line-or-region (indent-region-line-by-line?)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; (global-set-key [(super b)] 'backward-word)

(map!
 "TAB"                         'indent-for-tab-command
 "RET"                         'newline-and-indent
 ;; Ctrl:
 "C-a"                         'mark-whole-buffer
 "C-f"                         'ark-swiper
 "C-d"                         'ark-delete-file-and-buffer
 "C-g"                         'goto-line
 ;; "C-i"                         'indent-region
 "C-l"                         'downcase-dwim
 "C-n"                         'ark-new-untitled-buffer
 "C-o"                         'find-file
 "C-q"                         'delete-frame
 "C-r"                         'crux-rename-file-and-buffer
 "C-s"                         'save-buffer
 "<C-tab>"                     'ark-switch-to-previous-buffer
 "C-u"                         'upcase-dwim
 "C-v"                         'yank
 "C-w"                         'ark-kill-this-buffer ;TODO: Denne virker ikke på untitled etter at daemon ble tatt i bruk
 "C-y"                         'redo
 "C-z"                         'undo
                                        ;"<C-right>"                   'ark-bury-buffer
                                        ;"<C-left>"                    'ark-unbury-buffer
 "<C-right>"                   'awesome-tab-forward-tab
 "<C-left>"                    'awesome-tab-backward-tab
 "<C-M-up>"                    'mc/mark-previous-like-this
 "<C-M-down>"                  'mc/mark-next-like-this
 "<C-return>"				   'quickrun
 "C-SPC"					   'company-complete
 ;;Ctrl-Win:
 "C-s-i"                       'split-window-horizontally
 "C-s--"                       'split-window-vertically
 "C-s-SPC"                     'hydra-windows/body
 "C-s-<return>"                'zoom-window-zoom
 "C-s-<kp-enter>"              'zoom-window-zoom
 "<C-s-up>"                    'windmove-up  ;; TODO: Endre disse til å endre størrelse på vinduer
 "<C-s-down>"                  'windmove-down
 "<C-s-right>"                 'windmove-right
 "<C-s-left>"                  'windmove-left
 ;;Ctrl-Shift:
 "C-S-f"                       'swiper-all
;;"C-S-right" 'awesome-tab-move-current-tab-to-left
;;"C-S-prior"                       'awesome-tab-move-current-tab-to-left
 ;; "C-S-n"                       'what? -> se global sett oppe ;; TODO: Sett til ny kommando for ny fil av spesiell type (template?)
 "C-S-o"                       'ivy-switch-buffer
 "C-S-q"                       'save-buffers-kill-emacs ;Kills daemon as well?
 "C-S-s"                       'write-file
 "C-S-v"                       'counsel-yank-pop
 ;;Win:
 ;; "s-b"                         'backward-word ;; TODO: Virker bare når trykker b to ganger
 ;;Alt:
 "M-b"                         'counsel-bookmark
 "M-d"                         'crux-duplicate-current-line-or-region
 "M-g"                         'counsel-rg
 "M-i"                         'counsel-imenu
 "M-k"                         'counsel-descbinds
 "M-o"                         'counsel-outline
 "M-u"                         'counsel-unicode-char
 "M-s"                         'eshell
 "M-x"                         'counsel-M-x
 "<M-down>"                    'ark-move-text-down
 "<M-up>"                      'ark-move-text-up
 "<M-right>"                   'ark-shift-right
 "<M-left>"                    'ark-shift-left
 ;;Alt-Shift:
 "M-S-d"                       'crux-duplicate-and-comment-current-line-or-region
 ;;Other:
 "<f11>"                       'toggle-frame-maximized
 "<end>"                       'mwim-end-of-line-or-code
 "<escape>"                    'doom/escape
 "<home>"                      'mwim-beginning-of-code-or-line-or-comment
;;; Swiper keys
 (:after swiper
   (:map swiper-map
     "C-r"                     'swiper-query-replace
     )
   )
 ;;; Comint keys
 (:after comint
   (:map comint-mode-map
     "<up>"                    'comint-previous-input
     "<down>"                  'comint-next-input
     )
   )
 ;;; Company keys
 (:after company
   (:map company-active-map
     "<escape>"                'company-abort
     )
   )
;;; Frame-tab keys
 (:after frame-tabs
   (:map frame-tabs-map
     "<down-mouse-1>"         'frame-tabs-switch-to-buffer
     "<mouse-1>"              'ignore
     "<down-mouse-2>"         'frame-tabs-kill-buffer
     "<mouse-2>"              'ignore
     "<C-down-mouse-1>"       'ignore
     "<C-mouse-1>"            'ignore
     "<M-down-mouse-1>"       'ignore
     "<M-mouse-1>"            'ignore
     "<down-mouse-3>"         'ignore
     "<mouse-3>"              'ignore
     "<C-down-mouse-3>"       'ignore
     "<C-mouse-3>"            'ignore
     "<M-down-mouse-3>"       'ignore
     "<M-mouse-3>"            'ignore
     )
   )
;;; Ivy keys
 (:after ivy
   (:map ivy-switch-buffer-map
     ;; "TAB"                     'ivy-next-line ;; TODO: Endre evt når avklart ctrl-tab
     )
;;; ivy-minibuffer keys
   (:map ivy-minibuffer-map
     "<escape>"                'minibuffer-keyboard-quit
     "RET"                     'ivy-alt-done
     "M-x"                     '(lambda ()
                                  (interactive)
                                  (ivy-quit-and-run
                                    (counsel-M-x)))
     "C-S-o"                   '(lambda ()
                                  (interactive)
                                  (ivy-quit-and-run
                                    (ivy-switch-buffer)))
     "C-o"                     '(lambda ()
                                  (interactive)
                                  (ivy-quit-and-run
                                    (counsel-find-file)))
     "<C-up>"                  'ivy-previous-history-element
     "<C-down>"                'ivy-next-history-element
     )
   )
 )

(defun ark-switch-to-previous-buffer ()
  "Switch to previously open buffer"
  (interactive)
  (let ((temp-buffer-list (ark-buffer-list)))
    (switch-to-buffer
     (nth (+ (length temp-buffer-list) -1) ;; C-left (unbury) n=-1
          temp-buffer-list))
    ;; ))
    (switch-to-buffer (nth -1 temp-buffer-list))))
;; (switch-to-buffer (other-buffer (current-buffer) 1)))

(defhydra hydra-windows ()
  "C-arrow = switch, S-arrow = size, M-arrow = move"
  ("C-<left>" windmove-left nil)
  ("C-<right>" windmove-right nil)
  ("C-<up>" windmove-up nil)
  ("C-<down>" windmove-down nil)
  ("S-<left>" hydra-move-splitter-left nil)
  ("S-<right>" hydra-move-splitter-right  nil)
  ("S-<up>" hydra-move-splitter-up nil)
  ("S-<down>" hydra-move-splitter-down nil)
  ("M-<left>" buf-move-left nil)
  ("M-<right>" buf-move-right nil)
  ("M-<up>" buf-move-up nil)
  ("M-<down>" buf-move-down nil)
  ("p" previous-buffer "prev-buf")
  ("n" next-buffer "next-buf")
  ("1" delete-other-windows "1")
  ("d" delete-window "del")
  ("k" kill-buffer "kill")
  ("s" save-buffer "save")
  ("u" (progn (winner-undo) (setq this-command 'winner-undo)) "undo")
  ("r" winner-redo "redo")
  ("b" helm-mini "helm-mini" :exit t)
  ("f" helm-find-files "helm-find" :exit t)
  ("|" (lambda () (interactive) (split-window-right) (windmove-right)))
  ("_" (lambda () (interactive) (split-window-below) (windmove-down)))
  ("q" nil "cancel")
  )

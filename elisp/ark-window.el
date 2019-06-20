;;; ark-window.el --- description -*- lexical-binding: t; -*-

;; TODO: Right og below heller enn V og H?
;; TODO: Zygosphere virker ikke som toggle når kalt fra hydra -> eyebrowse heller?
(use-package pretty-hydra
  :ensure t
  :bind ("M-w" . hydra-window/body)
  :config
  (pretty-hydra-define hydra-window (:exit nil :hint nil)
    ("Windows Functions:"
     (("<right>    " enlarge-window-horizontally "Enlarge horizontally")
      ("<left>     " shrink-window-horizontally "Shrink horizontally")
      ("<down>     " enlarge-window "Enlarge vertically")
      ("<up>       " shrink-window "Shrink vertically")
      ("b          " balance-windows "Balance windows")
      ("h          " split-window-horizontally "Split horizontally")
      ("v          " split-window-vertically "Split vertically")
      ("<return>   " zygospore-toggle-delete-other-windows "Toggle split" :exit t)
      ("<backspace>" crux-swap-windows "Swap windows")
      ("<escape>   " nil "Exit" :exit t))))
  )

(use-package zygospore
  :ensure t)

(provide 'ark-window)

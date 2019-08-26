;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! restart-emacs :disable t)

;; TODO: Linje under virker ikke for disable?
(package! helm :disable t)
;; (package! projectile :disable t)

(package! bar-cursor)
(package! yascroll)
(package! frame-tabs)
(package! zoom-window)
(package! mwim)
(package! super-save)
(package! crux)
(package! highlight-indent-guides)
(package! awesome-tab :recipe (:fetcher github :repo "manateelazycat/awesome-tab" :files ("awesome-tab.el")))
(package! live-py-mode)
(package! hungry-delete)
(package! super-save)

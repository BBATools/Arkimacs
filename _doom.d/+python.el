;;; ~/.doom.d/+python.el -*- lexical-binding: t; -*-

;; For anaconda server when behind corporate proxy:
(setenv "no_proxy" "127.0.0.1,localhost")

;;(after! python
;;   (setq python-shell-interpreter (executable-find "python3")))

;;(after! anaconda-mode
;;   (setq python-shell-interpreter (executable-find "python3")))

(def-package! live-py-mode 
	:commands live-py-mode)


(provide '+python)
;;; +python.el ends here

;;; ark-data.el --- description -*- lexical-binding: t; -*-

(use-package nhexl-mode
  :ensure t)

;; TODO: For treg på store filer -> ha hva som std for tsv? Forbedret tsv-mode (se under) ?
;; (use-package csv-mode
;;   :ensure t
;;   :mode ("\\.csv\\'" "\\.tsv\\'")
;;   :init
;;   ;; (add-hook 'csv-mode-hook #'csv-header-line)
;;   )

;; TODO: Test tsv-mode på nytt men må endres slik at align ikke gjøres auto uansett størrelse på fil
;; -> importere auto til sqlite heller og så åpne med en sql-mode?
(use-package tsv-mode
  :load-path "elisp"
  ;; :mode ("\\.tsv\\'")
  :init
  ;; (add-hook 'csv-mode-hook #'csv-header-line)
  )



;; TODO: Fiks så keys samme for outline som i outshine: https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config (setq-default json-reformat:indent-width 2
                        js-indent-level 2))

;; WAIT: SKriv om denne så kan brukes til å åpne tsv-fil direkte
(defun ein:pytools-pandas-to-ses (dataframe)
  "View pandas_ DataFrame in SES_ (Simple Emacs Spreadsheet).
Open a `ses-mode' buffer and import DataFrame object into it.
SES_ is distributed with Emacs since Emacs 22, so you don't need
to install it if you are using newer Emacs.
.. _pandas: http://pandas.pydata.org
.. _SES: http://www.gnu.org/software/emacs/manual/html_node/ses/index.html"
  (interactive (list (read-from-minibuffer "pandas DataFrame "
                                           (ein:object-at-point))))
  (let ((buffer (get-buffer-create
                 (generate-new-buffer-name "*ein:ses pandas*"))))
    ;; fetch TSV (tab separated values) via stdout
    (ein:kernel-request-stream
     (ein:get-kernel)
     (concat dataframe ".to_csv(__import__('sys').stdout, sep='\\t')")
     (lambda (tsv buffer)
       (with-current-buffer buffer
         (cl-flet ((y-or-n-p
                    (prompt)
                    (if (string-prefix-p "Yank will insert " prompt)
                        t
                      (error "Unexpected prompt: %s" prompt))))
           ;; Import DataFrame as TSV
           (ses-yank-tsf tsv nil))
         ;; Force SES to update (equivalent to run `post-command-hook').
         (ses-command-hook)))
     (list buffer))
    ;; Open `ses-mode' buffer
    (with-current-buffer buffer
      (ses-mode))
    (pop-to-buffer buffer)))


(provide 'ark-data)


;; WAIT: Eget valg for quit emacs-daemon (kill-emacs)?

;; WAIT: Skriv noen ord om pimacs i aboutPimacs.txt
(define-key menu-bar-help-menu [about-pimacs]
  (cons "About Pimacs"
	(lambda ()
	  (interactive)
	  (message (convert-standard-filename
		    ;; (expand-file-name
		    (view-file ;; WAIT: åpne i info-mode eller annet ?
		     (concat pimacs "aboutPimacs.txt"))))))) ;; WAIT: Ikke vis navn på fil

(require 'table) ;; Ha tilgjengelig table-meny under tools
;; (add-hook 'text-mode-hook 'table-recognize) ;; Irriterende melding

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FILE-MENU
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: To valg -> "open from any"/"open with anything" og "open from dir"(åpner dired)
;; plus ett som bruker find-file eller tilsv fra ido ?
(define-key menu-bar-file-menu [open-file] nil) ;; Recent kommer feil sted hvis denne fjernes
(define-key menu-bar-file-menu [make-frame] nil)
(define-key menu-bar-file-menu [split-window] nil)
(define-key menu-bar-file-menu [one-window] nil)
(define-key menu-bar-file-menu [separator-window] nil)
(define-key menu-bar-file-menu [make-frame-on-display] nil)
(define-key menu-bar-file-menu [delete-this-frame] nil)
(define-key menu-bar-file-menu [ps-print-buffer-faces] nil)
(define-key menu-bar-file-menu [print-region] nil)
(define-key menu-bar-file-menu [ps-print-region-faces] nil)
(define-key menu-bar-file-menu [ps-print-buffer] nil)
(define-key menu-bar-file-menu [ps-print-region] nil)
(define-key menu-bar-file-menu [dired] nil)
(define-key menu-bar-file-menu [revert-buffer] nil)
(define-key global-map [menu-bar file insert-file] nil) ;; WAIT: Samme syntax som andre linjer ?
(define-key menu-bar-file-menu [rename-file-and-buffer]
  '(menu-item "Rename..." rename-file-and-buffer ;; TODO: Ha under "New", ikke over. Endre rekkefølge på flere
              ;; :enable (menu-bar-non-minibuffer-window-p)
              :help "Rename file and buffer"))
(define-key menu-bar-file-menu [kill-buffer]
  '(menu-item "Close..." kill-and-switch-to-previous-buffer
              ;; :enable (menu-bar-non-minibuffer-window-p)
	      :keys "Ctrl+W"))
;; TODO: Gjør det på denne måten for alle med key
(define-key menu-bar-file-menu [delete-current-file]
  '(menu-item "Delete..." delete-current-file
              ;; :enable (menu-bar-non-minibuffer-window-p)
	      :keys "Ctrl+D"))
(define-key menu-bar-file-menu [new-file]
  '(menu-item "New..." new-untitled-buffer
              ;; :enable (menu-bar-non-minibuffer-window-p)
	      :keys "Ctrl+N"))
(define-key menu-bar-file-menu [anything-files]
  '(menu-item "Open..." anything-files
              ;; :enable (menu-bar-non-minibuffer-window-p)
	      :keys "Alt+F"))
;; TODO: Bare justere :keys på eksisterende hvordan ?
;; (define-key menu-bar-file-menu [exit-emacs]
;; '(menu-item :keys "Alt+F"))
;; <menu-bar> <file> <exit-emacs> runs the command
;; save-buffers-kill-terminal, which is an interactive compiled Lisp
;; function.
;; It is bound to C-q, <menu-bar> <file> <exit-emacs>.
;; (save-buffers-kill-terminal &optional ARG)

;; TODO: Ta utgangspunk i den under heller ? Den virker
;; Fra: http://ergoemacs.googlecode.com/svn-history/r312/trunk/ergoemacs/init_clean_menus.el
;; File menu
;; (setq menu-bar-file-menu
;; '(keymap
;; (new-file menu-item "New" new-empty-buffer)
;; (make-frame menu-item "New Frame" make-frame-command)
;; (open-file menu-item "Open..." find-file)
;; (kill-buffer menu-item "Close" close-current-buffer)
;; (separator1 menu-item "--")
;; (save-buffer menu-item "Save" save-buffer)
;; (write-file menu-item "Save As..." write-file)
;; (revert-buffer menu-item "Revert" revert-buffer)
;; (separator2 menu-item "--")
;; (separator3 menu-item "--")
;; (print-buffer menu-item "Print" print-buffer)
;; (ps-print-buffer-faces menu-item "Print (font+color)" ps-print-buffer-faces)
;; (separator4 menu-item "--")
;; (split-window menu-item "Split Window"
;; split-window-vertically)
;; (split-window-leftright menu-item "Split Window left/right"
;; split-window-horizontally
;; :keys "Alt+Shift+2")
;; (one-window menu-item "Unsplit Window"
;; delete-other-windows)
;; (separator5 menu-item "--")
;; (exit-emacs menu-item "Quit" save-buffers-kill-emacs :keys "Ctrl+Q")
;; "File"))
;; (define-key global-map [menu-bar file] (cons "File" menu-bar-file-menu))


;;TODO: Trenger støtte for mus for å velge fil
;; --> Endre heller slik at separat valg for open file (ctrl-o) som åpner dired (har prosjektmappe som default)
;; --> open fil --> find-file --> annet valg blir search eller recent
;; (global-set-key [(control o)] 'menu-find-file-existing) ;; Open file (bare beholde hvis får til å virke uten andre popups)
(global-set-key [(control w)] 'kill-and-switch-to-previous-buffer) ;; Close tab/buffer
(global-set-key [(control f4)] 'kill-and-switch-to-previous-buffer) ;; Close tab/buffer
(global-set-key [(control n)] 'new-untitled-buffer) ;; New empty buffer
;;TODO: Test om bedre enn linjen over -> må da endre også menyvalg
;;er det anything-valg som gjør at ikke grafisk åpnet lenger ?

;; TODO: Anything-for-files gir en recentf først --> ha denne som startup ?
;; Fjerne grafisk recentf da?
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDIT-MENU
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key menu-bar-edit-menu [bookmark] nil)
(define-key menu-bar-edit-menu [props] nil)
(define-key menu-bar-edit-menu [fill] nil)
(define-key menu-bar-edit-menu [separator-bookmark] nil)
(define-key menu-bar-edit-menu [paste-from-menu] nil)
(define-key menu-bar-edit-menu [search search-forward] nil)
(define-key menu-bar-edit-menu [search search-backward] nil)
(define-key menu-bar-edit-menu [search re-search-forward] nil)
(define-key menu-bar-edit-menu [search re-search-backward] nil)
(define-key menu-bar-edit-menu [search repeat-search-fwd] nil)
(define-key menu-bar-edit-menu [search repeat-search-back] nil)
(define-key menu-bar-edit-menu [search separator-repeat-search] nil)
(define-key menu-bar-edit-menu [search i-search] nil)
(define-key menu-bar-edit-menu [redo]
  '(menu-item "Redo" redo
	      :enable (menu-bar-non-minibuffer-window-p)
	      :keys "Ctrl+Y"))

(require 'eol-conversion) ;; eol-conversion-menu
;; TODO: language-modes under File som i ergoemacs ?
;; Create language modes menu ;; TODO: Legg inn for sql mm.
(define-key-after global-map [menu-bar edit lang-modes]
  (cons "Language Modes" (make-sparse-keymap "major modes")) 'kill-buffer )
(define-key global-map [menu-bar edit lang-modes bash]
  '("Bash" . sh-mode))
(define-key global-map [menu-bar edit lang-modes tcl]
  '("TCL" . tcl-mode))
(define-key global-map [menu-bar edit lang-modes ruby]
  '("Ruby" . ruby-mode))
(define-key global-map [menu-bar edit lang-modes python]
  '("Python" . python-mode))
(define-key global-map [menu-bar edit lang-modes php]
  '("PHP" . php-mode))
(define-key global-map [menu-bar edit lang-modes perl]
  '("Perl" . cperl-mode))
(define-key global-map [menu-bar edit lang-modes separator1]
  '("--"))
(define-key global-map [menu-bar edit lang-modes haskell]
  '("Haskell" . haskell-mode))
(define-key global-map [menu-bar edit lang-modes ocaml]
  '("OCaml" . tuareg-mode))
(define-key global-map [menu-bar edit lang-modes elisp]
  '("Emacs Lisp" . emacs-lisp-mode))
(define-key global-map [menu-bar edit lang-modes separator2]
  '("--"))
(define-key global-map [menu-bar edit lang-modes latex]
  '("LaTeX" . latex-mode))
(define-key global-map [menu-bar edit lang-modes js]
  '("Javascript" . js2-mode))
(define-key global-map [menu-bar edit lang-modes xml]
  '("XML (xml-mode)" . xml-mode))
(define-key global-map [menu-bar edit lang-modes nxml]
  '("XML (nxml-mode)" . nxml-mode))
(define-key global-map [menu-bar edit lang-modes html]
  '("HTML" . html-mode))
(define-key global-map [menu-bar edit lang-modes htmlhelper]
  '("HTML (html-helper-mode)" . html-helper-mode))
(define-key global-map [menu-bar edit lang-modes css]
  '("CSS" . css-mode))
(define-key global-map [menu-bar edit lang-modes separator3]
  '("--"))
(define-key global-map [menu-bar edit lang-modes java]
  '("Java" . java-mode))
(define-key global-map [menu-bar edit lang-modes c++]
  '("C++" . c++-mode))
(define-key global-map [menu-bar edit lang-modes c]
  '("C" . c-mode))
(define-key global-map [menu-bar edit search isearch-backward-regexp]
  '("Backward regexp" . isearch-backward-regexp))
(define-key global-map [menu-bar edit search isearch-forward-regexp]
  '("Forward regexp" . isearch-forward-regexp))
(define-key global-map [menu-bar edit search isearch-backward]
  '("Backward string" . isearch-backward))
(define-key global-map [menu-bar edit search isearch-forward]
  '("Forward string" . isearch-forward))
(define-key global-map [menu-bar edit search a-occur]
  '("Search with occur" . anything-c-moccur-occur-by-moccur))
(define-key-after global-map [menu-bar edit line-to-top-of-window]
  (cons "Shift line to top" '("Shift line to top" . line-to-top-of-window))'goto)
(easy-menu-add-item nil '("edit")
		    (easy-menu-create-menu "Comments" '(["Toggle region" comment-or-uncomment-region+
							 :active t]
							["Make line" line-comment-banner
							 :active t]
							["Comment and down" comment-and-go-down
							 :active t]
							["Uncomment and up" uncomment-and-go-up
							 :active t])))

(easy-menu-add-item nil '("edit")
		    (easy-menu-create-menu "Change case" '(["Uppercase selection" my-upcase
							    :active t]
							   ["Lowercase selection" my-downcase
							    :active t]
							   ;; ["Capitalize selection" my-capitalize :active t]
							   )))

(easy-menu-add-item nil '("edit")
		    (easy-menu-create-menu "Bookmark" '(["Toggle" bm-toggle
							 :active t]
							["Next" bm-next
							 :active t]
							["Previous" bm-previous
							 :active t]
							["List" anything-bm
							 :active t])))

;; TODO: Hvordan håndtere at heller bruker elisp-format-region-or-buffer når en el-fil ?
;; Når i emacs-lisp-mode vises ikke keys i meny fordi keys da ikke stemmer overens med defun under
(define-key-after global-map [menu-bar edit indent-region-or-buffer]
  (cons "Indent region" '("Indent region" . indent-region-or-buffer
			  ;; :keys "Ctrl+W" ;; Hvordan angi key her ? Se eksempler lenger oppe
			  ))'Indent)
(define-key-after global-map [menu-bar edit untabify-buffer]
  (cons "Remove tabs" '("Remove tabs" . untabify-buffer))'Indent)
;; TODO: Fjern ekstra linjer i search-meny
;; TODO: Endre navn på valget clear til delete under edit
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPTIONS-MENU
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map [menu-bar edit goto go-to-pos] nil)
(define-key menu-bar-options-menu [line-wrapping window-wrap] nil)
;; WAIT: Bytt ut gjenværende valg for wrap av lines med bare menyvalg for toggle-wrap (har funksjon allerede)
(define-key menu-bar-options-menu [highlight-paren-mode] nil)
(define-key menu-bar-options-menu [uniquify] nil)
(define-key menu-bar-options-menu [transient-mark-mode] nil)
(define-key menu-bar-options-menu [cua-mode] nil)
(define-key menu-bar-options-menu [showhide] nil)
(define-key menu-bar-options-menu [debugger-separator] nil)
(define-key menu-bar-options-menu [highlight-separator] nil)
(define-key menu-bar-options-menu [edit-options-separator] nil)
(define-key menu-bar-options-menu [debug-on-error] nil)
(define-key menu-bar-options-menu [debug-on-quit] nil)
(define-key menu-bar-options-menu [mule-separator] nil)
(define-key menu-bar-options-menu [case-fold-search] nil)
(define-key menu-bar-options-menu [save-place] nil)
(define-key menu-bar-options-menu [blink-cursor-mode] nil)
(define-key menu-bar-options-menu [auto-fill-mode] nil)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TOOLS-MENU
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key menu-bar-tools-menu [gnus] nil)
(define-key menu-bar-tools-menu [rmail] nil)
(define-key menu-bar-tools-menu [separator-games] nil)
(define-key menu-bar-tools-menu [grep] nil)
(define-key menu-bar-tools-menu [separator-vc] nil)
(define-key menu-bar-tools-menu [separator-encryption-decryption] nil)
(define-key menu-bar-tools-menu [games] nil)
(define-key menu-bar-tools-menu [simple-calculator] nil)
(define-key menu-bar-tools-menu [compose-mail] nil)
(define-key menu-bar-tools-menu [directory-search] nil)
(define-key menu-bar-tools-menu [encryption-decryption] nil)
(define-key menu-bar-tools-menu [pcl-cvs] nil)
(easy-menu-remove-item global-map '("menu-bar" "tools") "Color Themes")
(easy-menu-add-item nil '("tools")
		    ["Compile" my-compile t])
(easy-menu-add-item nil '("tools")
		    ["Commands" anything-execute-extended-command t])
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDEX-MENU
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map [menu-bar tools a-index]
  '("Show file index" . anything-imenu))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EMACS-LISP-MENU
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key emacs-lisp-mode-map [menu-bar emacs-lisp indent-line] nil)
(define-key emacs-lisp-mode-map [menu-bar emacs-lisp indent-region] nil)
(define-key emacs-lisp-mode-map [menu-bar emacs-lisp comment-region] nil)
(define-key emacs-lisp-mode-map [menu-bar emacs-lisp byte-compile] nil)
;; TODO: Fjern linje som ble igjen øverst i meny etter at de 3 over ble fjernet
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELP-MENU
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key menu-bar-help-menu [sep2] nil)
(define-key menu-bar-help-menu [sep3] nil)
(define-key menu-bar-help-menu [sep4] nil)
(define-key menu-bar-help-menu [find-emacs-packages] nil)
(define-key global-map [menu-bar help-menu external-packages] nil)
(define-key menu-bar-help-menu [emacs-tutorial] nil)
(define-key menu-bar-help-menu [emacs-tutorial-language-specific] nil)
(define-key menu-bar-help-menu [emacs-psychotherapist] nil)
(define-key menu-bar-help-menu [debian-emacs-readme] nil)
(define-key menu-bar-help-menu [debian-emacs-news] nil)
(define-key menu-bar-help-menu [debian-emacs-changelog] nil)
(define-key menu-bar-help-menu [about-gnu-project] nil)
(define-key menu-bar-help-menu [describe-no-warranty] nil)
(define-key menu-bar-help-menu [describe-copying] nil)
(define-key menu-bar-help-menu [about-emacs] nil)
(define-key menu-bar-help-menu [send-emacs-bug-report] nil)
(define-key menu-bar-help-menu [getting-new-versions] nil)

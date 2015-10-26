(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

(if (eq system-type 'darwin)
    (setq system-name (car (split-string system-name "\\."))))


(cond 
 ((string-match "Jasons-MacBook-Air" (system-name)) (setq machine 'mba))
 ((string-match "odysseus" (system-name)) (setq machine 'odysseus))
 ((string-match "giles" (system-name)) (setq machine 'giles))
 ((string-match "mclovin" (system-name)) (setq machine 'mclovin))
 ((string-match "e73dd8c40824ff66d747331937bbec772687961b" (sha1 (system-name)))
  (setq machine 'werklt))
 ((string-match "c0bb8ac4db14b3bc5ed55c4f51d0663399a659de" (sha1 (system-name)))
  (setq machine 'werk))
 ((string-match "1b2cc281d0c8264a9978c6e51f0edfb02f495566" (sha1 (system-name)))
  (setq machine 'apollo))
 (t	(setq machine 'other)))

(when (display-graphic-p)
  (mouse-wheel-mode t)
  (scroll-bar-mode -1)
  (blink-cursor-mode 0)
  (tool-bar-mode 0)
  )

(when (not (display-graphic-p))
  (menu-bar-mode -1))

(when (eq window-system 'w32)
  (menu-bar-mode -1))

(defun open-init-file ()
  (interactive)
  (find-file user-init-file))

(global-font-lock-mode 1)

(add-to-list 'exec-path "/usr/local/bin")

;; truncate lines if they are too long

;; trucate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)

;; inhibit startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; get intermittent messages to stop typing
;;(type-break-mode)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)


(setq default-tab-width 4)

;; Insertion of Dates.
(defun insert-date-string ()
  "Insert a nicely formated date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))
;; C-c i calls insert-date-string
(global-set-key (kbd "C-c i") 'insert-date-string)

(defun journal () 
  (interactive) 
  (find-file "~/journal.txt")
  (end-of-buffer)
  (insert "\n\n")
  (insert "*")
  (insert-date-string)
  (insert "\n\n")
  )

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
				  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(global-set-key "\C-c\k" 'copy-line)

(defun mark-line ()
  "Marks a line from start of indentation to end"
  (interactive "p")
  (back-to-indentation)
  (set-mark-command)
  (move-end-of-line))

;;(global-set-key (kbd "C-c l") 'mark-line)
;;(global-set-key "\C-c\C-l" "\C-a\C- \C-e\M-w")

;; (defun duplicate-line()
;;   (interactive)
;;   (move-beginning-of-line 1)
;;   (kill-line)
;;   (yank)
;; ;  (open-line 1)
;; ;  (next-line 1)
;; ;  (yank)
;; )
;; (global-set-key (kbd "C-c l") 'duplicate-line)


(defun wicked/toggle-w3m ()
  "Switch to a w3m buffer or return to the previous buffer."
  (interactive)
  (if (derived-mode-p 'w3m-mode)
      ;; Currently in a w3m buffer
      ;; Bury buffers until you reach a non-w3m one
      (while (derived-mode-p 'w3m-mode)
		(bury-buffer))
    ;; Not in w3m
    ;; Find the first w3m buffer
    (let ((list (buffer-list)))
      (while list
		(if (with-current-buffer (car list)
			  (derived-mode-p 'w3m-mode))
			(progn
			  (switch-to-buffer (car list))
			  (setq list nil))
		  (setq list (cdr list))))
      (unless (derived-mode-p 'w3m-mode)
		(call-interactively 'w3m)))))

(defalias 'qrr 'query-replace-regexp)


(show-paren-mode t)
;;(setq show-paren-style 'expression)
(setq show-paren-style 'parenthesis)


;; Starts the Emacs server
;;(server-start)
;;(add-hook 'after-init-hook 'server-start)


(add-hook 'c-mode-common-hook
		  (lambda ()
			(font-lock-add-keywords nil
									'(("\\&lt;\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
;;(if window-system 
;;    ((add-hook 'server-switch-hook 'raise-frame)))

;; Auto-raise Emacs on activation
;;; (if window-system
;;;     ((defun raise-emacs-app() 
;;;        (shell-command "osascript -e 'tell application \"Emacs\" to activate' &amp;"))
;;;      (add-hook 'server-switch-hook 'raise-emacs-on-app)))

;; Auto-raise Emacs on activation
(defun raise-emacs-on-aqua() 
  (shell-command "osascript -e 'tell application \"Emacs\" to activate' &amp;"))

(when *is-a-mac*
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(0.001))
  (add-hook 'server-switch-hook 'raise-emacs-on-aqua)
  (add-to-list 'exec-path "/Library/Frameworks/Python.framework/Versions/Current/bin/")
  (add-to-list 'exec-path "/usr/local/homebrew/bin")
  (add-to-list 'exec-path (expand-file-name "~/bin"))
  (setenv "PATH" (concat (getenv "PATH") (concat ":" (expand-file-name "~/bin:/usr/local/bin:/usr/texbin"))))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/homebrew/bin"))
  (setq shell-file-name "zsh"))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec" "0a1a7f64f8785ffbf5b5fbe8bca1ee1d9e1fb5e505ad9a0f184499fe6747c1af" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "0f0e3af1ec61d04ff92f238b165dbc6d2a7b4ade7ed9812b4ce6b075e08f49fe" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "73fe242ddbaf2b985689e6ec12e29fab2ecd59f765453ad0e93bc502e6e478d6" "636ecbf1091fbc99d95526d7a3a4810d1ccb58997e58bd3184863821303553f3" "f3d2144fed1adb27794a45e61166e98820ab0bbf3cc7ea708e4bf4b57447ee27" "99cbc2aaa2b77374c2c06091494bd9d2ebfe6dc5f64c7ccdb36c083aff892f7d" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" "e439d894bf9406baf73056cf7e3c913ee5c794b6adadbbb9f614aebed0fd9ce7" "9117c98819cfdeb59780cb43e5d360ff8a5964d7dd9783b01708bda83098b9fd" "4870e6cb6f0a70c14ee73db30b69a8a1f08d6ec9a689c366e88636fb81e8022d" "e992575f7c09459bfc190e6776b8f5f96964023e98267a87fb3094e7c9686776" "36afe64261e1de73fcfadedf154e4bc2c9ec1969bde0c21798d31366897bc4d2" default)))
 '(display-time-mode t)
 '(ecb-options-version "2.32")
 '(flymake-allowed-file-name-masks
   (quote
	(("\\.c\\'" flymake-simple-make-init)
	 ("\\.cpp\\'" flymake-simple-make-init)
	 ("\\.cc\\'" flymake-simple-make-init)
	 ("\\.xml\\'" flymake-xml-init)
	 ("\\.html?\\'" flymake-xml-init)
	 ("\\.cs\\'" flymake-simple-make-init)
	 ("\\.p[ml]\\'" flymake-perl-init)
	 ("\\.php[345]?\\'" flymake-php-init)
	 ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
	 ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
	 ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
	 ("\\.tex\\'" flymake-simple-tex-init)
	 ("\\.idl\\'" flymake-simple-make-init))))
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(safe-local-variable-values
   (quote
	((TeX-master . "../thesis")
	 (TeX-master . "../thesis.tex"))))
 '(show-paren-mode t))

;;(push '("\\.cc$" flymake-cc-init) flymake-allowed-file-name-masks)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((((class color) (min-colors 89)) (:foreground "#859900" :background nil))))
 '(diff-removed ((((class color) (min-colors 89)) (:foreground "#dc322f" :background nil))))
 '(variable-pitch ((t (:height 160 :family "Georgia")))))


(when (eq machine 'giles)
  (require 'php-mode)

  (when *is-cocoa-emacs*
	(require 'color-theme)
	(color-theme-initialize)
	;;(color-theme-marquardt)
	;;(color-theme-robin-hood)
	(color-theme-greiner))

  (setq-default ispell-program-name "aspell")
  ;;(setq ispell-dictionary-alist
  ;;   '((nil
  ;;	 "[A-Za-z]" "[^A-Za-z]" "[']" nil
  ;;	 ("-B" "-d" "english" "--dict-dir"
  ;;	  "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
  ;;	 nil iso-8859-1)))
  (setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi")))

(defun c++-xref-hook ()
  (defvar xref-current-project nil) ;; can be also "my_project_name"
  (defvar xref-key-binding 'global) ;; can be also 'local or 'none
  (setq load-path (cons "/home/jason/sw/xref/emacs" load-path))
  (setq exec-path (cons "/home/jason/sw/xref" exec-path))
  (load "xrefactory")
  )

(when (eq machine 'mclovin)

  (if (eq window-system 'x)
	  (progn
	    (set-default-font "Consolas-10")
		(if (string-match (terminal-name) ":0.1")
			(set-default-font "Consolas-11"))
	    (set-frame-name "emacs")
	    (require 'color-theme)
	    (color-theme-initialize)
		;;(color-theme-blue-mood)
		;;(color-theme-subtle-hacker)
		;;(color-theme-greiner)
		;;(color-theme-tango)
	    (set-frame-height (selected-frame) 70)
	    (set-frame-width (selected-frame) 160)
	    (set-frame-height (selected-frame) 70)
	    (set-frame-width (selected-frame) 160)
		))

  (setq inferior-lisp-program "/usr/bin/clisp")
  ;;(add-to-list 'load-path "the path of your slime directory")
  ;;(require 'slime)
  (slime-setup)

  ;;(require 'flymake)
  ;;(add-hook 'find-file-hook 'flymake-find-file-hook)


  (autoload 'flymake-lua-load "flymake-lua")
  (add-hook 'lua-mode-hook 'flymake-lua-load)


  ;;(require 'flymake-python)
  (autoload 'flymake-python-load "flymake-python")
  (add-hook 'python-mode-hook 'flymake-python-load)

  ;;(require 'flymake-ruby)
  (autoload 'flymake-ruby-load "flymake-ruby")
  (add-hook 'ruby-mode-hook 'flymake-ruby-load)


  (require 'flymake)
  (defun flymake-get-tex-args (file-name)
	(list "pdflatex"
		  (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))
  ;; (add-hook 'LaTeX-mode-hook 'flymake-mode)

  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)

  (add-hook 'LaTeX-mode-hook 'reftex-mode)

  (add-hook 'LaTeX-mode-hook 
			(lambda ()
			  (set-fill-column 100)
			  (longlines-mode)
			  (color-theme-aalto-light)
			  (setq reftex-plug-into-AUCTeX t)
			  (setq reftex-default-bibliography '("/home/jason/projects/thesis/thesis.bib"))
			  ))



  ;; (setq load-path
  ;;   (cons (expand-file-name "~/vm_stuff/llvm/llvm-2.3/utils/emacs") load-path))
  ;; (require 'llvm-mode)

  ;;(require 'php-mode)
  
  ;;(require 'gist)

  (defun djcb-opacity-modify (&optional dec)
	"modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
	(let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	       (oldalpha (if alpha-or-nil alpha-or-nil 100))
	       (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
	  (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
	    (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

  ;; C-8 will increase opacity (== decrease transparency)
  ;; C-9 will decrease opacity (== increase transparency
  ;; C-0 will returns the state to normal
  (global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
  (global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
  (global-set-key (kbd "C-0") '(lambda()(interactive)
								 (modify-frame-parameters nil `((alpha . 100)))))


  (global-set-key [(mouse-13)] 'scroll-down)
  (global-set-key [(mouse-15)] 'scroll-up)

  (add-hook 'c-mode-hook 'c++-xref-hook)
  (add-hook 'c++-mode-hook 'c++-xref-hook)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (setq org-log-done t)
  (global-set-key [(f8)] 'wicked/toggle-w3m)


  )



;;LaTeX stuff
(when (or (and (eq window-system 'ns) (or (eq machine 'odysseus) (eq machine 'giles))) (eq machine 'mclovin))
  (load "auctex.el" nil t t)
  ;; The following only works with AUCTeX loaded
  (require 'tex-site)
  (load "preview-latex.el" nil t t)
  (require 'git-emacs))

(when (and (eq window-system 'ns) (or (eq machine 'odysseus) (eq machine 'giles)) )
  (add-hook 'TeX-mode-hook
			(lambda ()
			  (add-to-list 'TeX-output-view-style
						   '("^pdf$" "."
							 "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b")))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; Use PDF mode by default
(setq-default TeX-PDF-mode t)
;; Make emacs aware of multi-file projects
(setq-default TeX-master nil)


;; flymake
(defun my-flymake-show-next-error()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line)
  )

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)  ; use spaces only if nil
  (flymake-mode t)


  ;; Flymake does not recognize warnings in GCC 4.5, fix this
  (add-to-list
   'flymake-err-line-patterns
   '(" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(       \n]+\\):\\([0-9]+\\):[0-9]+:[   \n]*\\(.+\\)" 2 4 nil 5))

  ;; (global-set-key "C-cC-v" 'my-flymake-show-next-error)
  ;; (global-set-key (kbd "C-c C-v") 'my-flymake-show-next-error)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;;; (autoload 'python-mode "python-mode" "Python Mode." t)
;;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;; (add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-mode))
;;; (add-to-list 'auto-mode-alist '("\\makefile\\'" . makefile-mode))
;;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;;; (add-hook 'python-mode-hook
;;;     (lambda ()
;;;         (set (make-variable-buffer-local 'beginning-of-defun-function)
;;;             'py-beginning-of-def-or-class)
;;;         (setq outline-regexp "def\\|class ")))
										;(autoload 'py-complete-init "py-complete")
										;(add-hook 'python-mode-hook 'py-complete-init)
										;(autoload 'pycomplete-init "pycomplete")
										;(add-hook 'python-mode-hook 'pycomplete-init)
										;(require 'pycomplete)


;;; (defvar shell-login-switch nil
;;; "Command-line switch to be used with the shell to get a login shell.
;;; If nil, a switch is automatically chosen depending on
;;; `shell-file-name'.
;;; This is relevant only for `mac-read-environment-vars-from-shell'.")

;;; (defun mac-read-environment-vars-from-shell ()
;;; "Import the environment from the system's default login shell
;;; specified in `shell-file-name'."
;;;     (with-temp-buffer
;;;       ;; execute 'printenv' with the default login shell,
;;;       ;; running the shell with -l (to load the environment)
;;;       (setq default-directory "~/")     ; ensure it can be executed
;;;       ;; To Do: use call-process instead -&gt; this here
;;;       ;; will invoke two bashes

;;;       (let ((shell-login-switch
;;;              (or shell-login-switch
;;;                  (if (string-match ".*/\\(ba\\|z\\)sh" shell-file-name)
;;;                      "-l"
;;;                    (if (string-match ".*/\\tcsh" shell-file-name)
;;;                      ""
;;;                    (if (string-match ".*/ksh" shell-file-name)
;;;                        "" ;; works for ksh
;;;                      (message "Could not retrieve login shell environment with login shell: %s" shell-file-name)
;;;                    ;; won't work for csh, because it doesn't take -l -c ...
;;;                    ))))))

;;;         (call-process shell-file-name nil
;;;                t nil
;;;                 shell-login-switch
;;;                 shell-command-switch
;;;                 "printenv"))
;;;       (goto-char (point-min))
;;;       (while (re-search-forward "^[A-Za-z_0-9]+=()\s*[^\x]*?
;;; \s*}\s*$" nil t)
;;;         (replace-match "..." nil nil))
;;;       (goto-char (point-min))
;;;       (while (search-forward-regexp "^\\([A-Za-z_0-9]+\\)=\\(.*\\)$" nil t)
;;;         (setenv
;;;          (match-string 1)
;;;          (if (equal (match-string 1) "PATH")
;;;              (concat (match-string 2) ":" (getenv "PATH"))
;;;              (match-string 2))))))

;;; (defun mac-add-path-to-exec-path ()
;;;   "Add elements from environment variable `PATH' to `exec-path'."
;;;   (let ((l (split-string (getenv "PATH") ":")))
;;;   (mapc
;;;    (lambda (p)
;;;      (unless (member p l)
;;;        (nconc l (list p))))
;;;    exec-path)
;;;   (setq exec-path l)))

;;; (defun mac-add-local-directory-to-exec-path ()
;;;   "Add /usr/locaL/bin to `exec-path'"
;;;   (add-to-list 'exec-path "/usr/local/bin"))


;;; (mac-read-environment-vars-from-shell)
;;; (mac-add-path-to-exec-path)



(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))


(when (>= emacs-major-version 24)
  (require 'package)
  ;; (add-to-list 'package-archives
  ;; 			   '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
						   ("marmalade" . "https://marmalade-repo.org/packages/")
						   ("melpa" . "https://melpa.org/packages/")
						   ("org" . "http://orgmode.org/elpa/")))
  (or (file-exists-p package-user-dir)
	 (package-refresh-contents))

  (package-initialize)

  ;; '(ack-and-a-half auctex clojure-mode coffee-mode deft expand-region
  ;; 				 gist groovy-mode haml-mode haskell-mode inf-ruby
  ;; 				 magit magithub markdown-mode paredit projectile python
  ;; 				 sass-mode rainbow-mode scss-mode solarized-theme
  ;; 				 volatile-highlights yaml-mode yari zenburn-theme)

  ;;(4clojure ac-ispell ac-nrepl ace-jump-mode ack-and-a-half auctex autopair clojure-cheatsheet cider clojure-snippets clojurescript-mode color-theme-sanityinc-tomorrow color-theme-tango color-theme csharp-mode cyberpunk-theme ein elein flycheck f flymake-tuareg gist gh gnugo go-autocomplete auto-complete go-mode go-snippets google-maps graphviz-dot-mode guru-mode hackernews haskell-mode helm-ack helm-google google helm-spotify helm ibuffer-git ido-ubiquitous ipython js2-mode logito magit git-rebase-mode git-commit-mode markdown-mode monokai-theme multi nrepl clojure-mode nyan-prompt org-jekyll org-plus-contrib ox-reveal org pcache popup powerline pretty-lambdada pretty-mode projectile pkg-info epl protobuf-mode rainbow-blocks rainbow-delimiters request rust-mode s sentence-highlight smart-mode-line smartparens smex solarized-theme dash spotify ssh-config-mode tuareg caml w3m weather weather-metno websocket writegood-mode xkcd yasnippet zenburn-theme)

  (ensure-package-installed 'xkcd 'smex 'ace-jump-mode 'yasnippet 'pretty-mode)


  (global-set-key [(meta x)] (lambda ()
							   (interactive)
							   (or (boundp 'smex-cache)
								  (smex-initialize))
							   (global-set-key [(meta x)] 'smex)
							   (smex)))

  (global-set-key [(shift meta x)] (lambda ()
									 (interactive)
									 (or (boundp 'smex-cache)
										(smex-initialize))
									 (global-set-key [(shift meta x)] 'smex-major-mode-commands)
									 (smex-major-mode-commands)))

  (global-pretty-mode 1)
  ;;(add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)
  (add-hook 'clojure-mode-hook 'turn-on-pretty-mode)

  )

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


(yas-global-mode 1)

;; Completing point by some yasnippet key
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
	(while (and
			(not (= (point) (point-min) ))
			(not
			 (string-match "[[:space:]\n]" (char-to-string (char-before)))))
	  (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))
(define-key yas-minor-mode-map (kbd "<C-tab>")     'yas-ido-expand)

(require 'recentf)
(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))


(set-frame-parameter (selected-frame) 'alpha '(97 90))


(when (string= (system-name) "JMCCANDLESS")

  (add-to-list 'default-frame-alist '(left . 0))
  (add-to-list 'default-frame-alist '(top . 0))
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 120))

  (load-theme 'monokai)

  (set-default-font "Consolas-11:antialias=subpixel")
  ;;(set-default-font "Source Code Pro-10:antialias=subpixel")

  ;; (apply 'ensure-package-installed '(ace-jump-mode confluence csharp-mode csv-mode flycheck f fsharp-mode auto-complete helm-spotify helm js2-mode json-mode json-reformat jsshell magit git-rebase-mode git-commit-mode markdown-mode monokai-theme multi mustache ht mustache-mode org-jira paredit popup pos-tip pretty-mode projectile pkg-info epl rainbow-delimiters s smex solarized-theme dash xkcd xml-rpc yasnippet))


  (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")

  (setenv "PATH" (concat (getenv "PATH") ";C:\\Program Files (x86)\\Git\\bin"))

  (ensure-package-installed 'mustache 'confluence)

  ;; (require 'remember)
  ;;   (org-remember-insinuate)
  (setq org-remember-templates
		'(("Journal"
		   ?j
		   "* %U %? %^g\n\n   %x"
		   "~/journal.org"
		   'top)))
  (global-set-key (kbd "C-c j") 'org-remember)


  (setq projectile-indexing-method 'native)
  (projectile-global-mode)

  (defun csharp-set-flycheck-command ()
	"Set the flycheck command, dynamically, as a side effect.
 
This function is intended for use as a before-syntax-check-hook with
flycheck.  Use it like this:
 
    (add-hook 'flycheck-before-syntax-check-hook  #'csharp-set-flycheck-command)
 
Then, in your csharp file, specify this in the comments at the header.
 
    // flycheck: gmcs -t:module /debug+ -pkg:dotnet %f
 
This will cause flycheck to run the given command, replacing the %f with
the source file name."
	(and (eq major-mode 'csharp-mode)
	   (let ((cmd-string
			  (csharp-get-value-from-comments "flycheck" csharp-cmd-line-limit)))
		 (and cmd-string
			(not (eq cmd-string ""))
			(let* ((cmd (split-string cmd-string " "))
				   (ferf (member "%f" cmd)))
			  (and ferf (setcar ferf 'source))
			  (put 'csharp :flycheck-command cmd))))))
  (eval-after-load "flycheck"
	'(progn
	   (flycheck-define-checker csharp
		 "A C# syntax checker for dotnet. By default, it uses the Mono
compiler. If you would like to use a different compiler, see
`csharp-set-flycheck-command'."
		 :command ("gmcs" "-target:module" source)
		 :error-patterns
		 ;; WinFormsHello.cs(17,9): error CS0246: The type or namespace name `derp' could not be found. Are you missing an assembly reference?
		 ((error line-start (file-name) "(" line "," column "): error " (message) line-end)
		  (warning line-start (file-name) "(" line "," column "): warning " (message) line-end))
		 :modes csharp-mode)
	   (add-hook 'flycheck-before-syntax-check-hook  #'csharp-set-flycheck-command)))

  (setq confluence-url "https://zocdoc.atlassian.net/wiki/rpc/xmlrpc")


  (add-to-list 'load-path "~/org-impress-js.el")
  (require 'org-impress-js)

  (add-to-list 'load-path "~/.emacs.d/emacs-powerline")
  (require 'powerline)


  (setq jiralib-url "https://zocdoc.atlassian.net")

  (add-to-list 'load-path "~/.emacs.d/org-jira")
  (require 'org-jira)

  )

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "https://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
						   (buffer-substring (region-beginning) (region-end))
						 (read-string "Google: "))))))


(when (eq machine 'mba)

  ;; Set the starting position and width and height of Emacs Window
  ;; (add-to-list 'default-frame-alist '(left . 20))
  ;; (add-to-list 'default-frame-alist '(top . 0))
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(width . 140))

  (global-set-key "\C-cg" 'writegood-mode)

  (setq-default ispell-program-name "aspell")
  ;;(setq ispell-extra-args '("-d" "/usr/local/homebrew/bin/aspell"))
  ;;(setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi"))
  (setq ispell-extra-args '("-d" "/usr/local/homebrew/lib/aspell-0.60/en.multi"))
  ;;(setq ispell-extra-args '("--sug-mode=ultra"))


  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-buffer)


  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-buffer)

  (add-hook 'LaTeX-mode-hook 'reftex-mode)
  (add-hook 'LaTeX-mode-hook 
			(lambda ()
			  (set-fill-column 100)
			  (longlines-mode)
			  ;;(color-theme-aalto-light)
			  (setq reftex-plug-into-AUCTeX t)
			  (setq reftex-default-bibliography '("/Users/jason/Desktop/thesis/thesis.bib"))
			  (variable-pitch-mode t)
			  ;; (font-lock-add-keywords nil
			  ;; 						  '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))

			  ;; (font-lock-add-keywords nil
			  ;; 						  '(("\\<TODO\\>" 1 font-lock-warning-face t)))

			  (font-lock-add-keywords nil 
									  '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):" 
										 1 font-lock-warning-face prepend)))))

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   ;;'(echo-area ((t (:stipple nil :strike-through nil :underline nil :slant normal :weight normal :width normal :family "Inconsolata"))))
   ;; '(hl-sentence-face ((t (:foreground "white"))) t)
   ;; '(variable-pitch ((t (:foreground "gray80" :height 170 :family "Cochin")))))

   ;;'(variable-pitch ((t (:height 170 :family "Times"))))
   ;;'(variable-pitch ((t (:height 170 :family "Lucida Grande"))))
   ;;'(variable-pitch ((t (:family "Lucida Grande"))))
   ;;'(variable-pitch ((t (:size 24 :family "Georgia"))))

   '(variable-pitch ((t (:height 160 :family "Georgia"))))
   )

  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (add-to-list 'load-path "~/.emacs.d/themes")

  ;;(load-theme whiteboard)
  ;;(load-theme adwaita)
  (load-theme 'monokai)
  ;;(load-theme 'tomorrow-night-bright)
  ;;(load-theme 'sanityinc-tomorrow-bright)

  ;; (add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")
  ;; (require 'powerline)

  ; (powerline-default-theme)

  (setq sml/theme 'dark)
  (sml/setup)


  (add-to-list 'load-path "~/.emacs.d/org-impress-js.el")
  (require 'org-impress-js)
  ;;C-u <TAB>


  (require 'helm-config)
  ;;(global-set-key (kbd "C-c h") 'helm-mini)
  ;;(helm-mode 1)

  (setq weather-metno-location-name "Brooklyn, USA"
		weather-metno-location-latitude 40.708441
		weather-metno-location-longitude -73.921466)


  ;;(set-default-font "Consolas-13")
  ;;(set-default-font "Lucida Grande-13")
  (set-default-font "Source Code Pro-12")
  ;;(set-default-font "Inconsolata-15")
  ;;(set-default-font "Menlo-14")
  ;;(set-frame-font "Menlo:pixelsize=18")

  (add-hook 'clojure-mode-hook 'smartparens-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

  )

(when (eq machine 'apollo)
  (load-theme 'monokai t))


(when (eq machine 'werklt)
  (ensure-package-installed 'smex 'smart-mode-line 'powerline)
  
  (load-theme 'wombat t)
  (setq sml/theme 'dark)
  (sml/setup)
  (when (display-graphic-p)
	(set-default-font "Source Code Pro-12")
	(powerline-default-theme)
	(add-to-list 'default-frame-alist '(left . 0))
	(add-to-list 'default-frame-alist '(top . 0))
	(add-to-list 'default-frame-alist '(height . 70))
	(add-to-list 'default-frame-alist '(width . 100)))
  )

(when (eq machine 'werk)
  (ensure-package-installed 'smex 'smart-mode-line 'powerline)
  
  (load-theme 'monokai t)
  (setq sml/theme 'dark)
  (sml/setup)
  (when (display-graphic-p)
	(set-default-font "Source Code Pro-12")
	(powerline-default-theme)
	)
  )

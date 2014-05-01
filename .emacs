(add-to-list 'load-path "~/.emacs.d/")

(defun set-load-path ()
    (set 'load-path (cons (expand-file-name "~/lisp") load-path))
    ;; add others here
    (message "Load path set."))

(set-load-path)
(global-font-lock-mode 1)

(add-to-list 'exec-path "/usr/local/bin")

(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

(if (eq system-type 'darwin)
    (setq system-name (car (split-string system-name "\\."))))

(if (string-match "odysseus" (system-name))
    (setq machine 'odysseus)
  (if (string-match "giles" (system-name))
	  (setq machine 'giles)
	(if (string-match "mclovin" (system-name))
		(setq machine 'mclovin)
	(if (string-match "turing.scss.tcd.ie" (system-name))
		(setq machine 'turing)
		(setq machine 'other)))))


(when (or (or (eq window-system 'ns) (eq window-system 'x) (eq window-system 'w32)))
      (mouse-wheel-mode t)
      (scroll-bar-mode -1)
      (blink-cursor-mode 0)
      (tool-bar-mode 0))

;; truncate lines if they are too long

;; trucate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)

;; inhibit startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; get intermittent messages to stop typing
;;(type-break-mode)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1) 

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



;; Starts the Emacs server
;;(server-start)
;;(add-hook 'after-init-hook 'server-start)


(add-hook 'c-mode-common-hook
               (lambda ()
                (font-lock-add-keywords nil
                 '(("\\&lt;\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))


;(if window-system 
;    ((add-hook 'server-switch-hook 'raise-frame)))

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
      (add-to-list 'exec-path "/sw/bin")
      (add-to-list 'exec-path "/usr/local/git/bin")
      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/texbin"))
      (setq shell-file-name "/bin/zsh"))

(show-paren-mode t)
(setq show-paren-style 'expression)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "73fe242ddbaf2b985689e6ec12e29fab2ecd59f765453ad0e93bc502e6e478d6" "99cbc2aaa2b77374c2c06091494bd9d2ebfe6dc5f64c7ccdb36c083aff892f7d" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" "71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" default)))
 '(display-time-mode t)
 '(ecb-options-version "2.32")
 '(flymake-allowed-file-name-masks (quote (("\\.c\\'" flymake-simple-make-init) ("\\.cpp\\'" flymake-simple-make-init) ("\\.cc\\'" flymake-simple-make-init) ("\\.xml\\'" flymake-xml-init) ("\\.html?\\'" flymake-xml-init) ("\\.p[ml]\\'" flymake-perl-init) ("\\.php[345]?\\'" flymake-php-init) ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup) ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup) ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup) ("\\.tex\\'" flymake-simple-tex-init) ("\\.idl\\'" flymake-simple-make-init))))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(show-paren-mode t)
)

;;(push '("\\.cc$" flymake-cc-init) flymake-allowed-file-name-masks)







(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((((class color) (min-colors 89)) (:foreground "#859900" :background nil))))
 '(diff-removed ((((class color) (min-colors 89)) (:foreground "#dc322f" :background nil)))))

(when (eq machine 'giles)
      (require 'php-mode)

	  (when *is-cocoa-emacs*
		(require 'color-theme)
		(color-theme-initialize)
		;(color-theme-marquardt)
		;(color-theme-robin-hood)
		(color-theme-greiner))


	   (setq-default ispell-program-name "aspell")
	;(setq ispell-dictionary-alist
	;   '((nil
	;	 "[A-Za-z]" "[^A-Za-z]" "[']" nil
	;	 ("-B" "-d" "english" "--dict-dir"
	;	  "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
	;	 nil iso-8859-1)))
	   (setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi")))

(when (or (eq machine 'mclovin) (eq machine 'giles))
      (setq ange-ftp-local-host-regexp "\\.tcd\\.ie$\\|\\.[0-9]+\\.[0-9]+$\\|^[^.]*$")
      (setq ange-ftp-gateway-host "ftp-proxy.cs.tcd.ie")
      (setq ange-ftp-smart-gateway-port "24")
      (setq ange-ftp-smart-gateway t))


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
	    ;(color-theme-blue-mood)
	    ;(color-theme-subtle-hacker)
	    ;(color-theme-greiner)
	    ;(color-theme-tango)
	    (set-frame-height (selected-frame) 70)
	    (set-frame-width (selected-frame) 160)
	    (set-frame-height (selected-frame) 70)
	    (set-frame-width (selected-frame) 160)
	  ))

      (setq inferior-lisp-program "/usr/bin/clisp")
      ;;(add-to-list 'load-path "the path of your slime directory")
      ;(require 'slime)
      (slime-setup)

	  ;(require 'flymake)
	  ;(add-hook 'find-file-hook 'flymake-find-file-hook)


      (setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
      (autoload 'lua-mode "lua-mode" "Lua editing mode." t)

      (add-hook 'lua-mode-hook 'turn-on-font-lock)
      ;(add-hook 'lua-mode-hook 'hs-minor-mode)
	  ;(require 'flymake-lua)
	  (autoload 'flymake-lua-load "flymake-lua")
	  (add-hook 'lua-mode-hook 'flymake-lua-load)


	  ;(require 'flymake-python)
	  (autoload 'flymake-python-load "flymake-python")
	  (add-hook 'python-mode-hook 'flymake-python-load)

	  ;(require 'flymake-ruby)
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

      ;(require 'php-mode)
	  
	  ;(require 'gist)

      (defun djcb-opacity-modify (&amp;optional dec)
	"modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
	(let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
	       (oldalpha (if alpha-or-nil alpha-or-nil 100))
	       (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
	  (when (and (&gt;= newalpha frame-alpha-lower-limit) (&lt;= newalpha 100))
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


	  (require 'package)
	  (add-to-list 'package-archives
				   '("marmalade" . "http://marmalade-repo.org/packages/") t)
	  (package-initialize)

	  
	  ;; clojure-mode
	  ;;(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
	  ;;(require 'clojure-mode)

	  ;; swank-clojure
	  ;;(add-to-list 'load-path "~/sw/swank-clojure/src/emacs")

	  ;; (setq swank-clojure-jar-path "/usr/share/java/clojure.jar"
	  ;; 		swank-clojure-extra-classpaths (list
	  ;; 										"~/sw/swank-clojure/src/main/clojure"))


	  ;; (require 'swank-clojure-autoload)

	  ;; ;; slime
	  ;; (eval-after-load "slime" 
	  ;; 	'(progn (slime-setup '(slime-repl))))

	  ;; ;; (add-to-list 'load-path "~/opt/slime")
	  ;; (require 'slime)
	  ;; (slime-setup)


))))


(when (eq machine 'odysseus)
      (require 'php-mode)

      (setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
      (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
      (add-hook 'lua-mode-hook 'turn-on-font-lock)
      ;(add-hook 'lua-mode-hook 'hs-minor-mode)

	  (when *is-cocoa-emacs*
		(require 'color-theme)
		(color-theme-initialize)
		;(color-theme-greiner)
		(color-theme-tango)))




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



(when (string= (system-name) "JMCCANDLESS")
  (require 'package)
  ;; (add-to-list 'package-archives
  ;; 			   '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						   ("marmalade" . "http://marmalade-repo.org/packages/")
						   ("melpa" . "http://melpa.milkbox.net/packages/")
						   ("org" . "http://orgmode.org/elpa/")))
  (package-initialize)

  (require 'pretty-mode)
   ; if you want to set it globally
  (global-pretty-mode t)
   ; if you want to set it only for a specific mode
   ;;(add-hook 'my-pretty-language-hook 'turn-on-pretty-mode)

  (load-theme 'monokai)

  (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")

  (setenv "PATH" (concat (getenv "PATH") ";C:\\Program Files (x86)\\Git\\bin"))


  (require 'mustache-mode)

  ;; (require 'remember)
  ;;   (org-remember-insinuate)
  (setq org-remember-templates
		'(("Journal"
		   ?j
		   "* %U %? %^g\n\n   %x"
		   "~/journal.org"
		   'top)))
  (global-set-key (kbd "C-c j") 'org-remember)


  (set-default-font "Consolas-11:antialias=subpixel")

										;(set-default-font "Source Code Pro-10:antialias=subpixel")


  (menu-bar-mode -1)

  (setq projectile-indexing-method 'native)
  (projectile-global-mode)

  (require 'yasnippet)

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

  (require 'confluence)
  (setq confluence-url "https://zocdoc.atlassian.net/wiki/rpc/xmlrpc")


  (add-to-list 'load-path "~/org-impress-js.el")
  (require 'org-impress-js)

  (add-to-list 'load-path "~/.emacs.d/emacs-powerline")
  (require 'powerline)


  (setq jiralib-url "https://zocdoc.atlassian.net")

  (add-to-list 'load-path "~/.emacs.d/org-jira")
  (require 'org-jira)

  )

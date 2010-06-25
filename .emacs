(add-to-list 'load-path "~/.emacs.d/")

(defun set-load-path ()
    (set 'load-path (cons (expand-file-name "~/lisp") load-path))
    ;; add others here
    (message "Load path set."))

(set-load-path)
(global-font-lock-mode 1)

(add-to-list 'exec-path "/usr/local/bin")

(if (string-match "odysseus" (system-name))
	(setq machine 'odysseus)
  (if (string-match "giles" (system-name)
					(setq machine 'giles))
	  (if (string-match "mclovin" (system-name)
						(setq machine 'mclovin)))))


(if (or (eq window-system 'ns) (eq window-system 'x))
    (progn
      (mouse-wheel-mode t)
      (scroll-bar-mode -1)
      (blink-cursor-mode 0)
      (tool-bar-mode 0)))

;; truncate lines if they are too long

;; trucate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)

;; inhibit startup message
(setq inhibit-startup-message t)

;; get intermittent messages to stop typing
;;(type-break-mode)

(iswitchb-mode 1)

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
(server-start)
(add-hook 'after-init-hook 'server-start)

;(if window-system 
;    ((add-hook 'server-switch-hook 'raise-frame)))

;; Auto-raise Emacs on activation
;;; (if window-system
;;;     ((defun raise-emacs-app() 
;;;        (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))
;;;      (add-hook 'server-switch-hook 'raise-emacs-on-app)))

;; Auto-raise Emacs on activation
(defun raise-emacs-on-aqua() 
    (shell-command "osascript -e 'tell application \"Emacs\" to activate' &"))

(if (eq system-type 'darwin)
    (progn
      (add-hook 'server-switch-hook 'raise-emacs-on-aqua)
      (add-to-list 'exec-path "/Library/Frameworks/Python.framework/Versions/Current/bin/")
      (add-to-list 'exec-path "/sw/bin")
      (add-to-list 'exec-path "/usr/local/git/bin")
      (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/usr/texbin"))
      (setq shell-file-name "/bin/zsh")))

(show-paren-mode t)
(setq show-paren-style 'expression)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-time-mode t)
 '(ecb-options-version "2.32")
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(show-paren-mode t))

(custom-set-faces
 '(diff-added ((t (:foreground "Dark Green"))) 'now)
 '(diff-removed ((t (:foreground "Red"))) 'now)
 )

(if (eq machine 'giles)
    (progn (require 'color-theme)
	   (color-theme-initialize)
	   ;(color-theme-marquardt)
	   (color-theme-greiner)
	   ;(color-theme-robin-hood)
	   (require 'php-mode)
	   (setq-default ispell-program-name "aspell")
	;(setq ispell-dictionary-alist
	;   '((nil
	;	 "[A-Za-z]" "[^A-Za-z]" "[']" nil
	;	 ("-B" "-d" "english" "--dict-dir"
	;	  "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
	;	 nil iso-8859-1)))
	   (setq ispell-extra-args '("-d" "/Library/Application Support/cocoAspell/aspell6-en-6.0-0/en.multi"))
	   )
  )

(if (or (eq machine 'mclovin) (eq machine 'giles))
    (progn
      (setq ange-ftp-local-host-regexp "\\.tcd\\.ie$\\|\\.[0-9]+\\.[0-9]+$\\|^[^.]*$")
      (setq ange-ftp-gateway-host "ftp-proxy.cs.tcd.ie")
      (setq ange-ftp-smart-gateway-port "24")
      (setq ange-ftp-smart-gateway t)))


(defun c++-xref-hook ()
  (defvar xref-current-project nil) ;; can be also "my_project_name"
  (defvar xref-key-binding 'global) ;; can be also 'local or 'none
  (setq load-path (cons "/home/jason/sw/xref/emacs" load-path))
  (setq exec-path (cons "/home/jason/sw/xref" exec-path))
  (load "xrefactory")
)

(if (eq machine 'mclovin)
    (progn

      (if (eq window-system 'x)
	  (progn
	    (set-default-font "Consolas-10")
	    (set-frame-height (selected-frame) 70)
	    (set-frame-width (selected-frame) 160)
	    (set-frame-name "emacs")
	    (require 'color-theme)
	    (color-theme-initialize)
	    ;(color-theme-blue-mood)
	    ;(color-theme-subtle-hacker)
	    (color-theme-greiner)))

      (setq inferior-lisp-program "/usr/bin/clisp")
      ;;(add-to-list 'load-path "the path of your slime directory")
      (require 'slime)
      (slime-setup)

      (setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
      (autoload 'lua-mode "lua-mode" "Lua editing mode." t)

      (add-hook 'lua-mode-hook 'turn-on-font-lock)
      ;(add-hook 'lua-mode-hook 'hs-minor-mode)

      (setq load-path
	    (cons (expand-file-name "~/vm_stuff/llvm/llvm-2.3/utils/emacs") load-path))
      (require 'llvm-mode)

      (require 'php-mode)

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
      (global-set-key [(f8)] 'wicked/toggle-w3m)))


(if (eq machine 'odysseus)
    (progn
      (require 'color-theme)
      (require 'php-mode)
      (color-theme-initialize)
      (color-theme-greiner)))



;;LaTeX stuff
(if (or (and (eq window-system 'ns) (or (eq machine 'odysseus) (eq machine 'giles))) (eq machine 'mclovin))
    (progn
      (load "auctex.el" nil t t)
      ;; The following only works with AUCTeX loaded
      (require 'tex-site)
      (load "preview-latex.el" nil t t)
	  (require 'git-emacs)))

(if (and (eq window-system 'ns) (or (eq machine 'odysseus) (eq machine 'giles)) )
    (progn
      (add-hook 'TeX-mode-hook
		(lambda ()
		  (add-to-list 'TeX-output-view-style
			       '("^pdf$" "."
				 "/Applications/Skim.app/Contents/SharedSupport/displayline %n %o %b"))))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
;; Use PDF mode by default
(setq-default TeX-PDF-mode t)
;; Make emacs aware of multi-file projects
(setq-default TeX-master nil)

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
;;;       ;; To Do: use call-process instead -> this here
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
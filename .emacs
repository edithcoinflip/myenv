(require 'package)

;context-coloring
;helm-spotify-plus
;ac-c-headers
;ac-helm
;ac-js2
;alect-themes
;find-file-in-project
;flymake
;flymake-python-pyflakes
;highlight-indentation
;highlight-numbers
;highlight-parentheses
;js2-highlight-vars
;jsx-mode
;rainbow-delimiters
;solarized-theme
;theme-looper
;web-mode
;whitespace-cleanup-mode

(setq package-list '(use-package
		      auto-compile
		      auto-complete
		      auto-complete-c-headers
		      auto-complete-clang
		      auto-complete-nxml
		      auto-highlight-symbol
		      auto-package-update
		      avy
		      bash-completion
		      beacon
		      bicycle
		      c-eldoc
		      cmake-ide
		      cmake-mode
		      company
		      company-c-headers
		      company-irony
		      company-irony-c-headers
		      company-jedi
		      cpp-auto-include
		      cpp-capf
		      cpputils-cmake
		      cycle-themes
		      emacs-setup
		      git-ps1-mode
		      gitconfig
		      gitconfig-mode
		      github-clone
		      github-pullrequest
		      helm
		      helm-git
		      helm-git-grep
		      helm-spotify
		      highlight
		      ivy
		      jedi
		      js2-mode
		      json-mode
		      osx-clipboard
		      package+
		      rainbow-blocks
		      rainbow-delimiters
		      rainbow-identifiers
		      solarized-theme
		      sourcetrail
		      win-switch
		      yaml-mode		    
		      ))

;; Setup Melpa
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))
    (add-to-list 'package-archives (cons "marmalade" (concat proto "://marmalade-repo.org/packages/")))
    )
  )

(package-initialize)

;; install packages
(unless package-archive-contents
    (package-refresh-contents))

(dolist (package package-list)
  (unless (eq (package-installed-p package) t)
    (when 
	(yes-or-no-p (concat
		      "The package " (symbol-name package)
		      " is not installed. Install it? "))
      (package-install package)
      (when (eq package 'pdf-tools) (pdf-tools-install))
      )
    )
  )

(require 'use-package)

(use-package helm-config)
(use-package ido)
(use-package windmove)
(use-package beacon)
(use-package auto-complete)
;(use-package webmode)
(use-package find-file)


(setq custom-safe-themes t)
(load-theme 'solarized-dark)

(global-linum-mode 1)
(column-number-mode 1)
(beacon-mode 1)
(ac-config-default)
(global-auto-complete-mode t)
(helm-mode 1)
(rainbow-delimiters-mode t)
(rainbow-identifiers-mode t)

;variable setting

(setq c-default-style "linux")
(setq compilation-window-height 16)
(setq linum-format "%d ")
(setq-default indicate-empty-lines t)
(setq-default x-stretch-cursor t) ;; when on a TAB, the cursor has the TAB length
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq js-indent-level 2)
(setq compilation-scroll-output t) ;; keep the window focused on the messages during compilation
(setq vc-follow-symlinks nil)
(setq uncomfortable-buffer-size (* 10 1024 1024))
(setq next-error-highlight t) ;; Keep the highlight on the compilation error
(setq windmove-wrap-around t)
(when (functionp 'tool-bar-mode) (tool-bar-mode -1))
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)
(show-paren-mode 1)
(blink-cursor-mode 0) ;; Stop this crazy blinking cursor
(global-hl-line-mode) ;; highlight current line subtly, to help find cursor
(fset 'yes-or-no-p 'y-or-n-p) ;; Make all "yes or no" prompts be "y or n" instead

;; Get rid of the visual bell for some common 'errors'
(setq ring-bell-function
      (lambda ()
	(unless (memq this-command
		      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
	  (ding))))

(autoload 'jedi:setup "jedi" nil t)

;; Vince Search
(setq cc-search-directories
      '("."                          ; always look in . first
	".."
	"$ROOT/*"
	"/usr/include"
	"/usr/include/*"
	"/usr/local/include/*"       ; for system .h files
	"../*"
	"../../*"
	"../../*/*/*"
      ))

(setq cc-other-file-alist
  '(
    ("\\.c$"   (".h"))
    ("\\.cpp$" (".h" ".hpp"))
    ("\\.cxx$" (".hxx" ".h" ".h"))
    ("\\.h$"   (".cpp" ".cxx" ".c"))
    ("\\.hxx$"  (".cxx" ".cpp"))
    ("\\.hpp$"  (".cpp" ".cxx" ".c"))
    ("\\.C$"   (".I" ".H" ))
    ("\\.H$"   (".C" ".I" "INLINES.C" ))
    ("\\.I$"   (".H" ))
    ))

;;; Func defs

;;funcs

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
	((looking-at "\\s)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(defun close-frame-or-exit ()
  "Tries to close the current frame, if it's the only one left just exits."
  (interactive)
  (if (= (length (frame-list)) 1)
      (save-buffers-kill-emacs)
    (delete-frame)))

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

;; Taken from Trey Jackson's answer on superuser.com
;; http://superuser.com/questions/205420/how-can-i-interrupt-emacs-opening-a-large-file
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) uncomfortable-buffer-size)
    (setq buffer-read-only t)
    (setq auto-save-default nil)
    (buffer-disable-undo)
    (fundamental-mode)
    (message "Large buffer: Undo disabled, made read only, autosave disabled.")))

(defun my-c-mode-hook ()
  (c-set-style "linux")
  (c-toggle-hungry-state 1)
  (setq indent-tabs-mode nil)
  (setq show-trailing-whitespace t)
  (setq c-basic-offset 2
        c-brace-offset 0
        c-continued-brace-offset 0
        c-continued-statement-offset 0
        c-brace-imaginary-offset 0
        c-argdecl-indent 2
        c-label-offset -2)
  (linum-mode)
  (c-set-offset (quote substatement-open) 0 nil)
  (c-set-offset 'case-label '+)
  (c-subword-mode 1)
  )

(defun my-python-mode-hook ()
  (linum-mode)
  (setq show-trailing-whitespace t)
  (require 'pymacs)
  (require 'epc)
  (autoload 'pymacs-apply "pymacs")
  (autoload 'pymacs-call "pymacs")
  (autoload 'pymacs-eval "pymacs" nil t)
  (autoload 'pymacs-exec "pymacs" nil t)
  (autoload 'pymacs-load "pymacs" nil t)
  (define-key python-mode-map "\C-m" 'newline-and-indent)
  (auto-complete-mode t)
  (ac-ropemacs-setup)
  (setq ropemacs-confirm-saving 'nil)
  (setq ropemacs-enable-autoimport t)
  (ropemacs-mode t)
;  (require 'pretty-lambda)
;  (pretty-lambda-mode 1)
  )

;; Hook work

(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
(add-hook 'makefile-mode-hook 'whitespace-mode)
(add-hook 'c-mode-common-hook
	  (lambda()
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'python-mode-hook
	  (lambda()
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'shell-script-mode-hook
	  (lambda()
	    (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'find-file-hooks 'my-find-file-check-make-large-file-read-only-hook)

;;custom stuff

;;macros
(fset 'idir
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 5 134217848 114 101 112 108 97 99 101 45 115 116 114 105 110 103 return 32 return 32 92 17 10 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 return] 0 "%d")) arg)))



;;key binds
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c g") 'helm-git-grep)
(global-set-key (kbd "C-c C-g") 'helm-git-grep-at-point)
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key (kbd "C-x C-k 0") 'idir)
(global-set-key "%" 'match-paren)

;;; Gloabl Set Keys
;(global-set-key (kbd "<f4>") 'eval-last-sexp); eval
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-cb" 'compile)
(global-set-key (kbd "<f5>") 'compile)
(global-set-key "%" 'match-paren)
;(global-set-key [C-tab] 'hippie-expand)
(global-set-key "\C-cp" 'set-variable)
(global-set-key "\C-c_w" 'whitespace-mode)
(global-set-key (kbd "<f7>") 'find-tag)
;(global-set-key "\C-ct" 'find-tag)
;(global-set-key "\C-cg" 'find-tag-other-window)
;(global-set-key (kbd "<f8>") 'find-tag-other-window)
(global-set-key "\C-ca" 'pop-tag-mark)
(global-set-key (kbd "<f9>") 'pop-tag-mark)
(global-set-key "\C-cf" 'tags-apropos)
(global-set-key (kbd "s-o") 'other-frame)
(define-key global-map "\C-co" 'ff-find-other-file)
(global-set-key (kbd "s-j") 'previous-buffer)
(global-set-key (kbd "s-k") 'next-buffer)
(global-set-key "\C-x\C-c" 'close-frame-or-exit)
(define-key global-map "\C-cv" 'sourcepair-load)

;;(global-set-key (kbd "\s-left")  'windmove-left)
;;(global-set-key (kbd "\s-right") 'windmove-right)
;;(global-set-key (kbd "\s-up")    'windmove-up)
;;(global-set-key (kbd "\s-down")  'windmove-down)

(windmove-default-keybindings 'super)

;;extra helm bindssure
;Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-completing-read-handlers-alist
   (quote
    (
     (basic-save-buffer . helm-read-file-name-handler-1)
     (debug-on-entry . helm-completing-read-symbols)
     (describe-function . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (dired-do-copy . helm-read-file-name-handler-1)
     (dired-do-hardlink . helm-read-file-name-handler-1)
     (dired-do-relsymlink . helm-read-file-name-handler-1)
     (dired-do-rename . helm-read-file-name-handler-1)
     (dired-do-symlink . helm-read-file-name-handler-1)
     (disassemble . helm-completing-read-symbols)
     (execute-extended-command)
     (ffap . helm-completing-read-sync-default-handler)
     (ffap-alternate-file)
     (find-file . ido)
     (find-file-read-only . ido)
     (find-file-at-point . helm-completing-read-sync-default-handler)
     (find-function . helm-completing-read-symbols)
     (find-tag . helm-completing-read-default-find-tag)
     (org-capture . helm-org-completing-read-tags)
     (tmm-menubar)
     (trace-function . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (write-file . helm-read-file-name-handler-1)
     (write-region . helm-read-file-name-handler-1)
     (xref-find-definitions . helm-completing-read-default-find-tag)
     (xref-find-references . helm-completing-read-default-find-tag)
     ))
   )
 '(uniquify-buffer-name-style nil nil (uniquify))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

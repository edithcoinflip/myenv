(require 'package)

(setq package-list '(use-package sourcetrail yaml-mode win-switch solarized-theme rainbow-blocks rainbow-delimiters rainbow-identifiers package+ osx-clipboard json-mode js2-mode jedi ivy highlight github-clone github-pullrequest gitconfig gitconfig-mode git-ps1-mode emacs-setup cycle-themes cpp-auto-include cpp-capf cpputils-cmake company-jedi company-irony-c-headers company-irony company-c-headers company cmake-mode cmake-ide c-eldoc bicycle beacon bash-completion avy auto-package-update auto-highlight-symbol auto-complete-nxml auto-complete-clang auto-complete-c-headers auto-complete auto-compile helm-spotify helm-git-grep helm-git helm))

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
(setq custom-safe-themes t)
(load-theme 'solarized-dark)

(global-linum-mode 1)
(column-number-mode 1)
(beacon-mode 1)
(ac-config-default)
(global-auto-complete-mode t)
(helm-mode 1)
 
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

(add-hook 'makefile-mode-hook 'whitespace-mode)

;;custom stuff
(setq vc-follow-symlinks nil)

;;macros
;;macros
(fset 'idir
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 5 134217848 114 101 112 108 97 99 101 45 115 116 114 105 110 103 return 32 return 32 92 17 10 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 return] 0 "%d")) arg)))

;;funcs

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
	((looking-at "\\s)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))


;;key binds
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c g") 'helm-git-grep)
(global-set-key (kbd "C-c C-g") 'helm-git-grep-at-point)
(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
(global-set-key (kbd "C-x C-k 0") 'idir)
(global-set-key "%" 'match-paren)

(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)

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
    ((find-tag . helm-completing-read-default-find-tag)
     (xref-find-definitions . helm-completing-read-default-find-tag)
     (xref-find-references . helm-completing-read-default-find-tag)
     (tmm-menubar)
     (find-file-read-only . ido)
     (find-file . ido)
     (execute-extended-command)
     (dired-do-rename . helm-read-file-name-handler-1)
     (dired-do-copy . helm-read-file-name-handler-1)
     (dired-do-symlink . helm-read-file-name-handler-1)
     (dired-do-relsymlink . helm-read-file-name-handler-1)
     (dired-do-hardlink . helm-read-file-name-handler-1)
     (basic-save-buffer . helm-read-file-name-handler-1)
     (write-file . helm-read-file-name-handler-1)
     (write-region . helm-read-file-name-handler-1))))
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

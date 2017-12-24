(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(setq package-enable-at-startup nil)

; activate all the packages (in particular autoloads)
(package-initialize)
; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; list the packages you want
(setq package-list
      '(
        better-defaults
        solarized-theme

        ;; project management
        projectile
        
        ;; general tools
        flycheck ;; linting
        
        ;; javascript packages
        js2-mode ;; major mode
        js2-refactor ;; refactor support
        xref-js2 ;; find references
        
        ;;enable ido everywhere
        ido-ubiquitous

        ;; M-x autoscomplete
        smex
        
        ;;better package search
        paradox

        ;;task manager
        org
        org-plus-contrib
        org-bullets

        ;; Cloujure Brave and True Config
        ;; makes handling lisp expressions much, much easier
        ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
        paredit

        ;; key bindings and code colorization for Clojure
        ;; https://github.com/clojure-emacs/clojure-mode
        clojure-mode

        ;; extra syntax highlighting for clojure
        clojure-mode-extra-font-locking

        ;; integration with a Clojure REPL
        ;; https://github.com/clojure-emacs/cider
        cider

        ;; colorful parenthesis matching
        rainbow-delimiters

        ;; edit html tags like sexps
        tagedit

        ;; git integration
        magit
        
))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; configure projectile
(projectile-mode)
(setq projectile-indexing-method `alien)

(require 'better-defaults)
(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil)
(load-theme 'solarized-dark t)

;; Javascript config
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(add-to-list 'load-path "~/.emacs.d/customization")

(load "orgmode.el")
(load "magit-load.el" )

;; set terminal to bash
;; (setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
;; (setq shell-file-name "bash")
;; (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
;; (setenv "SHELL" shell-file-name)
;; (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;; smex

(smex-initialize)
(setq smex-save-file "~/.emacs.d/.smex-items")
;; keybindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Clear Scratch and start in org mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Show line numbers
(global-linum-mode)

;; Split vertical on widescreen
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; Set font settings
(set-face-attribute `default nil
                    :family "Fira Code"
                    :height 130)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(load "editing.el")
(load "elisp-editing.el")

(load "clojure.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(org-agenda-files
   (quote
    ("~/notes/personal.org" "~/notes/test.org" "~/notes/notes.org")))
 '(package-selected-packages
   (quote
    (org-bullets magit-gh-pulls htmlize org-plus-contrib ob-clojure cider clojure-mode-extra-font-locking clojure-mode paredit org better-defaults)))
 '(paradox-github-token t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

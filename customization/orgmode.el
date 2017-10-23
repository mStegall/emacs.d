;; org config
(require 'org)
(require 'ob-clojure)

(setq org-directory "~/Notes")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-babel-clojure-backend 'cider)
(require `cider)

(org-babel-do-load-languages 
`org-babel-load-languages
`((clojure .t)))

;; (defun my-org-confirm-babel-evaluate (lang body)
;;   (not (string= lang "clojure")))  
;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-refile-targets (quote (("personal.org" :maxlevel . 1))))

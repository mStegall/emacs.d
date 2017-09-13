;; org config
(setq org-directory "~/Notes")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-refile-targets (quote (("personal.org" :maxlevel . 1))))

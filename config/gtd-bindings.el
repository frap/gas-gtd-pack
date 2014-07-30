;; agasson org-mode GTD bindingss

(require 'org)

(global-set-key (kbd "C-c l")  'org-store-link)
(global-set-key (kbd "C-c a")  'org-agenda)
(global-set-key (kbd "C-c b")  'org-iswitchb)

(global-set-key (kbd "<f1>")   'org-agenda)
(global-set-key (kbd "<f2>")   'org-clock-goto)
(global-set-key (kbd "C-<f2>") 'org-clock-in)

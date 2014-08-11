;; agasson org-mode GTD bindingss


(global-set-key (kbd "C-c l")  'org-store-link)
(global-set-key (kbd "C-c a")  'org-agenda)
(global-set-key (kbd "C-c b")  'org-iswitchb)

(global-set-key (kbd "<f1>")   'org-agenda)
(global-set-key (kbd "<f2>")   'org-clock-goto)
(global-set-key (kbd "C-<f2>") 'org-clock-in)

(require 'org-habit)
(require 'org-helpers)
(require 'org-pomodoro)

(global-set-key (kbd "C-c C-x C-i") 'org-pomodoro)
(global-set-key (kbd "C-c C-x C-o") 'org-pomodoro)

;; Set up abbreviations
(setq org-link-abbrev-alist
       '(("bugzilla"  . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
         ("url-to-fr" . "http://translate.google.fr/translate?sl=en&tl=fr&u=%h")
         ("google"    . "http://www.google.com/search?q=")
         ("gmap"      . "http://maps.google.com/maps?q=%s")
         ("omap"      . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
         ("ads"       . "http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?author=%s&db_key=AST")
         ("ghub"      : "http://github.com/%s")
         ("ghub-pages" : "http://%s.github.io")
         ))
;; sets the default workflow keywords and their faces
(setq org-todo-keywords
      '((sequence "TODO(t)" "RENDEZ_VOUS(r)" "EN_COURS(e)" "|" "FINI(f!/!)")
        (sequence "VALUE(v)" "GOAL(G)"  "|" "FINI(f!/!)")
        (sequence "ATTENTE(w@/!)" "SOUTE(h@/!)" "UN_JOUR(j)" "|" "ANNULÉ(a@/!)" "TÉLÉPHONE")))

(setq org-priority-faces
      '((65 :foreground "#ff2f30" :weight bold)
        (66 :foreground "#ffaf60" :weight bold)
        (67 :foreground "#00dca8" :weight bold)))

(setq org-todo-keyword-faces
      '(("UN_JOUR"       :foreground "#c93f80" :weight bold)
        ("EN_COURS"      :foreground "#2f2ccc" :weight bold)
        ("ATTENTE"       :foreground "#fd9b3b" :weight bold)
        ("FINI"          :foreground "#19b85d" :weight bold)
        ("SOUTE"         :foreground "#afff64" :weight bold)
        ("ANNULÉ"        :foreground "#b81590" :weight bold)
        ("TÉLÉPHONE"     :foreground "#2eb9a7" :weight bold)
        ("GOAL"          :foreground "#1010ff" :weight bold)
        ("VALUE"         :foreground "#afff10" :weight bold)
        ("RENDEZ_VOUS"   :foreground "#0f4f43" :weight bold)
        ))

; change TODO face
(set-face-attribute 'org-todo nil
                    :weight 'bold :box '(:line-width 1 :color "#D80000")
                                        :foreground "#D80000" :background "#000000")

;; sets the TAG list
(setq org-tag-alist '((:startgroup . nil)
                      ("@maision" . ?m)
                      ("@bureau" . ?b)
                      ("@voiture" . ?v)
                      ("@ferme"   . ?f)
                      (:endgroup . nil)
                      ("TÉLÉPHONE" . ?t)
                      ("ATTENTE" . ?w)
                      ("SOUTE" . ?h)
                      ("PERSONAL" . ?P)
                      ("WORK" . ?W)
                      ("FERME" . ?F)
                      ("ORG" . ?O)
                      ("crypt" . ?c)
                      ("NOTE" . ?n)
                      ("ANNULÉ" . ?a)
                      ("GIT" . ?g)
                      ("PROG" . ?p)
                      ("en ligne" . ?e)))

;; setup for org-capture
(setq org-directory "~/Dropbox/GTD")
(setq org-default-notes-file "~/Dropbox/GTD/inbox.org")

;; The default agenda files. inbox.org is used only in custom agenda.
(setq org-agenda-files (list    "~/Dropbox/GTD/gtd.org"
                                "~/Dropbox/GTD/goals.org"
                                "~/Dropbox/GTD/career.org"
                                "/home/agasson/Dropbox/GTD/feedme.org"
                                "~/Dropbox/GTD/calendar.org"))

(setq org-capture-templates
      '(("r" "Todo" entry (file+headline "~/Dropbox/GTD/inbox.org" "Inbox")
         "* TODO %?")
        ("j" "Journal" entry (file+datetree "~/Dropbox/GTD/journal.org")
         (file "~/Dropbox/GTD/templates/review"))))

(define-key global-map "\C-cr"
  (lambda () (interactive) (org-capture nil "r")))
(define-key global-map "\C-cj"
  (lambda () (interactive) (org-capture nil "j")))

;; display the tags farther right
(setq org-agenda-tags-column -102)
;; display the org-habit graph right of the tags
(setq org-habit-graph-column 102)
(setq org-habit-following-days 7)
(setq org-habit-preceding-days 21)

(defun custom-org-agenda-mode-defaults ()
  (electric-indent-mode -1)
  (org-defkey org-agenda-mode-map "W" 'oh/agenda-remove-restriction)
  (org-defkey org-agenda-mode-map "N" 'oh/agenda-restrict-to-subtree)
  (org-defkey org-agenda-mode-map "P" 'oh/agenda-restrict-to-project)
  (org-defkey org-agenda-mode-map "q" 'bury-buffer)
  (org-defkey org-agenda-mode-map "I" 'org-pomodoro)
  (org-defkey org-agenda-mode-map "O" 'org-pomodoro)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-i") 'org-pomodoro)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-o") 'org-pomodoro))

(add-hook 'org-agenda-mode-hook 'custom-org-agenda-mode-defaults 'append)

(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "" nil)
          (alltodo ""
                   ((org-agenda-overriding-header "Tasks to Refile")
                    (org-agenda-files '("~/Dropbox/GTD/inbox.org"))
                    (org-agenda-skip-function
                     '(oh/agenda-skip :headline-if-restricted-and '(todo)))))
          (tags-todo "-ANNULÉ/!-SOUTE-ATTENTE-GOAL"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project habit scheduled deadline)))))
          (tags-todo "-ATTENTE-ANNULÉ/!EN_COURS"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
          (tags-todo "-ANNULÉ/!-EN_COURS-SOUTE-ATTENTE-VALUE-GOAL"
                     ((org-agenda-overriding-header "Available Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :subtree-if-unrestricted-and '(subtask)
                                        :subtree-if-restricted-and '(single-task)))
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-ANNULÉ/!"
                     ((org-agenda-overriding-header "Currently Active Projects")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(non-project stuck-project inactive habit)
                                        :headline-if-unrestricted-and '(subproject)
                                        :headline-if-restricted-and '(top-project)))
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-ANNULÉ/!ATTENTE|SOUTE"
                     ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(project habit))))))
         nil)
        ("r" "Tasks to Refile" alltodo ""
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-agenda-files '("~/.org/inbox.org"))))
        ("#" "Stuck Projects" tags-todo "-ANNULÉ/!-SOUTE-ATTENTE"
         ((org-agenda-overriding-header "Stuck Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project
                                                   habit scheduled deadline)))))
        ("n" "Next Tasks" tags-todo "-ATTENTE-ANNULÉ/!EN_COURS"
         ((org-agenda-overriding-header "Next Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
          (org-tags-match-list-sublevels t)
          (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
        ("R" "Tasks" tags-todo "-ANNULÉ/!-EN_COURS-SOUTE-ATTENTE"
         ((org-agenda-overriding-header "Available Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :headline-if '(project)
                            :subtree-if '(inactive habit scheduled deadline)
                            :subtree-if-unrestricted-and '(subtask)
                            :subtree-if-restricted-and '(single-task)))
          (org-agenda-sorting-strategy '(category-keep))))
        ("p" "Projects" tags-todo "-ANNULÉ/!"
         ((org-agenda-overriding-header "Currently Active Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(non-project inactive habit)))
          (org-agenda-sorting-strategy '(category-keep))
          (org-tags-match-list-sublevels 'indented)))
        ("w" "Waiting Tasks" tags-todo "-ANNULÉ/!ATTENTE|SOUTE"
         ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                    (org-agenda-skip-function '(oh/agenda-skip :subtree-if '(project habit)))))))

;; refiling
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))


(defun gtd ()
  (interactive)
  (find-file "~/Dropbox/GTD/gtd.org"))

;; Clocking shit
(setq gas/keep-clock-running nil)

(defun gas/clock-in-to-next (kw)
  "Switch a task from TODO to EN_COURS when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (oh/is-task-p))
      "EN_COURS")
     ((and (member (org-get-todo-state) (list "EN_COURS"))
           (oh/is-project-p))
      "TODO"))))


(defun gas/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq gas/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (gas/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (gas/clock-in-organization-task-as-default)))))

(defun gas/punch-out ()
  (interactive)
  (setq gas/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun gas/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun gas/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when gas/keep-clock-running
            (gas/clock-in-default-task)))))))

(defvar gas/organization-task-id "EA0E8723-0480-450F-8224-66438AC996E0")

(defun gas/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find gas/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun gas/clock-out-maybe ()
  (when (and gas/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (gas/clock-in-parent-task)))


(defun gas/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; Consider only tasks with done todo headings as archivable candidates
      (if (member (org-get-todo-state) org-done-keywords)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (daynr (string-to-int (format-time-string "%d" (current-time))))
                 (a-month-ago (* 60 60 24 (+ daynr 1)))
                 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                 (this-month (format-time-string "%Y-%m-" (current-time)))
                 (subtree-is-current (save-excursion
                                       (forward-line 1)
                                       (and (< (point) subtree-end)
                                            (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
            (if subtree-is-current
                next-headline ; Has a date in this month or last month, skip it
              nil))  ; available to archive
        (or next-headline (point-max))))))

'(org-clock-in-switch-to-state 'gas/clock-in-to-next)

;;Archivable defaults
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

;; Estimates
 ; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:25 0:50 2:00 4:00 8:00 16:00 24:00 32:00 40:00 80:00")
                                                                        ("STYLE_ALL" . "habit"))))

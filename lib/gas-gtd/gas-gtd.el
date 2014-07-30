;; my (agasson) GTD config

(require 'org)

(require 'org-habit)
(require 'org-helpers)

(require 'org-pomodoro)

;; automatically mark a todo headline as done
;; when all sub-checkboxes are checked
(add-hook 'org-checkbox-statistics-hook 'oh/summary-todo-checkbox)

;; sets the default workflow keywords and their faces
(setq org-todo-keywords
      '((sequence "TODO(t)" "RENDEZ_VOUS(r)" "EN_COURS(e)" "|" "FINI(f!/!)")
        (sequence "VALUE(v)" "GOAL(G)"  "|" "FINI(f!/!)")
        (sequence "ATTENTE(w@/!)" "SOUTE(h@/!)" "UN_JOUR(j)" "|" "ANNULÉ(a@/!)" "TÉLÉPHONE")))

(setq org-priority-faces
      '((65 :foreground "#ff2f30" :weight bold)
        (66 :foreground "#ffaf60" :weight bold)
        (67 :foreground "#ffdca8" :weight bold)))

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

;; sets the
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

;; The default agenda files. inbox.org is used only in custom agenda.
(setq org-agenda-files (quote ( "~/Dropbox/GTD/gtd.org"
                                "~/Dropbox/GTD/goals.org"
                                "~/Dropbox/GTD/career.org"
                                "~/Dropbox/GTD/feedme.org"
                                "~/Dropbox/GTD/calendar.org")))

;; my org settings

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

(custom-set-variables
 '(org-log-done t)
 ;; Allows changing from any TODO state to any other using key from fast todo selection
 '(org-use-fast-todo-selection t)
 ;; allows chaniging todo states with S-left S-right skipping all normal processing when entering/leaving a TODO state (no settign of timestamps!)
 '(org-treat-S-cursor-todo-selection-as-state-change nil)

 '(org-agenda-start-on-weekday nil)
 '(org-agenda-ndays 1)
  '(org-agenda-window-setup 'current-window)
 '(org-agenda-repeating-timestamp-show-all t)
 ;; Show all agenda dates - even if they are empty
 '(org-agenda-show-all-dates t)
 ;; Sorting order for tasks on the agenda
 '(org-agenda-sorting-strategy
   (quote ((agenda
            habit-down
            time-up
            user-defined-up
            priority-down
            effort-up
            category-keep)
           (todo priority-down category-up effort-up)
           (tags priority-down effort-up category-up)
           (search category-up))))
 '(org-agenda-cmp-user-defined 'oh/agenda-sort)
 ;; Keep tasks with dates on the global todo lists
 '(org-agenda-todo-ignore-with-date nil)
 ;; Keep tasks with deadlines on the global todo lists
 '(org-agenda-todo-ignore-deadlines nil)
 ;; Keep tasks with scheduled dates on the global todo lists
 '(org-agenda-todo-ignore-scheduled nil)
 ;; Keep tasks with timestamps on the global todo lists
 '(org-agenda-todo-ignore-timestamp nil)
 ;; Remove completed deadline tasks from the agenda view
 '(org-agenda-skip-deadline-if-done t)
 ;; Remove completed scheduled tasks from the agenda view
 '(org-agenda-skip-scheduled-if-done t)
 ;; Remove completed items from search results
 '(org-agenda-skip-timestamp-if-done t)
 ;; Show lot sof clocking history so it's easy to pick items off the C-F11 list
 '(org-clock-history-length 36)
 ;; Change tasks to NEXT when clocking in
 '(org-clock-in-switch-to-state 'gas/clock-in-to-next)

 ;; Clock out when moving task to a done state
 '(org-clock-out-when-done t)

 ;; Separate drawers for clocking and logs
 '(org-drawers (quote ("PROPERTIES" "LOGBOOK")))

 ;; Save clock data and state changes and notes in the LOGBOOK drawer
 '(org-clock-into-drawer t)

 ;; Sometimes I change tasks I'm clocking quickly
 ;; this removes clocked tasks with 0:00 duration
 '(org-clock-out-remove-zero-time-clocks t)

 ;; Save the running clock and all clock history when exiting Emacs, load it on startup
 '(org-clock-persist t)

 ;; Do not prompt to resume an active clock
 '(org-clock-persist-query-resume nil)

 ;; Enable auto clock resolution for finding open clocks
 '(org-clock-auto-clock-resolution (quote when-no-clock-is-running))

 ;; Include current clocking task in clock reports
 '(org-clock-report-include-clocking-task t)

 ;; So dont have to press RET to to exit fast tag selection - have to pres C-c to get window
 '(org-fast-tag-selection-single-key 'expert)

 '(org-agenda-skip-scheduled-if-done t)

 ;; Display tags farther right
 '(org-agenda-tags-column -102)

 ;; Enable display of the time grid
 ;; so we can see the marker for the current time
 '(org-agenda-time-grid (quote ((daily today remove-match)
                                #("----------------" 0 16 (org-heading t))
                                (830 1000 1200 1300 1500 1700))))

 ;; include diary elements
 '(org-agenda-include-diary t)

 ;; Do not dim blocked tasks
 '(org-agenda-dim-blocked-tasks nil))

;; setup for org-capture
(setq org-directory "~/Dropbox/GTD")
(setq org-default-notes-file "~/Dropbox/GTD/refile.org")

;; I use C-M-r to start capture mode
(global-set-key (kbd "C-M-r") 'org-capture)

(setq org-capture-templates
      '(("t" "todo" entry (file "~/Dropbox/GTD/refile.org")
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("r" "respond" entry (file "~/Dropbox/GTD/refile.org")
         "* EN COURS Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "note" entry (file "~/Dropbox/GTD/refile.org")
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("g" "Add Goal" entry (file "~/Dropbox/GTD/goals.org")
         "* GOAL %? :GOAL:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("j" "Journal Entry" entry (file+datetree "~/Dropbox/GTD/calendar.org")
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("w" "Review" entry (file "~/Dropbox/GTD/refile.org")
         "* TODO Review %c\n%U\n" :immediate-finish t)
        ("p" "Phone call" entry (file "~/Dropbox/GTD/refile.org")
         "* TÉLÉPHONE %? :TÉLÉPHONE:\n%U" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file "~/Dropbox/GTD/refile.org")
                        "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

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

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;;;; Refile settings
; Exclude FINI state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;                                                 ;;
;; (define-key global-map "\C-cr"                     ;;
;;   (lambda () (interactive) (org-capture nil "r"))) ;;
;; (define-key global-map "\C-cj"                     ;;
;;   (lambda () (interactive) (org-capture nil "j"))) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

(setq org-habit-graph-column 102)
(setq org-habit-following-days 7)
(setq org-habit-preceding-days 21)

;; Some keybindings that should be activated in org-mode
(defun custom-org-agenda-mode-defaults ()
  (org-defkey org-agenda-mode-map "W" 'oh/agenda-remove-restriction)
  (org-defkey org-agenda-mode-map "N" 'oh/agenda-restrict-to-subtree)
  (org-defkey org-agenda-mode-map "P" 'oh/agenda-restrict-to-project)
  (org-defkey org-agenda-mode-map "q" 'bury-buffer)
  (org-defkey org-agenda-mode-map "I" 'org-pomodoro)
  (org-defkey org-agenda-mode-map "O" 'org-pomodoro)
 ; (org-defkey org-agenda-mode-map (kbd "C-c C-x C-i") 'org-pomodoro)
 ; (org-defkey org-agenda-mode-map (kbd "C-c C-x C-o") 'org-pomodoro)
  )

(add-hook 'org-agenda-mode-hook 'custom-org-agenda-mode-defaults 'append)

;; configure org remember functions and hooks
(setq remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "" nil)
          (tags-todo "-ATTENTE-ANNULÉ-{^[15]yr.*}/!GOAL"
                ((org-agenda-overriding-header "Objectifs:")
                 (org-agenda-skip-function
                     '(oh/agenda-skip
                                        :subtree-if '(inactive habit deadline)
                                        ))
                 (org-tags-match-list-sublevels t)))
          (tags-todo "-ATTENTE-ANNULÉ/!EN_COURS"
                     ((org-agenda-overriding-header "Tâches Suivant")
                      (org-agenda-skip-function
                       '(oh/agenda-skip
                                        :subtree-if '(inactive habit scheduled deadline)
                                        ))
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy '(priority-down effort-up))))
          (tags "REFILE"
                ((org-agenda-overriding-header "Remanier les Tâches")
                 (org-tags-match-list-sublevels nil)))
          (tags-todo "-ANNULÉ/!-SOUTE-ATTENTE"
                     ((org-agenda-overriding-header "Projets Bloquès")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(non-project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :headline-if-restricted-and '(non-stuck-project)
                                        :subtree-if-unrestricted-and '(non-stuck-project)))
                      (org-agenda-sorting-strategy '(priority-down effort-down))))
          (tags-todo "-ANNULÉ/!-EN_COURS-SOUTE-ATTENTE"
                     ((org-agenda-overriding-header "Tâches Actif")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :subtree-if-unrestricted-and '(subtask)
                                        :subtree-if-restricted-and '(single-task)))
                      (org-agenda-sorting-strategy '(priority-down effort-down))))
          (tags-todo "-ANNULÉ/!"
                     ((org-agenda-overriding-header "Les Projets actuellement actifs")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(non-project stuck-project inactive habit)
                                        :headline-if-unrestricted-and '(subproject)
                                        :headline-if-restricted-and '(top-project)))
                      (org-agenda-sorting-strategy '(priority-down))))
          (tags-todo "-ANNULÉ/!ATTENTE|SOUTE"
                     ((org-agenda-overriding-header "D'attente et reporté tâches")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(project habit)))))
          (tags "-REFILE/"
                ((org-agenda-overriding-header "Tasks to Archive")
                 (org-agenda-skip-function 'gas/skip-non-archivable-tasks)
                                        (org-tags-match-list-sublevels nil))))
         nil)
        ("r" "Tasks to Refile" alltodo ""
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-agenda-files '("~/Dropbox/GTD/refile.org"))))
        ("#" "Stuck Projects" tags-todo "-ANNULÉ/!-SOUTE-ATTENTE"
         ((org-agenda-overriding-header "Projets Bloqués")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project
                                          habit scheduled deadline)))))
        ("n" "Next Tasks" tags-todo "-ATTENTE-ANNULÉ/!EN_COURS"
         ((org-agenda-overriding-header "Tâches Suivant")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
          (org-tags-match-list-sublevels t)
          (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
        ("R" "Tasks" tags-todo "-ANNULÉ/!-EN_COURS-SOUTE-ATTENTE"
         ((org-agenda-overriding-header "Tâches Actif")
          (org-agenda-skip-function
           '(oh/agenda-skip :headline-if '(project)
                            :subtree-if '(inactive habit scheduled deadline)
                            :subtree-if-unrestricted-and '(subtask)
                            :subtree-if-restricted-and '(single-task)))
          (org-agenda-sorting-strategy '(priority-up category-keep))))
        ("p" "Projects" tags-todo "-ANNULÉ/!"
         ((org-agenda-overriding-header "Les Projets actuellement actifs")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(non-project inactive habit)))
          (org-agenda-sorting-strategy '(category-keep))
          (org-tags-match-list-sublevels 'indented)))
        ("w" "Waiting Tasks" tags-todo "-ANNULÉ/!ATTENTE|SOUTE"
         ((org-agenda-overriding-header "Waiting and Postponed Tasks")
          (org-agenda-skip-function '(oh/agenda-skip :subtree-if '(project habit)))))))

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

(defun custom-org-mode-defaults ()
  (electric-indent-mode -1)
  (org-defkey org-mode-map (kbd "M-p") 'org-metaup)
  (org-defkey org-mode-map (kbd "M-n") 'org-metadown)
  (org-defkey org-mode-map (kbd "C-c C-x C-p") 'org-pomodoro)
;  (org-defkey org-mode-map (kbd "C-c C-x C-o") 'org-pomodoro)
  )


(add-hook 'org-mode-hook 'custom-org-mode-defaults)

(provide 'gas-gtd)

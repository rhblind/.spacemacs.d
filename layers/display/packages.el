;;; Display Layer -*- lexical-binding: t; -*-

(setq display-packages
      '(;; Owned packages
        all-the-icons
        all-the-icons-ivy
        all-the-icons-dired
        ;; pretty-mode

        ;; org-mode visuals
        doct
        org-fancy-priorities
        org-super-agenda
        org-superstar

        solarized-theme
        (prettify-utils :location (recipe :fetcher github
                                          :repo "Ilazki/prettify-utils.el"))

        ;; Elsehwere-owned packages
        spaceline-all-the-icons
        which-key

        ;; Personal display-related packages
        ;; (pretty-code     :location local)
        (pretty-eshell   :location local)
        (pretty-fonts    :location local)
        (pretty-magit    :location local)
        (pretty-outlines :location local)))

;;; Owned Packages
;;;; All-the-icons

(defun display/post-init-all-the-icons ()
  (use-package all-the-icons
    :config
    (let ((hy-icon '(all-the-icons-fileicon "hy" :face all-the-icons-orange))
          (dt-icon '(all-the-icons-fileicon "graphviz" :face all-the-icons-pink)))
      (add-to-list 'all-the-icons-icon-alist      `("\\.hy$"          ,@hy-icon))
      (add-to-list 'all-the-icons-icon-alist      `("\\.dot$"         ,@dt-icon))
      (add-to-list 'all-the-icons-mode-icon-alist `(hy-mode           ,@hy-icon))
      (add-to-list 'all-the-icons-mode-icon-alist `(graphviz-dot-mode ,@dt-icon)))))

;;;; All-the-icons-ivy

(defun display/init-all-the-icons-ivy ()
  (use-package all-the-icons-ivy
    :config
    (progn
      ;; Fix icon prompt alignment in ivy prompts
      (advice-add 'all-the-icons-ivy-file-transformer :override
                  'all-the-icons-ivy-file-transformer-stdized)

      ;; Add behavior to counsel projectile funcs too
      (advice-add 'counsel-projectile-find-file-transformer :filter-return
                  'all-the-icons-ivy-file-transformer-stdized)
      (advice-add 'counsel-projectile-transformer :filter-return
                  'all-the-icons-ivy-file-transformer-stdized)

      (all-the-icons-ivy-setup))))

;;;; All-the-icons-dired

(defun display/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)))

;;;; Declarative Org Capture Templates (DOCT)

(defun display/init-doct ()
  (use-package doct
    :ensure t
    :commands (doct)
    :after (org-capture)
    :init
    (setq org-capture-templates
          ;; https://github.com/progfolio/doct#manual
          ;; https://orgmode.org/manual/Template-expansion.html#Template-expansion
          (doct `(
                  (,(format "%s\tTasks" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
                   :keys "k"
                   :headline "Tasks"
                   :prepend t
                   :type entry
                   :file org-default-notes-file
                   :children ((,(format "%s\tGeneral task" (all-the-icons-octicon "checklist" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "k"
                               :template ("* TODO %? %^G"
                                          "%i"))
                              (,(format "%s\tCapture point task" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "c"
                               :template ("* TODO %? %^G"
                                          "%i %a"))
                              (,(format "%s\tPersonal note" (all-the-icons-faicon "sticky-note-o" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "n"
                               :headline "Notes"
                               :template ("* %? :personal:"
                                          "%i"))
                              (,(format "%s\tTask with deadline" (all-the-icons-material "timer" :face 'all-the-icons-red :v-adjust -0.1))
                               :keys "d"
                               :template ("* TODO %? %^G%{extra}"
                                          "%i %a")
                               :extra "\nDEADLINE: %^{Due date:}T")
                              (,(format "%s\tScheduled task" (all-the-icons-octicon "calendar" :face 'all-the-icons-red :v-adjust 0.01))
                               :keys "s"
                               :template ("* TODO %? %^G%{extra}"
                                          "%i %a")
                               :extra "\nSCHEDULED: %^{Task date}t")
                              ))
                  (,(format "%s\tWork" (all-the-icons-faicon "building" :face 'all-the-icons-purple :v-adjust 0.01))
                   :keys "w"
                   :headline "Tasks"
                   :prepend t
                   :type entry
                   :file org-work-file
                   :children ((,(format "%s\tMiscellaneous task" (all-the-icons-octicon "checklist" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "k"
                               :template ("* TODO [#C] %? %^G:work:"
                                          "%i"))
                              (,(format "%s\tWork note" (all-the-icons-faicon "sticky-note-o" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "n"
                               :headline "Notes"
                               :template ("* %? :work:"
                                          "%i"))
                              (,(format "%s\tTask with deadline" (all-the-icons-material "timer" :face 'all-the-icons-red :v-adjust -0.1))
                               :keys "d"
                               :template ("* TODO [#B] %? %^G:work:%{extra}"
                                          "%i")
                               :extra "\nDEADLINE: %^{Due date:}T")
                              (,(format "%s\tScheduled task" (all-the-icons-octicon "calendar" :face 'all-the-icons-red :v-adjust 0.01))
                               :keys "s"
                               :template ("* TODO [#C] %? %^G:work:%{extra}"
                                          "%i")
                               :extra "\nSCHEDULED: %^{Task date}t")))
                  (,(format "%s\tEmail" (all-the-icons-faicon "envelope" :face 'all-the-icons-blue :v-adjust 0.01))
                   :keys "e"
                   :headline "Tasks"
                   :prepend t
                   :type entry
                   :file org-default-notes-file
                   :template ("* TODO %? :email:"
                              "%i %a"))
                  (,(format "%s\tInteresting" (all-the-icons-faicon "eye" :face 'all-the-icons-lcyan :v-adjust 0.01))
                   :keys "i"
                   :headline "Interesting"
                   :prepend t
                   :type entry
                   :file org-default-notes-file
                   :template ("* [ ] %{desc}%? :%{i-type}:%^G"
                              "%i")
                   :children ((,(format "%s\tWebpage" (all-the-icons-faicon "globe" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "w"
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web"
                               )
                              (,(format "%s\tArticle" (all-the-icons-octicon "file-text" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "a"
                               :desc ""
                               :i-type "read:research"
                               )
                              (,(format "%s\tInformation" (all-the-icons-faicon "info-circle" :face 'all-the-icons-blue :v-adjust 0.01))
                               :keys "i"
                               :desc ""
                               :i-type "read:info"
                               )
                              (,(format "%s\tIdea" (all-the-icons-material "bubble_chart" :face 'all-the-icons-silver :v-adjust 0.01))
                               :keys "I"
                               :desc ""
                               :i-type "idea"
                               )))

                  (,(format "%s\tProject" (all-the-icons-octicon "repo" :face 'all-the-icons-silver :v-adjust 0.01))
                   :keys "p"
                   :headline "Tasks"
                   :prepend t
                   :type entry
                   :file org-projects-file
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :custom (:time-or-todo "")
                   :children ((,(format "%s\tProject todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                               :keys "t"
                               :time-or-todo "TODO")
                              (,(format "%s\tProject note" (all-the-icons-faicon "sticky-note" :face 'all-the-icons-yellow :v-adjust 0.01))
                               :keys "n"
                               :template "* %? %^G")
                              (,(format "%s\tProject changelog" (all-the-icons-faicon "list" :face 'all-the-icons-blue :v-adjust 0.01))
                               :keys "l"
                               :time-or-todo "%U"
                               :heading "Unreleased")
                              ;; (,(format "%s\tCurrent project" (all-the-icons-faicon "sticky-note" :face 'all-the-icons-red :v-adjust 0.01))
                              ;;  :keys "p"
                              ;;  :headline "Tasks"
                              ;;  :prepend t
                              ;;  :file org-projectile-per-project-filepath
                              ;;  :children ((,(format "%s\tCapture point task" (all-the-icons-faicon "code" :face 'all-the-icons-red :v-adjust 0.01))
                              ;;              :keys "c"
                              ;;              :template ("* %? %^G"
                              ;;                         "%i"
                              ;;                         "%a")
                              ;;              ))
                              ;;  )
                              )
                   ))
                ))
    :config
    (progn
      (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)
      (advice-add 'org-mks :override #'org-mks-pretty)))
  )

;;;; Org-fancy-priorities

(defun display/init-org-fancy-priorities ()
  (use-package org-fancy-priorities
    :ensure t
    :diminish
    :defines org-fancy-priorities-list
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-priority-faces '((?A . all-the-icons-red)
                               (?B . all-the-icons-yellow)
                               (?C . all-the-icons-blue))
          org-fancy-priorities-list '(
                                      (?A . "⬆")   ;; High
                                      (?B . "■")   ;; Medium
                                      (?C . "⬇"))) ;; Low

    ;; FIXME Cannot make org-priority work with more than three priorities..
    ;; (setq org-priority-faces '((?A . all-the-icons-red)
    ;;                            (?B . all-the-icons-orange)
    ;;                            (?C . all-the-icons-yellow)
    ;;                            (?D . all-the-icons-green)
    ;;                            (?E . all-the-icons-blue))
    ;;       org-priority-highest ?A
    ;;       org-priority-default ?D
    ;;       org-priority-lowest ?E)
    ;; (setq org-fancy-priorities-list '((?A . "⚑")     ;; ASAP
    ;;                                   (?B . "⬆")     ;; High
    ;;                                   (?C . "■")     ;; Medium
    ;;                                   (?D . "⬇")     ;; Low
    ;;                                   (?E . "❓")))  ;; Optional
    ;; (unless (char-displayable-p ?❓)
    ;;   (setq org-fancy-priorities-list '("ASAP" "HIGH" "MID" "LOW" "OPTIONAL")))
    ))

;;;; Org-super-agenda

(defun display/init-org-super-agenda ()
  (use-package org-super-agenda
    :ensure t
    :commands (org-super-agenda-mode)
    :hook (org-agenda-mode . org-super-agenda-mode)
    :init
    (setq org-agenda-block-separator 9472      ;; Use a straight line as separator between agenda agenda blocks
          org-agenda-compact-blocks t
          org-agenda-include-deadlines t       ;; Include deadlines in the agenda
          org-agenda-skip-deadline-if-done t   ;; Don't include deadlines in the agenda if they're in the `DONE' state
          org-agenda-skip-scheduled-if-done t  ;; Don't include items in the agenda if they're in the `DONE' state
          org-super-agenda-header-map nil      ;; Fixes issues with evil-mode
          )
    (setq org-agenda-custom-commands
          '(("o" "Overview"
             ((agenda "" ((org-agenda-span 'day)
                          (org-super-agenda-groups
                           '((:name "Today"
                                    :time-grid t
                                    :date today
                                    :todo "TODAY"
                                    :scheduled today
                                    :order 1)))))
              (alltodo "" ((org-agenda-overriding-header "")
                           (org-super-agenda-groups
                            '((:name "Next to do"
                                     :todo "NEXT"
                                     :order 1)
                              (:name "Important"
                                     :tag "Important"
                                     :priority "A"
                                     :order 6)
                              (:name "Due Today"
                                     :deadline today
                                     :order 2)
                              (:name "Due Soon"
                                     :deadline future
                                     :order 8)
                              (:name "Overdue"
                                     :deadline past
                                     :face error
                                     :order 7)
                              (:name "Work"
                                     :tag "Work"
                                     :order 10)
                              (:name "Personal"
                                     :tag "Personal"
                                     :order 11)
                              (:name "Issues"
                                     :tag "Issue"
                                     :order 12)
                              (:name "Projects"
                                     :tag "Project"
                                     :order 14)
                              (:name "Emacs"
                                     :tag "Emacs"
                                     :order 13)
                              (:name "Research"
                                     :tag "Research"
                                     :order 15)
                              (:name "To read"
                                     :tag "Read"
                                     :order 30)
                              (:name "Waiting"
                                     :todo "WAITING"
                                     :priority "C"
                                     :order 20)
                              (:name "Trivial"
                                     :priority<= "C"
                                     :tag ("Trivial" "Unimportant")
                                     :todo ("SOMEDAY" )
                                     :order 90)
                              (:discard (:tag ("Chore" "Routine" "Daily")))))))))))))

;;;; Org-superstar

(defun display/pre-init-org-superstar ()
  (use-package org-superstar
    :ensure t
    :hook (org-mode . org-superstar-mode)
    :config
    (setq org-superstar-prettify-item-bullets t
          org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))))

;;;; Pretty-mode

(defun display/init-pretty-mode ()
  ;; I *only* use greek letter replacements at the moment.
  ;; However, I go back and forht on whether to use nil-like <-> emptyset.
  ;; I currently have it *enabled*. Uncomment the deactivation to remove it.

  (use-package pretty-mode
    :config
    (progn
      (global-pretty-mode t)

      (pretty-deactivate-groups
       '(:equality :ordering :ordering-double :ordering-triple
                   :arrows :arrows-twoheaded :punctuation
                   :logic :sets
                   ;; :nil
                   ))
      (pretty-activate-groups
       '(:greek)))))

;;;; Prettify-utils

(defun display/init-prettify-utils ()
  (use-package prettify-utils))

;;;; Solarized-theme

(defun display/init-solarized-theme ()
  (use-package solarized-theme))

;;; Unowned Packages
;;;; Which-key

(defun display/post-init-which-key ()
  (when (configuration-layer/package-used-p 'pretty-fonts)
    (setq which-key-separator " ")
    (setq which-key-prefix-prefix " ")))

;;;; Spaceline-all-the-icons

(defun display/post-init-spaceline-all-the-icons ()
  (spaceline-all-the-icons-theme)

  (setq spaceline-highlight-face-func 'spaceline-highlight-face-default)

  (setq spaceline-all-the-icons-icon-set-modified         'chain)
  (setq spaceline-all-the-icons-icon-set-window-numbering 'square)
  (setq spaceline-all-the-icons-separator-type            'none)
  (setq spaceline-all-the-icons-primary-separator         "·")

  ;; !!!!!!!!!!!!!!!!
  ;; !! https://github.com/domtronn/spaceline-all-the-icons.el/issues/55
  ;; !! If you remove this - expect EXTREMELY degraded performance
  ;; !! on files of more-or-less any size and of any type
  ;; !!!!!!!!!!!!!!!!
  (spaceline-toggle-projectile-root-off)
  (spaceline-toggle-all-the-icons-projectile-off)
  (spaceline-toggle-all-the-icons-buffer-id-off)


  ;; Mode Segments
  (spaceline-toggle-all-the-icons-minor-modes-off)

  ;; Buffer Segments
  (spaceline-toggle-all-the-icons-buffer-size-off)
  (spaceline-toggle-all-the-icons-buffer-position-on)
  (spaceline-toggle-all-the-icons-buffer-id-on)

  ;; Git Segments
  (spaceline-toggle-all-the-icons-git-status-on)
  (spaceline-toggle-all-the-icons-vc-icon-off)
  (spaceline-toggle-all-the-icons-vc-status-on)

  ;; Misc Segments
  (spaceline-toggle-all-the-icons-eyebrowse-workspace-off)
  (spaceline-toggle-all-the-icons-flycheck-status-off)
  (spaceline-toggle-all-the-icons-time-on))

;;; Pretty Packages

;;;; Pretty-code

(defun display/init-pretty-code ()
  (use-package pretty-code
    :config
    (progn
      (pretty-code-add-hook 'emacs-lisp-mode-hook '((:def "defun")))
      (pretty-code-add-hook 'hy-mode-hook         '((:def "defn")
                                                    (:lambda "fn")))
      (pretty-code-add-hook 'python-mode-hook     '((:def "def")
                                                    (:lambda "lambda"))))))

;;;; Pretty-eshell

(defun display/init-pretty-eshell ()
  (use-package pretty-eshell
    :init
    (progn
      ;; Change default banner message
      (setq eshell-banner-message (s-concat (s-repeat 20 "---") "\n\n"))

      ;; More prompt styling
      (setq pretty-eshell-header "\n︳")
      (setq pretty-eshell-prompt-string " "))

    :config
    (progn
      ;; Directory
      (pretty-eshell-section
       esh-dir
       "\xf07c"  ; 
       (abbreviate-file-name (eshell/pwd))
       '(:foreground "#268bd2" :bold bold :underline t))

      ;; Git Branch
      (pretty-eshell-section
       esh-git
       "\xe907"  ; 
       (magit-get-current-branch)
       '(:foreground "#8D6B94"))

      ;; Python Virtual Environment
      (pretty-eshell-section
       esh-python
       "\xe928"  ; 
       pyvenv-virtual-env-name)

      ;; Time
      (pretty-eshell-section
       esh-clock
       "\xf017"  ; 
       (format-time-string "%H:%M" (current-time))
       '(:foreground "forest green"))

      ;; Prompt Number
      (pretty-eshell-section
       esh-num
       "\xf0c9"  ; 
       (number-to-string pretty-eshell-prompt-num)
       '(:foreground "brown"))

      (setq pretty-eshell-funcs
            (list esh-dir esh-git esh-python esh-clock esh-num)))))

;;;; Pretty-fonts

(defun display/init-pretty-fonts ()
  (use-package pretty-fonts
    :config
    ;; !! This is required to avoid segfault when using emacs as daemon !!
    (spacemacs|do-after-display-system-init
     (pretty-fonts-add-hook 'prog-mode-hook pretty-fonts-fira-code-alist)
     (pretty-fonts-add-hook 'org-mode-hook  pretty-fonts-fira-code-alist)

     (pretty-fonts-set-fontsets-for-fira-code)
     (pretty-fonts-set-fontsets
      '(;; All-the-icons fontsets
        ("fontawesome"
         ;;                         
         #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

        ("all-the-icons"
         ;;    
         #xe907 #xe928)

        ("github-octicons"
         ;;                               
         #xf091 #xf059 #xf076 #xf075 #xe192  #xf016 #xf071)

        ("material icons"
         ;;              
         #xe871 #xe918 #xe3e7  #xe5da
         ;;              
         #xe3d0 #xe3d1 #xe3d2 #xe3d4))))))

;;;; Pretty-magit

(defun display/init-pretty-magit ()
  (use-package pretty-magit
    :config
    (progn
      (pretty-magit-add-leaders
       '(("Feature" ? (:foreground "slate gray" :height 1.2))
         ("Add"     ? (:foreground "#375E97" :height 1.2))
         ("Fix"     ? (:foreground "#FB6542" :height 1.2))
         ("Clean"   ? (:foreground "#FFBB00" :height 1.2))
         ("Docs"    ? (:foreground "#3F681C" :height 1.2))))

      (pretty-magit-setup))))

;;;; Pretty-outlines

(defun display/init-pretty-outlines ()
  (use-package pretty-outlines
    :hook ((outline-mode       . pretty-outlines-set-display-table)
           (outline-minor-mode . pretty-outlines-set-display-table)
           (elixir-mode     . pretty-outlines-add-bullets)
           (emacs-lisp-mode . pretty-outlines-add-bullets)
           (hy-mode         . pretty-outlines-add-bullets)
           (python-mode     . pretty-outlines-add-bullets))))

;;; pretty-org.el --- Prettify Org stuff -*- lexical-binding: t; -*-

;;; Commentary:

;; Prettifies the org-agenda and org-agenda capture templates using
;; org-super-agenda and some internet copying from
;; https://github.com/tecosaur/emacs-config/compare/6bcdbaa..49c790e

;;; Config:
;;;; Readability
;;;;; Step 1 - Configure faces for Org mode headlines

;; (setq org-ellipsis ""
;;       org-startup-indented nil
;;       org-indent-indentation-per-level 1
;;       org-hide-emphasis-markers t
;;       org-hide-leading-stars t)

;; Set up a nice proportional font, in different sizes for the headlines.
;; Listed fonts will be tried in sequence, and first match will be used.
(let* ((variable-tuple
        (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;;;;; Step 2 - Setting up variable-pitch and fixed-pitch faces

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light))))
 '(fixed-pitch ((t ( :family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal)))))
(add-hook 'org-mode-hook 'variable-pitch-mode)

;;;;; Step 3 - (Optional) Use long lines and visual-line-mode

;; Use long lines and visual-line-mode. When using proportional fonts,
;; fill-paragraph doesn't make much sense anymore, since it's based on the
;; number of characters on any given line. Use `M-q' to readjust current paragraph!
;; (add-hook 'org-mode-hook 'visual-line-mode)

;;;;; Step 4 - Configure faces for specific Org elements

;; All the changes above should give nice proportional fonts in Org files.
;; However, there are certain things we still wish to use monospaced
;; fonts for (such as code blocks, examples, tags, etc.).
(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;;;; Declarative Org Capture Templates (DOCT)

(use-package doct
  :ensure t
  :commands (doct)
  ;; :after (org org-capture)
  ;; :after (org org-capture)
;;;;; :init
  :init
  (setq org-capture-templates
        (doct `((,(format "%s\tPersonal todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                 :keys "t"
                 :file org-default-notes-file
                 :prepend t
                 :headline "Inbox"
                 :type entry
                 :template ("* TODO %?"
                            "%i %a")
                 )
                (,(format "%s\tPersonal note" (all-the-icons-faicon "sticky-note-o" :face 'all-the-icons-green :v-adjust 0.01))
                 :keys "n"
                 :file org-default-notes-file
                 :prepend t
                 :headline "Inbox"
                 :type entry
                 :template ("* %?"
                            "%i %a")
                 )
                (,(format "%s\tWork" (all-the-icons-faicon "building" :face 'all-the-icons-purple :v-adjust 0.01))
                 :keys "w"
                 :file org-default-notes-file
                 :headline "Work"
                 :prepend t
                 :type entry
                 :children ((,(format "%s\tTask" (all-the-icons-faicon "tasks" :face 'all-the-icons-red :v-adjust 0.01))
                             :keys "t"
                             :template ("* TODO [#C] %? :work:tasks:"
                                        "SCHEDULED: %^{Task date:}T"
                                        "%i %a"))
                            (,(format "%s\tAssignment" (all-the-icons-material "timer" :face 'all-the-icons-orange :v-adjust 0.01))
                             :keys "a"
                             :template ("* TODO [#B] %? :work:assignments:"
                                        "DEADLINE: %^{Due date:}T"
                                        "%i %a"))
                            (,(format "%s\tMiscellaneous task" (all-the-icons-faicon "random" :face 'all-the-icons-yellow :v-adjust 0.01))
                             :keys "m"
                             :template ("* TODO [#C] %? :work:"
                                        "%i %a"))))
                (,(format "%s\tEmail" (all-the-icons-faicon "envelope" :face 'all-the-icons-blue :v-adjust 0.01))
                 :keys "e"
                 :file org-default-notes-file
                 :prepend t
                 :headline "Inbox"
                 :type entry
                 :template ("* TODO %? :email:"
                            "%i %a"))
                (,(format "%s\tInteresting" (all-the-icons-faicon "eye" :face 'all-the-icons-lcyan :v-adjust 0.01))
                 :keys "i"
                 :file org-default-notes-file
                 :prepend t
                 :headline "Interesting"
                 :type entry
                 :template ("* [ ] %{desc}%? :%{i-type}:"
                            "%i %a")
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
                (,(format "%s\tTasks" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
                 :keys "k"
                 :file org-default-notes-file
                 :prepend t
                 :headline "Tasks"
                 :type entry
                 :template ("* TODO %? %^G%{extra}"
                            "%i")
                 :children ((,(format "%s\tGeneral Task" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
                             :keys "k"
                             :extra ""
                             )
                            (,(format "%s\tTask with deadline" (all-the-icons-material "timer" :face 'all-the-icons-orange :v-adjust -0.1))
                             :keys "d"
                             :extra "\nDEADLINE: %^{Deadline:}t"
                             )
                            (,(format "%s\tScheduled Task" (all-the-icons-octicon "calendar" :face 'all-the-icons-orange :v-adjust 0.01))
                             :keys "s"
                             :extra "\nSCHEDULED: %^{Start time:}t"
                             )
                            ))
                (,(format "%s\tProject" (all-the-icons-octicon "repo" :face 'all-the-icons-silver :v-adjust 0.01))
                 :keys "p"
                 :prepend t
                 :type entry
                 :headline "Inbox"
                 :template ("* %{time-or-todo} %?"
                            "%i"
                            "%a")
                 :file ""
                 :custom (:time-or-todo "")
                 :children ((,(format "%s\tProject todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
                             :keys "t"
                             :time-or-todo "TODO"
                             :file org-projects-file)
                            (,(format "%s\tProject note" (all-the-icons-faicon "sticky-note" :face 'all-the-icons-yellow :v-adjust 0.01))
                             :keys "n"
                             :time-or-todo "%U"
                             :file org-projects-file)
                            (,(format "%s\tProject changelog" (all-the-icons-faicon "list" :face 'all-the-icons-blue :v-adjust 0.01))
                             :keys "c"
                             :time-or-todo "%U"
                             :heading "Unreleased"
                             :file org-projects-file))
                 )
                )))
;;;;; :config
  :config
  (defun org-capture-select-template-prettier (&optional keys)
    "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
    (let ((org-capture-templates
           (or (org-contextualize-keys
                (org-capture-upgrade-templates org-capture-templates)
                org-capture-templates-contexts)
               '(("t" "Task" entry (file+headline "" "Tasks")
                  "* TODO %?\n  %u\n  %a")))))
      (if keys
          (or (assoc keys org-capture-templates)
              (error "No capture template referred to by \"%s\" keys" keys))
        (org-mks org-capture-templates
                 "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
                 "Template key: "
                 `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
  (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

  (defun org-mks-pretty (table title &optional prompt specials)
    "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
    (save-window-excursion
      (let ((inhibit-quit t)
            (buffer (org-switch-to-buffer-other-window "*Org Select*"))
            (prompt (or prompt "Select: "))
            case-fold-search
            current)
        (unwind-protect
            (catch 'exit
              (while t
                (setq-local evil-normal-state-cursor (list nil))
                (erase-buffer)
                (insert title "\n\n")
                (let ((des-keys nil)
                      (allowed-keys '("\C-g"))
                      (tab-alternatives '("\s" "\t" "\r"))
                      (cursor-type nil))
                  ;; Populate allowed keys and descriptions keys
                  ;; available with CURRENT selector.
                  (let ((re (format "\\`%s\\(.\\)\\'"
                                    (if current (regexp-quote current) "")))
                        (prefix (if current (concat current " ") "")))
                    (dolist (entry table)
                      (pcase entry
                        ;; Description.
                        (`(,(and key (pred (string-match re))) ,desc)
                         (let ((k (match-string 1 key)))
                           (push k des-keys)
                           ;; Keys ending in tab, space or RET are equivalent.
                           (if (member k tab-alternatives)
                               (push "\t" allowed-keys)
                             (push k allowed-keys))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                        ;; Usable entry.
                        (`(,(and key (pred (string-match re))) ,desc . ,_)
                         (let ((k (match-string 1 key)))
                           (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                           (push k allowed-keys)))
                        (_ nil))))
                  ;; Insert special entries, if any.
                  (when specials
                    (insert "─────────────────────────\n")
                    (pcase-dolist (`(,key ,description) specials)
                      (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                      (push key allowed-keys)))
                  ;; Display UI and let user select an entry or
                  ;; a sub-level prefix.
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer))
                  (let ((pressed (org--mks-read-key allowed-keys prompt)))
                    (setq current (concat current pressed))
                    (cond
                     ((equal pressed "\C-g") (user-error "Abort"))
                     ;; Selection is a prefix: open a new menu.
                     ((member pressed des-keys))
                     ;; Selection matches an association: return it.
                     ((let ((entry (assoc current table)))
                        (and entry (throw 'exit entry))))
                     ;; Selection matches a special entry: return the
                     ;; selection prefix.
                     ((assoc current specials) (throw 'exit current))
                     (t (error "No entry available")))))))
          (when buffer (kill-buffer buffer))))))
  (advice-add 'org-mks :override #'org-mks-pretty)

  ;; The org-capture bin is rather nice, but I’d be nicer with a smaller frame, and no modeline.
  ;; (setf (alist-get 'height +org-capture-frame-parameters) 15)
  ;; (alist-get 'name +org-capture-frame-parameters) "❖ Capture") ;; ATM hardcoded in other places, so changing breaks stuff
  (setq +org-capture-fn
        (lambda ()
          (interactive)
          (set-window-parameter nil 'mode-line-format 'none)
          (org-capture))))




;; ;;;; Org-fancy-priorities

;; (use-package org-fancy-priorities
;;   :ensure t
;;   :diminish
;;   :defines org-fancy-priorities-list
;;   :hook (org-mode . org-fancy-priorities-mode)
;;   :config
;;   (setq org-fancy-priorities-list '((?A . "⚑")  ;; ASAP
;;                                     (?B . "⬆")  ;; High
;;                                     (?C . "■")  ;; Medium
;;                                     (?D . "⬇")  ;; Low
;;                                     (?E . "❓")) ;; Optional
;;         org-priority-faces '((?A . all-the-icons-red)
;;                              (?B . all-the-icons-orange)
;;                              (?C . all-the-icons-yellow)
;;                              (?D . all-the-icons-green)
;;                              (?E . all-the-icons-blue))
;;         org-priority-highest ?A
;;         org-priority-lowest ?E)
;;   (unless (char-displayable-p ?❗)
;;     (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))))

;; ;;;; Org-pretty-tags

;; (use-package org-pretty-tags
;;   :ensure t
;;   :config
;;   (setq org-pretty-tags-surrogate-strings
;;         '(("uni" . "🎓")
;;           ("assignment" . "📓")
;;           ("email" . "🖂")
;;           ("read" . "🕮")
;;           ("article" . "🖹")
;;           ("web" . "🌐")
;;           ("info" . "🛈")
;;           ("issue" . "🐛")
;;           ("emacs" . "ɛ")))
;;   (org-pretty-tags-global-mode))

;; ;; ;;;; Org-super-agenda

;; ;; (use-package org-super-agenda
;; ;;   :ensure t
;; ;;   :commands (org-super-agenda-mode)
;; ;;   :hook (org-agenda-mode . org-super-agenda-mode)
;; ;;   :config
;; ;;   (setq org-agenda-skip-scheduled-if-done t
;; ;;         org-agenda-skip-deadline-if-done t
;; ;;         org-agenda-include-deadlines t
;; ;;         org-agenda-block-separator nil
;; ;;         org-agenda-compact-blocks t)

;; ;;   (setq org-agenda-custom-commands
;; ;;         '(("o" "Overview"
;; ;;            ((agenda "" ((org-agenda-span 'day)
;; ;;                         (org-super-agenda-groups
;; ;;                          '((:name "Today"
;; ;;                                   :time-grid t
;; ;;                                   :date today
;; ;;                                   :todo "TODAY"
;; ;;                                   :scheduled today
;; ;;                                   :order 1)))))
;; ;;             (alltodo "" ((org-agenda-overriding-header "")
;; ;;                          (org-super-agenda-groups
;; ;;                           '((:name "Next to do"
;; ;;                                    :todo "NEXT"
;; ;;                                    :order 1)
;; ;;                             (:name "Important"
;; ;;                                    :tag "Important"
;; ;;                                    :priority "A"
;; ;;                                    :order 6)
;; ;;                             (:name "Due Today"
;; ;;                                    :deadline today
;; ;;                                    :order 2)
;; ;;                             (:name "Due Soon"
;; ;;                                    :deadline future
;; ;;                                    :order 8)
;; ;;                             (:name "Overdue"
;; ;;                                    :deadline past
;; ;;                                    :face error
;; ;;                                    :order 7)
;; ;;                             (:name "Assignments"
;; ;;                                    :tag "Assignment"
;; ;;                                    :order 10)
;; ;;                             (:name "Issues"
;; ;;                                    :tag "Issue"
;; ;;                                    :order 12)
;; ;;                             (:name "Projects"
;; ;;                                    :tag "Project"
;; ;;                                    :order 14)
;; ;;                             (:name "Emacs"
;; ;;                                    :tag "Emacs"
;; ;;                                    :order 13)
;; ;;                             (:name "Research"
;; ;;                                    :tag "Research"
;; ;;                                    :order 15)
;; ;;                             (:name "To read"
;; ;;                                    :tag "Read"
;; ;;                                    :order 30)
;; ;;                             (:name "Waiting"
;; ;;                                    :todo "WAITING"
;; ;;                                    :order 20)
;; ;;                             (:name "Trivial"
;; ;;                                    :priority<= "E"
;; ;;                                    :tag ("Trivial" "Unimportant")
;; ;;                                    :todo ("SOMEDAY" )
;; ;;                                    :order 90)
;; ;;                             (:discard (:tag ("Chore" "Routine" "Daily"))))))))))))

;; ;;;; Org-superstar

;; (use-package org-superstar
;;   :ensure t
;;   :hook (org-mode . org-superstar-mode)
;;   :config
;;   (setq org-superstar-prettify-item-bullets t
;;         org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")))

;;; Provides:

(provide 'pretty-org)

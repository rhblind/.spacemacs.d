;;; Config Layer -*- lexical-binding: t; -*-
(setq config-packages
      '(;; Unowned Packages
        aggressive-indent
        avy
        company
        dap
        drag-stuff
        dtrt-indent
        elixir-mode
        eshell
        evil
        evil-mc
        evil-string-inflection
        flycheck
        flyspell
        gdscript-mode
        html
        ivy
        magit
        ob org org-gcal org-projectile
        python
        ranger
        smartparens

        ;; Owned Packages
        auto-dim-other-buffers
        dash-functional
        faceup
        outshine  ; also configures `outline-mode'
        s

        ;; Local Packages
        ;; (redo-spacemacs :location local)
        ))


;;;; Aggressive indent
(defun config/pre-init-aggressive-indent ()
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'python-mode-hook     #'aggressive-indent-mode))

;;;; Avy
(defun config/pre-init-avy ()
  (setq avy-timeout-seconds 0.35)

  (evil-global-set-key 'normal "s" 'avy-goto-char-timer)
  (bind-keys ("C-l" . evil-avy-goto-line)
             ("C-h" . avy-pop-mark)))

;;;; Company
(defun config/post-init-company ()
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-d") #'company-next-page)
    (define-key company-active-map (kbd "C-u") #'company-previous-page))


  (when (configuration-layer/package-used-p 'lsp)
    (setq company-lsp-cache-candidates nil
          company-lsp-async t)
    (spacemacs|add-company-backends
      :backends company-lsp
      :modes elixir-mode python-mode))

  ;; Sweet icons for company-mode
  (use-package company-box
    :diminish company-box-mode
    :hook (company-mode . company-box-mode)
    :init
    (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    (require 'all-the-icons)
    (setf (alist-get 'min-height company-box-frame-parameters) 6)
    (setq company-box-icons-alist 'company-box-icons-all-the-icons
          company-box-backends-colors nil

          ;; These are the Doom Emacs defaults
          company-box-icons-all-the-icons
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))))

    ;; Add a space after the icon
    (dolist (elt company-box-icons-all-the-icons)
      (setcdr elt (concat (cdr elt) " "))))
  )

;;;; Dap
(defun config/post-init-dap ()
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra))))  ;; Open a Hydra debug menu whenever hitting a breakpoint

;;;; Drag-stuff
(defun config/init-drag-stuff ()
  (drag-stuff-global-mode t)
  (global-set-key (kbd "<C-up>") 'drag-stuff-up)
  (global-set-key (kbd "<C-down>") 'drag-stuff-down))

;;;; Dtrt-indent
(defun config/init-dtrt-indent ()
  (use-package dtrt-indent))

;;;; Elixir
(defun config/post-init-elixir-mode ()
  (with-eval-after-load 'elixir-mode
    (spacemacs/declare-prefix-for-mode 'elixir-mode
      "mt" "test" "testing related functionality")
    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
      "tb" 'exunit-verify-all
      "ta" 'exunit-verify
      "tk" 'exunit-rerun
      "tt" 'exunit-verify-single
      "tu" 'exunit-verify-all-in-umbrella
      ))

  ;; Register your debug run configurations here
  ;; (see dap-elixir for defaults injected into the "Elixir" type)
  (require 'dap-elixir)
  (with-eval-after-load 'dap-mode
    (dap-register-debug-template "Phoenix Run Configuration"
                                 (list :type "Elixir"
                                       :cwd (lsp-workspace-root)
                                       :request "launch"
                                       :program nil
                                       :projectDir (lsp-workspace-root)
                                       :name "Elixir::Phoenix Server"
                                       :task "phx.server")))

  (add-hook 'elixir-mode-hook (lambda () (setq-local counsel-dash-docsets '("Elixir"))))
  (add-hook 'lsp-mode-hook (lambda ()
                             (dolist (ignore-pattern '("[/\\\\]\\.elixir_ls$" "[/\\\\]\\.log$" "[/\\\\]_build$" "[/\\\\]deps$"))
                               (add-to-list 'lsp-file-watch-ignored ignore-pattern)))))

;;;; Eshell
(defun config/pre-init-eshell ()
  (spacemacs|use-package-add-hook eshell
    :post-init
    (evil-define-key '(normal insert) 'global (kbd "C-e") 'eshell-pop-eshell)))

;;;; Evil
(defun config/post-init-evil ()
  (setq evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence nil)

  (evil-global-set-key 'normal "Q" 'evil-execute-q-macro)
  (define-key evil-normal-state-map (kbd "C-S-u") 'evil-scroll-other-window-interactive)
  (define-key evil-normal-state-map (kbd "C-S-d") 'evil-scroll-other-window-down-interactive)
  (evil-define-key '(normal visual motion) 'global
    "H"  'evil-first-non-blank
    "L"  'evil-end-of-line-interactive
    "0"  'evil-jump-item)

  (advice-add 'evil-ex-search-next     :after 'evil-scroll-to-center-advice)
  (advice-add 'evil-ex-search-previous :after 'evil-scroll-to-center-advice))

;;;; Evil-MC
(defun config/post-init-evil-mc ()
  (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
  (add-hook 'text-mode-hook 'turn-on-evil-mc-mode))

;;;; Evil-String-Inflection
(defun config/init-evil-string-inflection ()
  (use-package evil-string-inflection :ensure t)
  (define-key evil-normal-state-map "gC" 'evil-operator-string-inflection))

;;;; Flycheck
(defun config/post-init-flycheck ()
  (use-package flycheck-credo
    :config
    (flycheck-credo-setup)
    (with-eval-after-load 'lsp-ui
      (lambda () (flycheck-add-next-checker 'lsp-ui 'elixir-credo)))))

;;;; Flyspell
(defun config/post-init-flyspell ()
  (let ((langs '("english" "norsk")))
    (setq lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert lang-ring elem)))

  (global-set-key (kbd "<f8>") 'cycle-ispell-languages))

;;;; Gdscript-mode (Godot)
(defun config/init-gdscript-mode ()
  (use-package gdscript-mode
    :ensure t
    :hook (gdscript-mode . lsp)
    :config
    (when (spacemacs/system-is-mac)
      (setq gdscript-godot-executable "/Applications/Godot.app/Contents/MacOS/Godot"))))

;;;; Ivy
(defun config/pre-init-ivy ()
  (setq ivy-format-function 'ivy-format-function-arrow)
  (setq completion-in-region-function 'ivy-completion-in-region))

(defun config/post-init-ivy ()
  (setq ivy-height 20)

  (spacemacs/set-leader-keys "ai" 'ivy-resume)

  (bind-keys :map ivy-minibuffer-map
             ("C-l"        . ivy-avy)
             ("C-u"        . ivy-scroll-down-command)
             ("C-d"        . ivy-scroll-up-command)
             ("C-n"        . ivy-restrict-to-matches)
             ("C-y"        . ivy-yank-word)
             ("C-<return>" . ivy-call)
             ("C-SPC"      . ivy-dispatching-done)
             ("C-S-SPC"    . ivy-dispatching-call)))

;;;; Magit
(defun config/post-init-magit ()
  (use-package forge
    :after magit
    :defer t
    :config
    (add-to-list 'forge-alist '("gitlab.intility.no" "gitlab.intility.no/api/v4" "gitlab.intility.no" forge-gitlab-repository))
    (setq gitlab.user "user")
    (when (string= system-type "darwin")
      (setq ghub-use-workaround-for-emacs-bug 'force))

    (define-key magit-mode-map (kbd "C-c C-c") 'forge-copy-url-at-point-as-kill))

  (bind-keys :map magit-mode-map
             ("M-1" . winum-select-window-1)
             ("M-2" . winum-select-window-2)
             ("M-3" . winum-select-window-3)
             ("M-4" . winum-select-window-4)))

;;;; Org
(defun config/pre-init-ob ()
  (setq org-confirm-babel-evaluate   nil)
  (setq org-src-fontify-natively     t)
  (setq org-src-tab-acts-natively    t)
  (setq org-src-preserve-indentation t)
  (setq org-src-window-setup         'current-window)

  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(dot . t))))

(defun config/pre-init-org ()
  (setq-default org-display-custom-times t)
  (setq org-agenda-skip-unavailable-files t
        org-catch-invisible-edits t
        org-ellipsis ""
        org-export-in-background t
        org-fontify-whole-heading-line t
        org-fontify-done-headline nil
        org-fontify-quote-and-verse-blocks t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-indent-indentation-per-level 1
        org-log-state-notes-into-drawer t
        org-log-done-with-time t
        ;; org-refile-targets '((org-agenda-files . (:maxlevel . 6)))  ;; TODO https://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
        org-re-reveal-root "https://revealjs.com/"
        org-startup-indented t
        org-pretty-entities t
        org-priority-faces '((65 :inherit org-priority :foreground "red")
                             (66 :inherit org-priority :foreground "brown")
                             (67 :inherit org-priority :foreground "blue"))
        ;; org-structure-template-alist '(("n"  "#+NAME: ?")
        ;;                                ("L"  "#+LaTeX: ")
        ;;                                ("h"  "#+HTML: ")
        ;;                                ("q"  "#+BEGIN_QUOTE\n\n#+END_QUOTE")
        ;;                                ("s"  "#+BEGIN_SRC ?\n\n#+END_SRC")
        ;;                                ("se" "#+BEGIN_SRC elixir\n\n#+END_SRC")
        ;;                                ("sj" "#+BEGIN_SRC javascript\n\n#+END_SRC")
        ;;                                ("sp" "#+BEGIN_SRC python\n\n#+END_SRC"))
        org-time-stamp-custom-formats '("<%a %d.%m.%Y>" . "<%a %d.%m.%Y %H:%M>"))

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook (lambda () (progn
                                        (require 'org-tempo)  ;; Required for new org templating system to work
                                        (setq line-spacing 0.2
                                              header-line-format " "
                                              left-margin-width 2
                                              right-margin-width 2))))

  ;; Org LaTeX, templates (also for PDF exports)
  (with-eval-after-load 'ox-latex
    ;; (add-to-list 'org-latex-classes
    ;;'("org-article"
                                        ;                    "\\documentclass[11pt,a4paper]{article}
    ;; \\usepackage[utf8]{inputenc}
    ;; \\usepackage[T1]{fontenc}
    ;; \\usepackage{fixltx2e}
    ;; \\usepackage{graphicx}
    ;; \\usepackage{longtable}
    ;; \\usepackage{float}
    ;; \\usepackage{wrapfig}
    ;; \\usepackage{rotating}
    ;; \\usepackage[normalem]{ulem}
    ;; \\usepackage{amsmath}
    ;; \\usepackage{textcomp}
    ;; \\usepackage{marvosym}
    ;; \\usepackage{wasysym}
    ;; \\usepackage{amssymb}
    ;; \\usepackage{hyperref}
    ;; \\usepackage{mathpazo}
    ;; \\usepackage{color}
    ;; \\usepackage{enumerate}
    ;; \\definecolor{bg}{rgb}{0.95,0.95,0.95}
    ;; \\tolerance=1000
    ;;       [NO-DEFAULT-PACKAGES]
    ;;       [PACKAGES]
    ;;       [EXTRA]
    ;; \\linespread{1.1}
    ;; \\hypersetup{pdfborder=0 0 0}"
    ;;                    ("\\section{%s}" . "\\section*{%s}")
    ;;                    ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;                    ("\\paragraph{%s}" . "\\paragraph*{%s}"))


    ;; )

    ;; (add-to-list 'org-latex-classes
    ;;              '("org-article"
    ;;                "\\documentclass[10pt,article,oneside]{memoir}"
    ;;                ("\\section{%s}" . "\\section*{%s}")
    ;;                ("\\subsection{%s}" . "\\subsection*{%s}")
    ;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
    ;;              )

    ;; ;; Settings to export code with `minted' instead of `verbatim'.
    ;; (setq org-export-latex-listings t)
    ;; (setq org-latex-listings 'minted
    ;;       org-latex-packages-alist '(("" "minted")
    ;;                                  ("" "memoir"))
    ;;       ;; org-latex-pdf-process '("pdflatex -shell-escape -intera")
    ;;       )

    ))

(defun config/post-init-org ()
  (evil-define-key '(normal visual motion) org-mode-map
    "gh" 'outline-up-heading
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gl" 'outline-next-visible-heading
    "gu" 'outline-previous-visible-heading)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "o"   'counsel-outline
    "r"   'org-refile
    "h"   'org-metaleft                    ;; Because of MacOS's damned, indestructable M-h binding...
    "d o" 'org-toggle-time-stamp-overlays  ;; Required to toggle off before changing time when using custom formats
    "s p" 'org-sort-entries-priorities))

(defun config/init-org-gcal ()
  ;; https://github.com/kidd/org-gcal.el/
  (use-package org-gcal
    :after org
    :config (setq
             ;; org-gcal-client-id variable set in `secrets.el.gpg'
             ;; org-gcal-client-secret  variable set in `secrets.el.gpg'
             ;; TODO: Read org-gcal-file-alist from `personal.el' file
             org-gcal-file-alist '(("rhblind@gmail.com" . "~/Dropbox/org/gcal.org")
                                   ("family07115093619329273023@group.calendar.google.com" . "~/Dropbox/org/family.org"))
             org-gcal-auto-archive nil
             org-gcal-notify-p nil)
    (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
    (add-hook 'org-capture-after-finalize-hook 'org-gcal-fetch))
  )

(defun config/pre-init-org-projectile ()
  (use-package org-projectile
    :after org
    :config
    (progn
      (setq org-projectile-per-project-filepath "TODO.org"
            org-projectile-capture-template "* TODO %? %^G\n%i\n%a"
            org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
      (org-projectile-per-project))
    ))

;;;; Python
(defun config/pre-init-python ()
  (require 'dap-python)
  (require 'lsp-python-ms)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python"))))

  (with-eval-after-load 'python
    (setq python-shell-interpreter "python3")
    (custom-set-variables
     '(flycheck-python-flake8-executable "python3")
     '(flycheck-python-pycompile-executable "python3")
     '(flycheck-python-pylint-executable "python3"))))

;;;; Ranger
(defun config/pre-init-ranger ()
  (setq ranger-deer-show-details nil)

  (evil-global-set-key 'normal "_" 'ranger)

  ;; To get around `ranger/post-init-dired' overwriting keybindings
  (spacemacs|use-package-add-hook ranger
    :post-config
    (bind-keys :map ranger-mode-map
               ("n"   . dired-create-directory)
               ("E"   . wdired-change-to-wdired-mode)
               ("C-j" . ranger-travel)
               ("C-e" . ranger-pop-eshell)
               ("M-1" . winum-select-window-1)
               ("M-2" . winum-select-window-2)
               ("M-3" . winum-select-window-3)
               ("M-4" . winum-select-window-4)
               ("M-5" . winum-select-window-5))))

;;;; Smartparens
(defun config/post-init-smartparens ()
  (add-hook 'typescript-mode-hook 'turn-off-smartparens-strict-mode)
  (add-hook 'javascript-mode-hook 'turn-off-smartparens-strict-mode)
  (add-hook 'js2-mode-hook        'turn-off-smartparens-strict-mode)
  (add-hook 'rjsx-mode-hook       'turn-off-smartparens-strict-mode))

;;;; Web
(defun config/post-init-html ()
  ;; 2 space indent also for element's attributes, concatenations and contiguous function calls
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))))

;;; Owned Packages
;;;; Auto Dim Other Buffers
(defun config/init-auto-dim-other-buffers ()
  (use-package auto-dim-other-buffers
    :config
    (auto-dim-other-buffers-mode)))

;;;; Dash functional
(defun config/init-dash-functional ()
  ;; The spacemacs core file `core-documentation' requires dash.
  ;; So we only have to use-pkg dash-functional to have all of dash around.
  (use-package dash-functional))

;;;; Faceup
(defun config/init-faceup ()
  (use-package faceup
    :defer t))

;;;; Outshine
(defun config/init-outshine ()
  (use-package outshine
    :hook ((prog-mode          . outline-minor-mode)
           (outline-minor-mode . outshine-mode))

    :bind (("<backtab>"           . outshine-cycle-buffer)
           ([(meta return)]       . outshine-insert-heading)
           ([(meta shift return)] . outshine-insert-subheading)
           :map outline-minor-mode-map)

    :init
    (progn
      (evil-define-key '(normal visual motion) outline-minor-mode-map
        "gh" 'outline-up-heading
        "gj" 'outline-forward-same-level
        "gk" 'outline-backward-same-level
        "gl" 'outline-next-visible-heading
        "gu" 'outline-previous-visible-heading)

      (spacemacs/set-leader-keys
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen
        "nj" 'outline-move-subtree-down
        "nk" 'outline-move-subtree-up
        "nh" 'outline-promote
        "nl" 'outline-demote)

      (advice-add 'outshine-narrow-to-subtree :before 'outshine-fix-narrow-pos)

      (advice-add 'outshine-insert-heading    :before 'outshine-fix-insert-pos)
      (advice-add 'outshine-insert-heading    :after 'evil-insert-advice)
      (advice-add 'outshine-insert-subheading :after 'evil-insert-advice)

      ;; Fix the new bindings in outline-minor-mode overwriting org-mode-map
      ;; I also add advice here because it mirrors outshine modifications
      (spacemacs|use-package-add-hook org
        :post-config
        (progn
          (bind-keys :map org-mode-map
                     ("C-j"                 . counsel-outline)
                     ([(meta return)]       . org-meta-return)
                     ([(meta shift return)] . org-insert-subheading))
          (advice-add 'org-insert-heading    :before 'org-fix-heading-pos)
          (advice-add 'org-insert-heading    :after 'evil-insert-advice)
          (advice-add 'org-insert-subheading :after 'evil-insert-advice))))))

;;;; Strings
(defun config/init-s ()
  (use-package s))

;;; Local Packages
;;;; Redo-spacemacs
;; `redo-spacemacs-bindings' is executed in user-config in `init.el'
;; with the `dotspacemacs/user-config/post-layer-load-config' function

;; If any removed bindings make you scratch your head, check out
;; the ending `redo-spacemacs-new-bindings-alist' to see what I rebound it
;; to (for example, `spacemacs/delete-window' from 'SPC w d' to 'M-d')
;; They are unbound to force muscle-memory development.

;; (defun config/init-redo-spacemacs ()
;;   (use-package redo-spacemacs
;;     :if (and (boundp 'redo-bindings?) redo-bindings?)
;;     :init
;;     (progn
;;       (setq redo-spacemacs-prefixes-list
;;             '(;; Primary prefixes
;;               "C"    ; capture/colors
;;               "i"    ; insertion
;;               "j"    ; jump/join/split
;;               "N"    ; navigation
;;               "r"    ; registers/rings/resume
;;               "t"    ; toggles
;;               "z"    ; zoom

;;               ;; Sub prefixes
;;               "a s"  ; shells
;;               "b N"  ; new buffers
;;               "f v"  ; file/dir-local-variables
;;               "f C"  ; files/convert
;;               "p $"  ; projects/shell
;;               "s k"  ; search/ack
;;               "s r"  ; search/ripgrep
;;               "s t"  ; search/pt
;;               "w p"  ; windows/popup
;;               "x d"  ; text/delete
;;               "x g"  ; text/google-translate
;;               "x j"  ; text/justification
;;               "x t"  ; text/transposition
;;               "x w"  ; text/words
;;               ))

;;       (setq redo-spacemacs-undo-bindings-alist
;;             '(;; Top-level
;;               ("!" shell-command)
;;               ("'" spacemacs/default-pop-shell)
;;               ("0" neotree-show)
;;               ("?" counsel-descbinds)
;;               ("`" winum-select-window-by-number)
;;               ("1" winum-select-window-1)
;;               ("2" winum-select-window-2)
;;               ("3" winum-select-window-3)
;;               ("4" winum-select-window-4)
;;               ("5" winum-select-window-5)
;;               ("6" winum-select-window-6)
;;               ("7" winum-select-window-7)
;;               ("8" winum-select-window-8)
;;               ("9" winum-select-window-9)

;;               ;; A - applications
;;               ("ad" deer)
;;               ("ar" ranger)

;;               ;; B - buffers
;;               ("b." spacemacs/buffer-transient-state/body)
;;               ("bB" spacemacs-layouts/non-restricted-buffer-list-ivy)
;;               ("bD" spacemacs/ace-kill-this-buffer)
;;               ("bh" spacemacs/home
;;                spacemacs/switch-to-help-buffer)
;;               ("bH" spacemacs/switch-to-help-buffer)
;;               ("be" spacemacs/safe-erase-buffer)
;;               ("bb" ivy-switch-buffer
;;                ibuffer)
;;               ("bI" ibuffer)
;;               ("bn" next-buffer)
;;               ("bp" previous-buffer)
;;               ("bP" spacemacs/copy-clipboard-to-whole-buffer)
;;               ("bR" spacemacs/safe-revert-buffer)
;;               ("bw" read-only-mode)
;;               ("bW" spacemacs/goto-buffer-workspace)
;;               ("bY" spacemacs/copy-whole-buffer-to-clipboard)
;;               ("b C-d"   spacemacs/kill-other-buffers)
;;               ("b C-S-d" spacemacs/kill-matching-buffers-rudely)

;;               ;; c - compile/comments
;;               ("cl" spacemacs/comment-or-uncomment-lines)
;;               ("cL" spacemacs/comment-or-uncomment-lines-inverse)
;;               ("cP" spacemacs/comment-or-uncomment-paragraphs-inverse)
;;               ("cT" spacemacs/quick-comment-or-uncomment-to-the-line-inverse)
;;               ("cY" spacemacs/copy-and-comment-lines-inverse)

;;               ;; e - errors
;;               ;; ... Haven't went through yet ...

;;               ;; F - frames
;;               ("Fb" spacemacs/switch-to-buffer-other-frame)
;;               ("FB" spacemacs/display-buffer-other-frame)
;;               ("FD" delete-other-frames)
;;               ("Ff" spacemacs/find-file-other-frame)
;;               ("Fn" make-frame)
;;               ("FO" spacemacs/dired-other-frame)

;;               ;; f - files
;;               ("fA" spacemacs/find-file-and-replace-buffer)
;;               ("fb" counsel-bookmark)
;;               ("fE" spacemacs/sudo-edit)
;;               ("fg" rgrep)
;;               ("fh" hexl-find-file)
;;               ("fi" spacemacs/insert-file)
;;               ("fJ" spacemacs/open-junk-file)
;;               ("fj" dired-jump)
;;               ("fl" find-file-literally)
;;               ("fL" counsel-locate)

;;               ;; g - git/version-control
;;               ;; ... Haven't went through yet ...

;;               ;; h - help
;;               ;; ... Haven't went through yet ...

;;               ;; i - insertion
;;               ;; Removed entire leader

;;               ;; j - jump/join/split
;;               ;; Removed entire leader

;;               ;; k - lisp
;;               ;; Removed entire leader (I always use transient state for these)

;;               ;; N - navigation
;;               ;; Removed entire leader

;;               ;; n - narrow/numbers
;;               ("n+" spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt)
;;               ("np" narrow-to-page)
;;               ("nr" narrow-to-region)

;;               ;; p - projects
;;               ("p%" projectile-replace-regexp)
;;               ("pe" projectile-edit-dir-locals)
;;               ("pF" projectile-find-file-dwim)
;;               ("pR" projectile-replace)
;;               ("pT" projectile-test-project)
;;               ("pv" projectile-vc)

;;               ;; q - quit
;;               ("qs" spacemacs/save-buffers-kill-emacs)
;;               ("qt" spacemacs/restart-emacs-adv-timers)

;;               ;; r - registers/rings/resume
;;               ;; Removed entire leader

;;               ;; s - search/symbol
;;               ("sf" spacemacs/search-auto)
;;               ("sF" spacemacs/search-auto-region-or-symbol)
;;               ("sj" spacemacs/counsel-jump-in-buffer)
;;               ("sp" spacemacs/search-project-auto)
;;               ("sP" spacemacs/search-project-auto-region-or-symbol)
;;               ("ss" swiper)
;;               ("sS" spacemacs/swiper-region-or-symbol)

;;               ;; T - UI toggles/themes
;;               ;; Leaving unchanged

;;               ;; t - toggles
;;               ;; Removed entire leader

;;               ;; w - windows
;;               ("w+" spacemacs/window-layout-toggle)
;;               ("w1" spacemacs/window-split-single-column)
;;               ("w2" spacemacs/window-split-double-columns)
;;               ("w3" spacemacs/window-split-triple-columns)
;;               ("w_" spacemacs/maximize-horizontally)
;;               ("wC" spacemacs/toggle-distraction-free)
;;               ("wc" spacemacs/toggle-centered-buffer)
;;               ("wF" make-frame)
;;               ("wh" evil-window-left)
;;               ("wj" evil-window-down)
;;               ("wk" evil-window-up)
;;               ("wl" evil-window-right)
;;               ("ws" split-window-below)
;;               ("wS" split-window-below-and-focus)
;;               ("wv" split-window-right)
;;               ("wV" split-window-right-and-focus)
;;               ("ww" other-window
;;                ace-window)
;;               ("wx" kill-buffer-and-window)
;;               ("wW" ace-window)
;;               ("w|" spacemacs/maximize-vertically)
;;               ("w <down>"    evil-window-down)
;;               ("w <up>"      evil-window-up)
;;               ("w <left>"    evil-window-left)
;;               ("w <right>"   evil-window-right)
;;               ("w <S-down>"  evil-window-move-very-bottom)
;;               ("w <S-up>"    evil-window-move-very-top)
;;               ("w <S-left>"  evil-window-move-far-left)
;;               ("w <S-right>" evil-window-move-far-right)

;;               ;; x - text
;;               ("x TAB" indent-rigidly)
;;               ("xJ" spacemacs/move-text-transient-state/move-text-down)
;;               ("xK" spacemacs/move-text-transient-state/move-text-up)
;;               ("xo" link-hint-open-link)
;;               ("xO" link-hint-open-multiple-links)

;;               ;; z - zoom
;;               ;; Removed entire leader

;;               ;; Important bindings that I use chords for now.
;;               ;; They are removed to force muscle-memory.
;;               ("v" er/expand-region)
;;               ("wm" spacemacs/toggle-maximize-buffer)
;;               ("wd" spacemacs/delete-window)
;;               ("w/" split-window-right)
;;               ("w-" split-window-below)
;;               ("ff" counsel-find-file)
;;               ("fr" counsel-recentf)
;;               ))

;;       (setq redo-spacemacs-new-bindings-alist
;;             '(;; Windows, Layouts Management
;;               ("M-w"   spacemacs/toggle-maximize-buffer)
;;               ("M-d"   spacemacs/delete-window)
;;               ("M-c"   spacemacs/toggle-centered-buffer-mode)
;;               ("M-/"   split-window-right)
;;               ("C-M-/" split-window-right-and-focus)
;;               ("M--"   split-window-below)
;;               ("C-M--" split-window-below-and-focus)
;;               ("M-1" winum-select-window-1)
;;               ("M-2" winum-select-window-2)
;;               ("M-3" winum-select-window-3)
;;               ("M-4" winum-select-window-4)
;;               ("M-5" winum-select-window-5)

;;               ;; Editing, Searching, Movement
;;               ("C-,"   lisp-state-toggle-lisp-state)
;;               ("C-SPC" er/expand-region)
;;               ("C-S-s" spacemacs/swiper-region-or-symbol)

;;               ;; Files, Buffers
;;               ("M-f" counsel-find-file)
;;               ("M-r" counsel-recentf)

;;               ;; Rebindings to look at
;;               ;; spacemacs/kill-this-buffer
;;               ;; M-u, M-i
;;               )))))

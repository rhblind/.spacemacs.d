;;; Config Layer -*- lexical-binding: t; -*-
(setq config-packages
      '(;; Unowned Packages
        aggressive-indent
        auto-highlight-symbol
        avy
        csharp-mode
        company
        dap
        drag-stuff
        dtrt-indent
        elixir-mode
        eshell
        evil
        evil-mc
        evil-string-inflection
        flyspell
        lsp-mode
        ivy
        magit
        ob
        org
        org-roam
        org-projectile
        python
        ranger
        writeroom-mode
        web-mode
        yasnippet-snippets

        ;; Owned Packages
        auto-dim-other-buffers
        faceup
        outshine  ; also configures `outline-mode'
        s

        ;; Local Packages
        (dap-shell :location local) ; provides shell tasks for dap mode
        ;; (redo-spacemacs :location local)
        (ivy-todo :location local)
        ))


;;;; Aggressive indent
(defun config/pre-init-aggressive-indent ()
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;;;; Auto-highlight-symbol
(defun config/pre-init-auto-highlight-symbol ()
  (add-hook 'prog-mode 'auto-highlight-symbol-mode)
  (add-hook 'text-mode 'auto-highlight-symbol-mode))

;;;; Avy
(defun config/pre-init-avy ()
  (setq avy-timeout-seconds 0.35)

  ;; Trying out evil-snipe, which conflicts with this shortcut. Use C-j-j to trigger avy-goto-char-timer
  ;; (evil-global-set-key 'normal "s" 'avy-goto-char-timer)
  (bind-keys ("C-l" . evil-avy-goto-line)
             ("C-h" . avy-pop-mark)))


;;;; Csharp
(defun config/post-init-csharp-mode ()
  (setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")

  (add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))

;;;;; Mode hooks

  (add-hook 'csharp-mode-hook (lambda () (setq-local counsel-dash-docsets '("NET_Framework")
                                                     dash-at-point-docset "NET_Framework")))
  (add-hook 'csharp-mode-hook (lambda ()
                                (add-hook 'before-save-hook #'lsp-format-buffer nil t)
                                (add-hook 'before-save-hook #'delete-trailing-crlf))))

;;;; Company
(defun config/post-init-company ()
  (add-hook 'after-init-hook 'global-company-mode)

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-d") #'company-next-page)
    (define-key company-active-map (kbd "C-u") #'company-previous-page)

    (setq company-dabbrev-other-buffers nil
          company-dabbrev-ignore-case   nil
          company-dabbrev-downcase      nil))


;;;;; Company-box
  (use-package company-box
    :diminish
    :if (display-graphic-p)
    :defines company-box-icons-all-the-icons
    :hook (company-mode . company-box-mode)
    :custom
    (company-box-backends-colors nil)
    :config
    (with-no-warnings
      ;; Prettify icons
      (defun +company-box-icons--elisp-fn (candidate)
        (when (derived-mode-p 'emacs-lisp-mode)
          (let ((sym (intern candidate)))
            (cond ((fboundp  sym) 'ElispFunction)
                  ((featurep sym) 'ElispFeature)
                  ((facep    sym) 'ElispFace)
                  ((boundp   sym) 'ElispVariable)
                  ((symbolp  sym) 'Text)
                  (t .       nil)))))
      (advice-add #'company-box-icons--elisp :override #'+company-box-icons--elisp-fn))

    (when (and (display-graphic-p)
               (require 'all-the-icons nil t))
      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (declare-function all-the-icons-octicon 'all-the-icons)
      (setq company-box-icons-all-the-icons
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
              (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
              (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
              (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
              (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
              (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink)))
            company-box-icons-alist 'company-box-icons-all-the-icons))))

;;;; Dap
(defun config/post-init-dap ()
  (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra))))  ;; Open a Hydra debug menu whenever hitting a breakpoint

(defun config/init-dap-shell ()
  (use-package dap-shell))

;;;; Drag-stuff
(defun config/post-init-drag-stuff ()
  (drag-stuff-global-mode t)
  (global-set-key (kbd "<C-up>") 'drag-stuff-up)
  (global-set-key (kbd "<C-down>") 'drag-stuff-down))

;;;; Dtrt-indent
(defun config/init-dtrt-indent ()
  (use-package dtrt-indent))

;;;; Elixir
(defun config/post-init-elixir-mode ()

;;;;; Error handling

  (with-eval-after-load 'flycheck
    '(flycheck-credo-setup)
    '(flycheck-dialyxir-setup))
  (with-eval-after-load 'lsp-ui
    '(flycheck-add-next-checker 'lsp-ui 'elixir-credo))
  (with-eval-after-load 'elixir-mode
    (add-hook 'elixir-mode-hook #'flycheck-mode))

;;;;; Keybindings

  ;; Set up some custom keybindings that's not automatically configured using elixir-mode
  (with-eval-after-load 'elixir-mode
    (spacemacs/declare-prefix-for-mode 'elixir-mode
      "mt" "test" "testing related functionality")
    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
      "tb" 'exunit-verify-all
      "ta" 'exunit-verify
      "tl" 'exunit-rerun
      "tt" 'exunit-verify-single
      "tu" 'exunit-verify-all-in-umbrella
      "hd" 'dash-at-point
      "hD" 'dash-at-point-with-docset))

;;;;; Debugging

  ;; Register your debug run configurations here
  ;; (see dap-elixir for defaults injected into the "Elixir" type)
  (require 'dap-elixir)
  (with-eval-after-load 'dap-mode
    ;; (dap--put-if-absent :type "shell")
    ;; Task for running a Phoenix Server
    (dap-register-debug-template "Elixir Phoenix Server"
                                 (list :name "Elixir::Phoenix Server"
                                       :type "Elixir"
                                       :task "phx.server"
                                       :request "launch"
                                       :program nil
                                       :cwd nil  ;; defaults to lsp-workspace-root
                                       ))

    ;; Task for running an interactive mix session
    (dap-register-debug-template "Elixir Interactive Shell"
                                 (list :name "Elixir::Interactive Shell"
                                       :type "shell"
                                       :command "iex"
                                       :taskArgs (list "-S")
                                       :task "mix"
                                       :request "launch"
                                       :program nil
                                       :cwd nil)))

;;;;; Polymode (using web-mode for inline html)

  ;; Use polymode to enable web mode for inline live view templates
  ;; https://blog.evalcode.com/phoenix-liveview-inline-syntax-highlighting-for-emacs/
  ;; TODO: Make it work for a list of sigils (~L, ~F)
  ;; FIXME: How to enable auto-complete for web mode while editing html blocks??
  ;; FIXME this seems to fuck up lsp-auto formatting?
  ;; (use-package polymode
  ;;   :mode ("\.ex$" . poly-elixir-web-mode)
  ;;   :config
  ;;   (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  ;;   (define-innermode poly-liveview-expr-elixir-innermode
  ;;     :mode 'web-mode
  ;;     :head-matcher (rx line-start (* space) "~L" (= 3 (char "\"'")) line-end)
  ;;     :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
  ;;     :head-mode 'host
  ;;     :tail-mode 'host
  ;;     :allow-nested nil
  ;;     :keep-in-mode 'host
  ;;     :fallback-mode 'host)
  ;;   (define-innermode poly-liveview-surfaceui-expr-elixir-innermode
  ;;     :mode 'web-mode
  ;;     :head-matcher (rx line-start (* space) "~F" (= 3 (char "\"'")) line-end)
  ;;     :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
  ;;     :head-mode 'host
  ;;     :tail-mode 'host
  ;;     :allow-nested nil
  ;;     :keep-in-mode 'host
  ;;     :fallback-mode 'host)
  ;;   (define-polymode poly-elixir-web-mode
  ;;     :hostmode 'poly-elixir-hostmode
  ;;     :innermodes '(poly-liveview-expr-elixir-innermode
  ;;                   poly-liveview-surfaceui-expr-elixir-innermode)))
  ;; (add-to-list 'web-mode-engines-alist ("elixir" . "\\.ex\\"))


;;;;; Mode hooks
  ;; Auto-highlight symbols
  (add-hook 'elixir-mode-hook 'auto-highlight-symbol-mode)

  ;; Enable Dash documentation
  (add-hook 'elixir-mode-hook (lambda () (setq-local counsel-dash-docsets '("Elixir")
                                                     dash-at-point-docset "elixir")))
  ;; Format buffer on save
  (add-hook 'elixir-mode-hook (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

  ;; Try to delete all tabs - NOTE does not quite work as intended
  (add-hook 'elixir-mode-hook (lambda () (untabify (point-min) (point-max))))

  ;; Set custom settings for elixir-ls language server
  (add-hook 'lsp-after-initialize-hook (lambda () (lsp-register-custom-settings '(("elixirLS.projectDir" lsp-elixir-project-dir)))))

  ;; Add some ignore patterns for stuff I don't care about
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
  ;; Toggle between snake case, camel case and pascal case
  (use-package evil-string-inflection :ensure t)
  (define-key evil-normal-state-map "gC" 'evil-operator-string-inflection))

;;;; Flyspell
(defun config/post-init-flyspell ()
  (let ((langs '("english" "norsk")))
    (setq lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert lang-ring elem)))

  (global-set-key (kbd "<f8>") 'cycle-ispell-languages))

;;;; LSP-mode
(defun config/post-init-lsp-mode ()
  (spacemacs|use-package-add-hook lsp-after-open-hook #'lsp-origami-try-enable))

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

;;;;; Ivy-todo
(defun config/init-ivy-todo ()
  (use-package ivy-todo
    :init (progn (spacemacs/set-leader-keys "pO" 'ivy-todo/task-list))))

;;;; Magit
(defun config/post-init-magit ()
  (use-package forge
    :after magit
    :defer t
    :config
    (add-to-list 'forge-alist '("gitlab.intility.com" "gitlab.intility.com/api/v4" "gitlab.intility.com" forge-gitlab-repository))
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
        org-export-in-background nil ;; Async export not working when ox is bytecompiled?
        org-fontify-whole-heading-line t
        org-fontify-done-headline nil
        org-fontify-quote-and-verse-blocks t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-indent-indentation-per-level 1
        org-log-state-notes-into-drawer t
        org-log-done-with-time t
        ;; org-refile-targets '((org-agenda-files . (:maxlevel . 6)))  ;; TODO https://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
        org-re-reveal-root "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.9.2"
        org-re-reveal-revealjs-version "3.8"
        org-re-reveal-title-slide "<h1>%t</h1><h2>%s</h2><h4>%e</h4>"
        org-startup-indented t
        org-pretty-entities t
        org-priority-faces '((65 :inherit org-priority :foreground "red")
                             (66 :inherit org-priority :foreground "brown")
                             (67 :inherit org-priority :foreground "blue"))
        org-time-stamp-custom-formats '("<%a %d.%m.%Y>" . "<%a %d.%m.%Y %H:%M>"))

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook (lambda () (progn
                                        (require 'org-tempo)  ;; Required for new org templating system to work
                                        (setq line-spacing 0.2
                                              header-line-format " "
                                              left-margin-width 2
                                              right-margin-width 2))))

  ;; Org LaTeX, templates (also for PDF exports)
  (with-eval-after-load 'ox-latex
    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-minted-options '(("breaklines" "true")
                                     ("breakanywhere" "true")))
    (setq org-latex-pdf-process
          (list (concat "latexmk "
                        "-xelatex "
                        "-recorder -synctex=1 -bibtex-cond %b")))
    (setq org-latex-classes
          '(("article"
             "\\RequirePackage{fix-cm}
\\PassOptionsToPackage{svgnames}{xcolor}
\\documentclass[11pt]{article}
\\usepackage{fontspec}
\\setmainfont{ETBembo RomanOSF}
\\setsansfont[Scale=MatchLowercase]{Raleway}
\\setmonofont[Scale=MatchLowercase]{ETBembo}
\\usepackage{sectsty}
\\allsectionsfont{\\sffamily}
\\usepackage{enumitem}
\\setlist[description]{style=unboxed,font=\\sffamily\\bfseries}
\\usepackage{listings}
\\lstset{frame=single,aboveskip=1em,
	framesep=.5em,backgroundcolor=\\color{AliceBlue},
	rulecolor=\\color{LightSteelBlue},framerule=1pt}
\\usepackage{xcolor}
\\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
\\lstset{basicstyle=\\basicdefault{\\spaceskip1em}}
\\lstset{literate=
	    {§}{{\\S}}1
	    {©}{{\\raisebox{.125ex}{\\copyright}\\enspace}}1
	    {«}{{\\guillemotleft}}1
	    {»}{{\\guillemotright}}1
	    {Á}{{\\'A}}1
	    {Ä}{{\\\"A}}1
	    {É}{{\\'E}}1
	    {Í}{{\\'I}}1
	    {Ó}{{\\'O}}1
	    {Ö}{{\\\"O}}1
	    {Ú}{{\\'U}}1
	    {Ü}{{\\\"U}}1
	    {ß}{{\\ss}}2
	    {à}{{\\`a}}1
	    {á}{{\\'a}}1
	    {ä}{{\\\"a}}1
	    {é}{{\\'e}}1
	    {í}{{\\'i}}1
	    {ó}{{\\'o}}1
	    {ö}{{\\\"o}}1
	    {ú}{{\\'u}}1
	    {ü}{{\\\"u}}1
	    {¹}{{\\textsuperscript1}}1
            {²}{{\\textsuperscript2}}1
            {³}{{\\textsuperscript3}}1
	    {ı}{{\\i}}1
	    {—}{{---}}1
	    {’}{{'}}1
	    {…}{{\\dots}}1
            {⮠}{{$\\hookleftarrow$}}1
	    {␣}{{\\textvisiblespace}}1,
	    keywordstyle=\\color{DarkGreen}\\bfseries,
	    identifierstyle=\\color{DarkRed},
	    commentstyle=\\color{Gray}\\upshape,
	    stringstyle=\\color{DarkBlue}\\upshape,
	    emphstyle=\\color{Chocolate}\\upshape,
	    showstringspaces=false,
	    columns=fullflexible,
	    keepspaces=true}
\\usepackage[a4paper,margin=1in,left=1.5in]{geometry}
\\usepackage{parskip}
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\begingroup\\parindent0pt
  \\sffamily
  \\Huge{\\bfseries\\@title}\\par\\bigskip
  \\LARGE{\\bfseries\\@author}\\par\\medskip
  \\normalsize\\@date\\par\\bigskip
  \\endgroup\\@afterindentfalse\\@afterheading}
\\makeatother
[DEFAULT-PACKAGES]
\\hypersetup{linkcolor=Blue,urlcolor=DarkBlue,
  citecolor=DarkRed,colorlinks=true}
\\AtBeginDocument{\\renewcommand{\\UrlFont}{\\ttfamily}}
[PACKAGES]
[EXTRA]"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

            ("report" "\\documentclass[11pt]{report}"
             ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

            ("book" "\\documentclass[11pt]{book}"
             ("\\part{%s}" . "\\part*{%s}")
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
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
    "g"   'org-mark-ring-goto
    "h"   'org-metaleft                    ;; Because of MacOS's damned, indestructable M-h binding...
    "d o" 'org-toggle-time-stamp-overlays  ;; Required to toggle off before changing time when using custom formats
    "s p" 'org-sort-entries-priorities)

  ;; Custom file handlers for org-mode (MacOS) FIXME not working
  ;; (when (string= system-type "darwin")
  ;;   (push '("\\.docx?\\'"   . "open %s") org-file-apps-macos)
  ;;   (push '("\\.x?html?\\'" . "open %s") org-file-apps-macos))
  )

;;;;; Org-Roam
(defun config/post-init-org-roam ()
  (with-eval-after-load 'org-roam
    (cl-defmethod org-roam-node-filetitle ((node org-roam-node))
      "Return the file TITLE for the node."
      (org-roam-get-keyword "TITLE" (org-roam-node-file node)))

    (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
      "Return the hierarchy for the node."
      (let ((title (org-roam-node-title node))
            (olp (org-roam-node-olp node))
            (level (org-roam-node-level node))
            (filetitle (org-roam-node-filetitle node)))
        (concat
         (if (> level 0) (concat filetitle " -> "))
         (if (> level 1) (concat (string-join olp " -> ") " -> "))
         title))
      )

    ;; Display node hierarchy in the org-roam-node-find list
    (setq org-roam-node-display-template "${hierarchy:*} ${tags:20}")

    ;; Encrypt org-roam files by default (NOTE does currently not work good with `org-roam-node-find' in v2)
    ;; (setq org-roam-capture-templates '(("d" "default" plain "%?"
    ;;                                     :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n")
    ;;                                     :unnarrowed t)))

    ;; Allow mouse navigation in backlink buffer
    (define-key org-roam-mode-map [mouse-1] #'org-roam-visit-thing))

  ;; Use side window for backlink buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.25)
                 (preserve-size . (t nil))
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  )

;;;;; Org-projectile
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

;;;;; Debugging

  (require 'dap-python)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python"))))

;;;;; Generic after loading python

  (with-eval-after-load 'python
    (setq python-shell-interpreter "python3")

    ;; TODO Figure out how to run poetry automatically when entering a python project.
    ;; FIXME This breaks python snippets in org-mode - should only apply for actual python projects
    ;; Still have to run poetry command manually once
    ;; (use-package poetry
    ;;   :ensure t
    ;;   :hook ((python-mode . poetry-tracking-mode)
    ;;          (python-mode . (lambda () (when (poetry-venv-exist-p) ;; FIXME This fails if run before poetry is initialized for project
    ;;                                      (setq-local lsp-pyright-venv-path poetry-project-venv))))))
    (custom-set-variables
     '(flycheck-python-flake8-executable "python3")
     '(flycheck-python-pycompile-executable "python3")
     '(flycheck-python-pylint-executable "python3"))

    (spacemacs/set-leader-keys-for-major-mode 'python-mode "p" 'poetry)))

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
               ("C-t" . ranger-travel)
               ("C-e" . ranger-pop-eshell)
               ("M-1" . winum-select-window-1)
               ("M-2" . winum-select-window-2)
               ("M-3" . winum-select-window-3)
               ("M-4" . winum-select-window-4)
               ("M-5" . winum-select-window-5))))

;;;; Web-mode
(defun config/pre-init-web-mode ()
  "Various changes to how web-mode (and minors) should work"

  ;; Workaround for emacs lockfiles causing node to crash
  ;; https://github.com/facebook/create-react-app/issues/9056#issuecomment-633540572
  (dolist (mode-hook '(web-mode-hook
                       css-mode-hook
                       scss-mode-hook
                       rjsx-mode-hook
                       typescript-mode-hook
                       javascript-mode-hook))
    (add-hook mode-hook (lambda () (setq-local create-lockfiles nil))))

  ;; Enable sgml-electric-tag-pair-mode for some minor modes
  (dolist (mode-hook '(html-mode-hook
                       rjsx-mode-hook
                       typescript-tsx-mode-hook
                       xml-mode))
    (add-hook mode-hook 'sgml-electric-tag-pair-mode))

  ;; Disable smartparens strict mode in order to be able to write out
  ;; arrow functions like ie. `() => {...}'
  (dolist (mode-hook '(rjsx-mode-hook
                       typescript-mode-hook
                       typescript-tsx-mode-hook
                       javascript-mode-hook))
    (add-hook mode-hook 'turn-off-smartparens-strict-mode)))

;;;; Writeroom-mode
(defun config/post-init-writeroom-mode ()
  "See configuration options here
   https://github.com/joostkremers/writeroom-mode"

  (setq writeroom-width 140)
  (with-eval-after-load 'writeroom-mode
    (define-key writeroom-mode-map (kbd "C-<") #'writeroom-decrease-width)
    (define-key writeroom-mode-map (kbd "C->") #'writeroom-increase-width)
    (define-key writeroom-mode-map (kbd "C-=") #'writeroom-adjust-width))

  ;; Recalculate margins after text size adjustment
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust))

;;;; Yasnippet
(defun config/post-init-yasnippet-snippets ()
  (add-to-list 'yas-snippet-dirs 'yasnippet-snippets-dir t)
  (define-key prog-mode-map (kbd "C-c C-i") 'yas-insert-snippet))

;;; Owned Packages
;;;; Auto Dim Other Buffers
(defun config/init-auto-dim-other-buffers ()
  (use-package auto-dim-other-buffers
    :config
    (auto-dim-other-buffers-mode)))

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
                     ;; ("C-j"                 . counsel-outline)
                     ("C-j"                 . oi-jump)
                     ([(meta return)]       . org-meta-return)
                     ([(meta shift return)] . org-insert-subheading))
          (advice-add 'org-insert-heading    :before 'org-fix-heading-pos)
          (advice-add 'org-insert-heading    :after 'evil-insert-advice)
          (advice-add 'org-insert-subheading :after 'evil-insert-advice)))
      )))

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

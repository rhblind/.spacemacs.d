;;; -*- lexical-binding: t; -*-

(configuration-layer/declare-layers
 '(;; Core
   (auto-completion :variables
                    auto-completion-return-key-behavior 'complete
                    auto-completion-tab-key-behavior 'complete
                    auto-completion-idle-delay 0.2
                    auto-completion-private-snippets-directory nil
                    auto-completion-enable-snippets-in-popup t
                    auto-completion-enable-help-tooltip nil
                    auto-completion-use-company-box t
                    auto-completion-enable-sort-by-usage t)
   better-defaults
   (dash :variables
         dash-docs-docset-newpath (cond ((eq system-type 'gnu/linux) "~/.local/share/Zeal/Zeal/docsets")
                                        ((eq system-type 'darwin) "~/Library/Application Support/Dash/DocSets")))
   docker
   (multiple-cursors :variables
                     multiple-cursors-backend 'evil-mc)
   dap
   (lsp :variables
        lsp-use-lsp-ui t
        lsp-lens-enable t
        lsp-headerline-breadcrumb-enable nil
        lsp-ui-remap-xref-keybindings t
        lsp-navigation 'peek)
   git
   (ivy :variables
        ivy-extra-directories nil)
   (org :variables
        org-enable-valign t
        org-enable-roam-support t
        org-enable-github-support t
        org-enable-reveal-js-support t
        org-want-todo-bindings t)
   (shell :variables
          close-window-with-terminal t
          ;; shell-default-shell 'vterm
          shell-default-shell 'eshell
          shell-default-term-shell "/usr/local/bin/zsh"
          )
   (spell-checking :variables
                   spell-checking-enable-by-default t
                   enable-flyspell-auto-completion nil)
   (syntax-checking :variables
                    syntax-checking-enable-by-default t)
   (version-control :variables
                    version-control-diff-side 'left
                    version-control-global-margin t
                    version-control-diff-tool 'git-gutter+)

   ;; Misc
   graphviz
   nginx
   (ranger :variables
           ranger-show-preview t
           ranger-show-literal nil
           ranger-show-hidded t
           ranger-cleanup-eagerly t
           ranger-ignored-extensions '("mkv" "iso" "mp4" "flv"))
   (ibuffer :variables
            ibuffer-group-buffers-by 'projects)
   (osx :variables
        osx-option-as 'meta
        osx-right-option-as 'none)
   (unicode-fonts :variables unicode-fonts-force-multi-color-on-mac t)

   ;; Markups
   csv
   (html :variables
         css-enable-lsp t
         css-indent-offset 2
         less-enable-lsp t
         scss-enable-lsp t
         html-enable-lsp t
         web-fmt-tool 'prettier
         web-mode-markup-indent-offset 2
         web-mode-css-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-attr-indent-offset 2)
   (latex :variables
          latex-build-command "LaTex"
          latex-enable-folding t
          latex-enable-magic t)
   (markdown :variables
             markdown-command "/usr/local/bin/pandoc"
             markdown-live-preview-engine 'vmd)
   yaml
   (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
   ;; Languages
   (csharp :variables
           csharp-backend 'lsp)
   dotnet
   (elixir :variables
           elixir-backend 'lsp
           elixir-ls-path "~/.local/opt/elixir-ls")
   emacs-lisp
   (erlang :variables
           erlang-backend 'lsp)
   (fsharp :variables
           fsharp-backend 'lsp)
   (go :variables
       go-backend 'lsp)
   prettier
   ruby
   (typescript :variables
               typescript-backend 'lsp
               typescript-fmt-on-save t
               typescript-fmt-tool 'prettier
               typescript-indent-level 2
               typescript-linter 'eslint
               typescript-lsp-linter nil)
   (javascript :variables
               javascript-backend 'lsp
               javascript-lsp-linter nil
               javascript-disable-tern-port-files nil
               javascript-fmt-tool 'prettier
               javascript-fmt-on-save t
               javascript-repl `nodejs
               js-indent-level 2
               js2-basic-offset 2
               js2-include-node-externs t
               js2-mode-show-strict-warnings nil ;; js2-mode is sometimes confused by the type syntax when using lsp backend
               js2-mode-show-parse-errors nil    ;; js2-mode is sometimes confused by the type syntax when using lsp backend
               node-add-modules-path t)
   django
   (python :variables
           python-backend 'lsp
           python-lsp-server 'pyright
           python-pipenv-activate nil ;; using poetry.el for python
           python-test-runner '(pytest nose)
           python-save-before-test t
           python-formatter 'lsp
           python-spacemacs-indent-guess t)
   react
   ;; rust
   sql
   windows-scripts
   ))

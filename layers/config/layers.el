;;; -*- lexical-binding: t; -*-

(configuration-layer/declare-layers
 '(;; Core
   (auto-completion :variables
                    auto-completion-return-key-behavior 'complete
                    auto-completion-tab-key-behavior 'complete
                    auto-completion-enable-snippets-in-popup t
                    auto-completion-enable-help-tooltip t
                    auto-completion-enable-sort-by-usage t)
   better-defaults
   (dash :variables
         dash-docs-docset-newpath: "~/.local/share/Zeal/Zeal/docsets") ;; FIXME: fix MacOS compatibility
   docker
   (multiple-cursors :variables
                     multiple-cursors-backend 'evil-mc)
   git
   (ivy :variables
        ivy-extra-directories nil)
   (org :variables
        org-enable-github-support t
        org-enable-reveal-js-support t
        org-want-todo-bindings t)
   (shell :variables
          shell-default-shell 'eshell)
   ;; (spell-checking :variables
   ;;                 spell-checking-enable-by-default t
   ;;                 enable-flyspell-auto-completion t)
   syntax-checking
   (version-control :variables
                    version-control-diff-side 'left
                    version-control-global-margin t
                    version-control-diff-tool 'git-gutter+)

   ;; Misc
   graphviz
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
   html
   markdown
   yaml

   ;; Languages
   lsp
   (elixir :variables
           elixir-backend 'lsp
           elixir-ls-path "~/.local/opt/elixir-ls")
   emacs-lisp
   erlang
   (javascript :variables
               javascript-disable-tern-port-files nil)
   django
   (python :variables
           python-backend 'lsp
           python-lsp-server 'mspyls
           ;; python-lsp-git-root "~/.local/opt/python-language-server"
           python-pipenv-activate t
           python-test-runner 'pytest
           python-save-before-test t
           python-formatter 'yapf
           python-spacemacs-indent-guess nil)

   windows-scripts
   ))

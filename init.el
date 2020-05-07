;;; Setup -*- lexical-binding: t; -*-
;;;; Commentary

;; Ligatures and icons require installation - see README.
;;
;; Layers are declared in `layers/config/layers.el'.
;;
;; Set `server?'        to true if you - use emacs as a daemon.
;;
;; `init.el' sets spacemacs up, defining required `dotspacemacs/..' funcs & vars.

;;;; Constants

(defconst linux?   (eq system-type 'gnu/linux)  "Are we on a linux machine?")
(defconst mac?     (eq system-type 'darwin)     "Are we on a macOS machine?")
(defconst windows? (eq system-type 'windows-nt) "Are we on windows machine?")

;;;; Configuration

(defvar server? t
  "Alias `dotspacemacs-enable-server'. Set true if running emacs as a daemon")

;;; Spacemacs/
;;;; Spacemacs/init

(defun dotspacemacs/init ()
  "Instantiate Spacemacs core settings.

All `dotspacemacs-' variables with values set different than their defaults.

They are all defined in `~/.emacs.d/core/core-dotspacemacs.el'.
Check `dotspacemacs/get-variable-string-list' for all vars you can configure."
  (setq-default
   ;; Display
   dotspacemacs-default-font `("Fira Code",
                               :size ,(if (= 1440 (display-pixel-height)) 15 13))
   dotspacemacs-themes       '(solarized-light
                               doom-gruvbox)
   ;; General
   dotspacemacs-auto-generate-layout-names t
   dotspacemacs-auto-resume-layouts        t
   dotspacemacs-editing-style              '(vim :variables
                                                 vim-style-visual-feedback t)
   dotspacemacs-elpa-https                 t
   dotspacemacs-elpa-subdirectory          nil
   dotspacemacs-enable-server              server?
   dotspacemacs-fullscreen-at-startup      nil
   dotspacemacs-large-file-size            5
   dotspacemacs-helm-use-fuzzy             'always
   dotspacemacs-line-numbers               '(
                                             :relative t
                                             :disabled-for-modes dired-mode doc-view-mode markdown-mode org-mode pdf-view-mode
                                             :size-limit-kb 5000)
   dotspacemacs-persistent-server          server?
   dotspacemacs-pretty-docs                t
   dotspacemacs-search-tools               '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-smartparens-strict-mode    t
   dotspacemacs-smart-closing-parenthesis  nil
   dotspacemacs-remap-Y-to-y$              nil
   dotspacemacs-scratch-mode               'org-mode
   dotspacemacs-startup-banner             nil
   dotspacemacs-startup-lists              nil
   dotspacemacs-whitespace-cleanup         'trailing

   ;; The following are unchanged but are still required for reloading via
   ;; 'SPC f e R' `dotspacemacs/sync-configuration-layers' to not throw warnings
   dotspacemacs-emacs-leader-key  "M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-leader-key        "SPC"
   dotspacemacs-mode-line-theme   'all-the-icons))

;;;; Spacemacs/layers

(defun dotspacemacs/layers ()
  "Instantiate Spacemacs layers declarations and package configurations."
  (setq-default
   dotspacemacs-configuration-layers     '((config   :location local)
                                           (display  :location local)
                                           (personal :location local))
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-additional-packages      '(company-lsp
                                           counsel-dash
                                           doct
                                           dap-mode
                                           drag-stuff
                                           dtrt-indent
                                           evil-mc
                                           evil-string-inflection
                                           exunit
                                           forge
                                           keychain-environment
                                           live-py-mode
                                           lsp-ui
                                           org-fancy-priorities
                                           org-gcal
                                           org-super-agenda
                                           org-superstar
					                                 pinentry
                                           rainbow-mode
                                           posframe
                                           quelpa
                                           quelpa-use-package)
   dotspacemacs-frozen-packages          '()
   dotspacemacs-excluded-packages
   '(;; Must Exclude (for styling, functionality, bug-fixing reasons)
     fringe importmagic scss-mode vi-tilde-fringe

     ;; Packages I don't use (non-exhaustive)
     anzu centered-cursor-mode column-enforce-mode company-statistics
     doom-modeline eshell-prompt-extras evil-anzu evil-tutor
     fancy-battery fill-column-indicator gnuplot golden-ratio indent-guide
     iedit live-py-mode multi-term mwim neotree org-bullets paradox py-isort
     yapfify

     ;; Packages that is installed as a dependency of others, but I don't want installed
     alchemist)))

;;;; Spacemacs/user-init

(defun dotspacemacs/user-init ()
  "Package independent settings to run before `dotspacemacs/user-config'."
  (fringe-mode 0)
  (require 'iso-transl)  ;; Enables "dead keys" for non-english keyboards
  (require 'epa-file)    ;; Load library for decrypting the `secrets.el.gpg' file
  (require 'pinentry)    ;; Allows unlocking gpg keys using the Emacs minibuffer (gpg --> gpg-agent --> pinentry --> Emacs)

  (setq epa-pinentry-mode 'loopback)

  (pinentry-start)
  (epa-file-enable)

  (setq auto-resume-layers t
        auth-source-debug nil  ;; Enable logging of authentication related stuff to the `*Messages' buffer. Disable when not needed!
        custom-file "~/.spacemacs.d/.custom-settings.el"
        secrets-file "~/.spacemacs.d/secrets.el.gpg")

  ;; This file keeps secrets for emacs configurations
  (load-file secrets-file)

  (when (spacemacs/system-is-mac)
    (setq shell-file-name "/bin/bash")
    (setq dired-listing-switches "-aBhl --group-directories-first"
          helm-locate-command "glocate %s -e -A --regex %s"
          helm-locate-recursive-dirs-command "glocate -i -e -A --regex '^%s' '%s.*$'"
          insert-directory-program "/usr/local/bin/gls"
          ))
  (when (spacemacs/system-is-linux)
    (setq shell-file-name "/bin/bash"))
  )

;;;; Spacemacs/user-config
;;;;; Post Layer Load

(defun dotspacemacs/user-config/post-layer-load-config ()
  "Configuration to take place *after all* layers/pkgs are instantiated."
  (when (and (boundp 'redo-bindings?) redo-bindings?
             (configuration-layer/package-used-p 'redo-spacemacs))
    (redo-spacemacs-bindings))

  )

;;;;; Core
(defun dotspacemacs/user-config ()
  "Configuration that cannot be delegated to layers."
  (dotspacemacs/user-config/post-layer-load-config))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(package-selected-packages
     '(zeal-at-point yasnippet-snippets yaml-mode xterm-color ws-butler writeroom-mode winum which-key wgrep web-mode web-beautify vterm volatile-highlights uuidgen unicode-fonts unfill treemacs-projectile treemacs-persp treemacs-magit treemacs-evil toc-org terminal-here tagedit symon symbol-overlay spaceline-all-the-icons solarized-theme smex smeargle slim-mode shell-pop sass-mode reveal-in-osx-finder restart-emacs ranger rainbow-mode rainbow-delimiters quelpa-use-package pytest pyenv-mode pug-mode prettify-utils prettier-js powershell posframe popwin pony-mode pippel pipenv pip-requirements pinentry pcre2el password-generator ox-gfm overseer outshine osx-trash osx-dictionary osx-clipboard orgit org-superstar org-super-agenda org-re-reveal org-projectile org-present org-pomodoro org-mime org-gcal org-fancy-priorities org-download org-cliplink org-brain open-junk-file ob-elixir nodejs-repl nameless move-text mmm-mode markdown-toc magit-svn magit-section magit-gitflow macrostep lsp-ui lsp-python-ms lorem-ipsum livid-mode link-hint launchctl keychain-environment json-navigator js2-refactor js-doc ivy-yasnippet ivy-xref ivy-purpose ivy-hydra impatient-mode ibuffer-projectile hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-make graphviz-dot-mode google-translate gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ gh-md fuzzy forge font-lock+ flyspell-correct-ivy flycheck-pos-tip flycheck-package flycheck-mix flycheck-elsa flycheck-credo flx-ido faceup eyebrowse exunit expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-textobj-line evil-surround evil-string-inflection evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args eval-sexp-fu eshell-z esh-help erlang emmet-mode elisp-slime-nav editorconfig dumb-jump dtrt-indent drag-stuff dotenv-mode doom-themes doct dockerfile-mode docker diminish devdocs dap-mode cython-mode csv-mode counsel-projectile counsel-dash counsel-css company-web company-tern company-lsp company-box company-anaconda clean-aindent-mode browse-at-remote blacken auto-yasnippet auto-highlight-symbol auto-dim-other-buffers auto-dictionary auto-compile all-the-icons-ivy all-the-icons-dired aggressive-indent add-node-modules-path ace-link ac-ispell)))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((((class color) (min-colors 89)) (:foreground "#657b83" :background "#fdf6e3"))))
   '(auto-dim-other-buffers-face ((t (:background "#fcf4df"))))
   '(avy-background-face ((t (:italic nil))))
   '(company-tooltip-common ((t (:weight bold :underline nil :inherit company-tooltip))))
   '(company-tooltip-common-selection ((t (:weight bold :underline nil :inherit company-tooltip-selection))))
   '(font-lock-comment-face ((t (:foreground "#586e75" :italic t :weight normal))))
   '(font-lock-doc-face ((t (:foreground "#2aa198" :italic t :weight normal))))
   '(fringe ((t (:background nil))))
   '(mode-line ((t (:box nil :underline nil :background nil))))
   '(mode-line-inactive ((t (:box nil :underline nil))))
   '(org-block-begin-line ((t (:inherit fixed-pitch :italic nil :underline nil :box t))))
   '(org-block-end-line ((t (:inherit fixed-pitch :italic nil :underline nil :box t))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-title ((t (@headline (\, @variable-tuple) :underline nil :inherit nil :height 2))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-level-1 ((t (@headline (\, @variable-tuple) :underline nil :inherit nil :height 1.75 :foreground "#a71d31"))))
   '(org-level-2 ((t (@headline (\, @variable-tuple) :underline nil :inherit nil :height 1.5 :foreground "#8D6B94"))))
   '(org-level-3 ((t (@headline (\, @variable-tuple) :underline nil :inherit nil :height 1.25))))
   '(org-level-4 ((t (@headline (\, @variable-tuple) :underline nil :inherit nil :height 1.1))))
   '(org-level-5 ((t (@headline (\, @variable-tuple) :underline nil :inherit nil))))
   '(org-level-6 ((t (@headline (\, @variable-tuple) :underline nil :inherit nil))))
   '(org-level-7 ((t (@headline (\, @variable-tuple) :underline nil :inherit nil))))
   '(org-level-8 ((t (@headline (\, @variable-tuple) :underline nil :inherit nil))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   '(set-face-attribute ((t ('git-gutter+-added :background nil :foreground "green"))))
   '(setq ((t (git-gutter+-modified-sign "!"))))
   '(sp-show-pair-match-face ((t (:background "CadetBlue3")))))
  )

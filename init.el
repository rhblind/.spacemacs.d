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

(defconst linux?   (eq system-type 'gnu/linux) "Are we on a linux machine?")
(defconst mac?     (eq system-type 'darwin)    "Are we on a macOS machine?")
(defconst windows? (not (or linux? mac?))      "Are we on windows machine?")

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
   dotspacemacs-default-font '(("Fira Code"
                                :size 13
                                :weight medium
                                :width normal)
                               ("Fira Code Symbol"
                                :size 13
                                :weight normal
                                :width normal
                                ))
   dotspacemacs-themes       '(solarized-light
                               doom-gruvbox
                               zenburn)

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
   dotspacemacs-line-numbers               '(:relative t
                                                       :disabled-for-modes dired-mode
                                                       doc-view-mode
                                                       markdown-mode
                                                       org-mode
                                                       pdf-view-mode
                                                       :size-limit-kb 1000)
   dotspacemacs-persistent-server          server?
   dotspacemacs-pretty-docs                t
   dotspacemacs-search-tools               '("rg ""ag" "pt" "ack" "grep")
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
   dotspacemacs-additional-packages      '(buttercup
                                           dap-mode
                                           drag-stuff
                                           dtrt-indent
                                           exec-path-from-shell
                                           exunit
                                           rainbow-mode
                                           quelpa
                                           quelpa-use-package)
   dotspacemacs-frozen-packages          '()
   dotspacemacs-excluded-packages
   '(;; Must Exclude (for styling, functionality, bug-fixing reasons)
     fringe importmagic scss-mode vi-tilde-fringe

     ;; Packages I don't use (non-exhaustive)
     anzu centered-cursor-mode column-enforce-mode company-statistics
     doom-modeline eshell-prompt-extras evil-anzu evil-mc evil-tutor
     fancy-battery fill-column-indicator gnuplot golden-ratio indent-guide
     live-py-mode multi-term multiple-cursors mwim neotree paradox py-isort
     yapfify

     ;; Packages that is installed as a dependency of others, but I don't want installed
     alchemist)))

;;;; Spacemacs/user-init

(defun dotspacemacs/user-init ()
  "Package independent settings to run before `dotspacemacs/user-config'."
  (fringe-mode 0)

  ;; Enables "dead keys" for non-english keyboards
  (require 'iso-transl)
  (setq auto-resume-layers t
        custom-file "~/.spacemacs.d/.custom-settings.el")

  (when (spacemacs/system-is-mac)
    (setq dired-listing-switches "-aBhl --group-directories-first"
          helm-locate-command "glocate %s -e -A --regex %s"
          helm-locate-recursive-dirs-command "glocate -i -e -A --regex '^%s' '%s.*$'"
          insert-directory-program "/usr/local/bin/gls"
          ))
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
  (dotspacemacs/user-config/post-layer-load-config)
  )

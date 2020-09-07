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
(defconst linux?   (eq system-type 'gnu/linux)                   "Are we on a linux machine?")
(defconst mac?     (eq system-type 'darwin)                      "Are we on a macOS machine?")
(defconst windows? (eq system-type 'windows-nt)                  "Are we on a windows machine?")
(defconst wsl?     (and (not (eq (getenv "WSLENV") nil)) linux?) "Are we on a wsl environment?")

;;;; Configuration

(defvar debug? t
  "Set true to enable debug messages")
(defvar server? t
  "Alias `dotspacemacs-enable-server'. Set true if running emacs as a daemon")

;;; Spacemacs/
;;;; Spacemacs/init

(defun dotspacemacs/init ()
  "Instantiate Spacemacs core settings.

  All `dotspacemacs-' variables with values set different than their defaults.
They are all defined in `~/.emacs.d/core/core-dotspacemacs.el'.
Check `dotspacemacs/get-variable-string-list' for all vars you can configure."

  ;; Adopt a sneaky garbage collection strategy of waiting until idle time to
  ;; collect; staving off the collector while the user is working.
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb
        gcmh-verbose debug?)

  (setq-default
   ;; Display
   dotspacemacs-default-font `("Fira Code",
                               :size ,(if (= 1440 (display-pixel-height)) 15 13))
   dotspacemacs-themes       '(solarized-light
                               solarized-zenburn)
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
                                           company-org-roam
                                           counsel-dash
                                           doct
                                           doom-themes
                                           drag-stuff
                                           dtrt-indent
                                           evil-mc
                                           evil-string-inflection
                                           exec-path-from-shell
                                           exunit
                                           forge
                                           gcmh
                                           gdscript-mode
                                           keychain-environment
                                           live-py-mode
                                           lsp-ui
                                           org-fancy-priorities
                                           org-roam
                                           org-super-agenda
                                           org-superstar
	                                         pinentry
                                           rainbow-mode
                                           posframe)
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

  (setq epa-pinentry-mode 'loopback)  ;; Allows unlocking gpg keys using the Emacs minibuffer (gpg --> gpg-agent --> pinentry --> Emacs)
  (when (spacemacs/system-is-mac)
    (custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg")))
  (epa-file-enable)

  ;; Enable gcmh-mode
  (add-hook 'emacs-startup-hook #'gcmh-mode)

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

  (when (spacemacs/system-is-mac)
    (setq exec-path-from-shell-check-startup-files nil)  ;; Don't complain about putting thing in the wrong files
    (exec-path-from-shell-initialize)))

;;;;; Core
(defun dotspacemacs/user-config ()
  "Configuration that cannot be delegated to layers."
  (require 'window-purpose) ;; Workaround until https://github.com/bmag/emacs-purpose/issues/158 is fixed
  (dotspacemacs/user-config/post-layer-load-config))

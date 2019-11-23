;;; Setup -*- lexical-binding: t; -*-
;;;; Commentary

;; -- Eric Kaschalk's Spacemacs Configuration --
;; -- Contact: ekaschalk@gmail.com --
;; -- MIT License --
;; -- Emacs 26.1 ~ Spacemacs Dev Branch 0.300.0.x ~ pkgs updated: 1/21/19 --
;; -- http://modernemacs.com --
;;
;; Personal layers host most of my configuration - see README.
;; Ligatures and icons require installation - see README.
;;
;; Layers are declared in `layers/config/layers.el'.
;;
;; Set `redo-bindings?' to true if you - want my aggressive rebindings.
;; Set `server?'        to true if you - use emacs as a daemon.
;;
;; `init.el' sets spacemacs up, defining required `dotspacemacs/..' funcs & vars.

;;;; Constants

(defconst eric?    (string= "Eric Kaschalk" (user-full-name)) "Am I me?") ;; Delete this when not needed anymore
(defconst linux?   (eq system-type 'gnu/linux) "Are we on a linux machine?")
(defconst mac?     (eq system-type 'darwin)    "Are we on a macOS machine?")
(defconst windows? (not (or linux? mac?))      "Are we on windows machine?")

;;;; Configuration

(defvar server? t
  "Alias `dotspacemacs-enable-server'. Set true if running emacs as a daemon")

(defvar redo-bindings? (if eric? t nil)
  "Redo spacemacs bindings? Defaults to, and I recommend, nil to non-eric users.

See the commentary in the config layer's local pkg `redo-spacemacs'.")

;;; Spacemacs/
;;;; Spacemacs/init

(defun dotspacemacs/init ()
  "Instantiate Spacemacs core settings.

All `dotspacemacs-' variables with values set different than their defaults.

They are all defined in `~/.emacs.d/core/core-dotspacemacs.el'.
Check `dotspacemacs/get-variable-string-list' for all vars you can configure."
  (setq-default
   ;; Display
   ;; dotspacemacs-default-font `(,(if (x-list-fonts "Operator Mono")
   ;;                                  "operator mono medium"
   ;;                                "Source Code Pro")
   ;;                             :size ,(if (= 1440 (display-pixel-height)) 20 18))

   dotspacemacs-default-font '(("Fira Code"
                                :size 14
                                :weight medium
                                :width normal
                                :powerline-scale 1.1)
                               ("Fira Code Symbol"
                                :size 14
                                :weight normal
                                :width normal
                                :powerline-scale 1.1))
   dotspacemacs-themes       '(solarized-light
                               doom-gruvbox)

   ;; General
   dotspacemacs-auto-generate-layout-names nil
   dotspacemacs-editing-style              '(vim :variables
                                                 vim-style-visual-feedback t
   dotspacemacs-elpa-https                 t
   dotspacemacs-elpa-subdirectory          nil
   dotspacemacs-enable-server              server?
   dotspacemacs-fullscreen-at-startup      nil
   dotspacemacs-large-file-size            5
   dotspacemacs-persistent-server          server?
   dotspacemacs-pretty-docs                t
   dotspacemacs-search-tools               '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-scratch-mode               'org-mode
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
   dotspacemacs-additional-packages      '(buttercup)
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
  ;;(setq custom-file "~/.spacemacs.d/.custom-settings.el")
  ;; Enables "dead keys" for non-english keyboards
  (require 'iso-transl)

  (setq auto-resume-layers t)

  (when (spacemacs/system-is-mac)
    (setq insert-directory-program "/usr/local/bin/gls"
          ;; TODO - Check if locate can work with OS X after
          ;; 'sudo launchctl load -w /System/Library/LaunchDaemons/com.apple.locate.plist'
          ;; If so, submit PR to https://github.com/emacs-helm/helm/wiki/Locate
          helm-locate-command "glocate %s -e -A --regex %s"
          helm-locate-recursive-dirs-command "glocate -i -e -A --regex '^%s' '%s.*$'"
          dired-listing-switches "-aBhl --group-directories-first"))
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
)

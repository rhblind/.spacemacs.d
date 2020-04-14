;;; personal.el --- The random config dumping ground -*- lexical-binding: t; -*-

;; (require 'mu4e)
(provide 'personal)

;;; Globals
(global-company-mode)                                     ;; Enable company-mode globally
(global-unset-key [down-mouse-1])                         ;; No dragging nonsense
(global-set-key [down-mouse-1] 'mouse-select-window)      ;; Select window with mouse click
(rainbow-mode)                                            ;; Display color in buffer for color identifiers
(treemacs-resize-icons 14)                                ;; Treemacs icon size
(keychain-refresh-environment)                            ;; Refresh keychain information on start

;;;; Keybindings
(global-set-key (kbd "<C-return>") 'newline-below)
(global-set-key (kbd "<S-return>") 'newline-above)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)
(evil-ex-define-cmd "q[uit]" 'evil-delete-buffer)         ;; Redefine :q to delete buffer instead of exiting emacs

;;;; Variables
(setq display-time-24hr-format t                          ;; Use 24h clock
      layouts-enable-autosave t                           ;; Automatically save layouts
      layouts-autosave-delay 1800                         ;; Save layouts every 30 minutes
      lsp-ui-doc-enable nil                               ;; Disable ui-doc popup. Toggle help with ,hh
      pixel-scroll-mode t                                 ;; Enable pixel scrolling
      mouse-wheel-follow-mouse t                          ;; Scroll window under mouse
      mouse-wheel-progressive-speed nil                   ;; Don't accelerate scrolling
      mouse-wheel-scroll-amount '(1 ((shift) . 1))        ;; Mouse scroll 1 line at a time
      scroll-step 1                                       ;; Keyboard scroll 1 line at the time
      scroll-preserve-screen-position t
      scroll-conservatively 100
      user-full-name "Rolf Håvard Blindheim"
      user-email-address "rhblind@gmail.com"
      vc-follow-symlinks nil                              ;; Don't follow symlinks, edit them directly
      ws-butler-global-mode t                             ;; Enable ws-butler globally
      projectile-enable-caching t                         ;; Let projectile cache files
      projectile-project-search-path '("~/Documents/workspace")
      ;; projectile-globally-ignored-files '()
      ;; projectile-globally-ignored-file-suffixes '()
      projectile-globally-ignored-directories '(
                                                ".git"
                                                ".idea"
                                                ".elixir_ls"
                                                ".htmlcov"
                                                ".pytest_cache"
                                                "_build"
                                                "deps"
                                                "node_modules"
                                                )
      x-mouse-click-focus-ignore-position t               ;; Makes switching windows with mouse work on X-Window system
      )

;;; Org-mode
(setq org-directory "~/Dropbox/org"
      org-default-notes-file "~/Dropbox/org/todo.org"
      org-download-image-dir "~/Dropbox/org/pics"
      org-agenda-files (file-expand-wildcards "~/Dropbox/org/*.org"))

;; Don't enable this package as this config is not generally applicable

;;; Emacs Anywhere

;; ;; Emacs-anywhere defaults to org-mode with maximized window
;; (add-hook 'ea-popup-hook
;;           (lambda (&rest args) (org-mode) (spacemacs/toggle-maximize-buffer)))

;;; Hy-mode

;; ;; Hy-mode development
;; ;; (load-file "~/dev/hy-mode/hy-mode.el")
;; ;; (load-file "~/dev/hy-mode/hy-personal.el")
;; ;; (require 'hy-mode)
;; ;; (require 'hy-personal)

;;; Mail

;; ;; message.el
;; (setq message-directory "~/mail")
;; (setq message-send-mail-function 'smtpmail-send-it)

;; ;; smptmail.el
;; (setq smtpmail-smtp-server "smtp.gmail.com")
;; (setq smtpmail-smtp-service 587)
;; (setq smtpmail-default-smtp-server "smtp.gmail.com")
;; (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
;; (setq smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    "ekaschalk@gmail.com" nil)))

;; ;; mu4e
;; ;; solid
;; (setq mu4e-get-mail-command "offlineimap")
;; (setq mu4e-maildir "~/mail")
;; (setq mu4e-sent-messages-behavior 'delete)
;; (setq user-mail-address "ekaschalk@gmail.com")
;; (setq mu4e-user-mail-address-list (list user-mail-address))

;; ;; experiment
;; (setq mu4e-drafts-folder "/[Gmail].Drafts")
;; (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
;; (setq mu4e-maildir-shortcuts '(("/INBOX"               . ?i)
;;                                ("/[Gmail].Sent Mail"   . ?s)))

;; ;; mu4e-vars.el go through this

;; (setq mu4e-hide-index-messages t)
;; ;; mu4e-use-fancy-chars  ; true by default
;; ;; mu4e-marks            ; all the unicode stuff setup here
;; ;; configure through `mu4e-headers-..-mark' and `mu4e-headers..-prefix'
;; ;; mu4e-enable-async-operations
;; ;; (setq mu4e-update-interval 600)
;; ;; (setq mu4e-index-cleanup nil)      ;; don't do a full cleanup check
;; ;; (setq mu4e-index-lazy-check t)    ;; don't consider up-to-date dir
;; ;; w3m -dump -T text/html

;;; Notate Development

;; (add-to-list 'load-path "~/dev/virtual-indent/")
;; (with-eval-after-load 'hl-todo
;;   (setq hl-todo-keyword-faces
;;         (--remove (s-equals? (car it) "NOTE") hl-todo-keyword-faces)))
;; (require 'nt-dev)
;; (load-file "~/dev/virtual-indent/nt-test.el")

;;; Hy-mode Development

;; (add-to-list 'load-path "~/dev/hy-mode/")
;; (load-file "~/dev/hy-mode/hy-mode.el")
;; (load-file "~/dev/hy-mode/hy-test.el")
;; (spacemacs/set-leader-keys-for-major-mode 'hy-mode
;;   "'" #'hy-shell-start-or-switch-to-shell
;;   "," #'lisp-state-toggle-lisp-state)
;; (spacemacs/set-leader-keys-for-major-mode 'inferior-hy-mode
;;   "," #'lisp-state-toggle-lisp-state)

;; (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
;;   "," #'lisp-state-toggle-lisp-state)
;; (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
;;   "," #'lisp-state-toggle-lisp-state)

;;; Misc

;; (setq find-function-C-source-directory "~/dev/emacs-dev/src/")

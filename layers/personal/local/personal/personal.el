;;; personal.el --- The random config dumping ground -*- lexical-binding: t; -*-

(provide 'personal)

;;; Globals
(global-company-mode)                                     ;; Enable company-mode(autocomplete) globally
(global-unset-key [down-mouse-1])                         ;; No dragging nonsense
(global-set-key [down-mouse-1] 'mouse-select-window)      ;; Select window with mouse click
(smartparens-global-mode)                                 ;; Set global smart parenthesis mode
(treemacs-resize-icons 14)                                ;; Treemacs icon size
(keychain-refresh-environment)                            ;; Refresh keychain information on start

;;;; Keybindings
(global-set-key (kbd "<C-return>") 'newline-below)
(global-set-key (kbd "<S-return>") 'newline-above)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

;; (define-key text-mode-map (kbd "<tab>") 'indent-for-tab-command)  ;; Not quite working

(evil-ex-define-cmd "q[uit]" 'evil-delete-buffer)         ;; Redefine :q to delete buffer instead of exiting emacs

;;;; Variables
(setq display-time-24hr-format t                          ;; Use 24h clock
      layouts-enable-autosave t                           ;; Automatically save layouts
      layouts-autosave-delay 1800                         ;; Save layouts every 30 minutes
      lsp-ui-doc-enable nil                               ;; Disable ui-doc popup. Toggle help with ,hh
      pixel-scroll-mode nil                               ;; Disable pixel scrolling - veeeeeeery slow
      mouse-wheel-follow-mouse t                          ;; Scroll window under mouse
      mac-mouse-wheel-smooth-scroll nil                   ;; Also disable this to prevent sloooow pixel scrolling on mac
      mouse-wheel-progressive-speed nil                   ;; Don't accelerate scrolling
      mouse-wheel-scroll-amount '(1 ((shift) . 1)         ;; Mouse scroll 1 line at a time
                                    ((control) . nil))    ;; Hold ctrl to scroll to top/end of buffer
      scroll-step 1                                       ;; Keyboard scroll 1 line at the time
      scroll-preserve-screen-position t
      scroll-conservatively 100
      user-full-name "Rolf Håvard Blindheim"
      user-email-address "rhblind@gmail.com"
      vc-follow-symlinks nil                              ;; Don't follow symlinks, edit them directly
      ws-butler-global-mode t                             ;; Enable ws-butler globally
      projectile-enable-caching t                         ;; Let projectile cache files
      projectile-project-search-path '("~/workspace")
      projectile-globally-ignored-files '(".DS_Store")
      ;; projectile-globally-ignored-file-suffixes '()
      projectile-globally-ignored-directories '(
                                                ".git"
                                                ".idea"
                                                ".import"
                                                ".elixir_ls"
                                                ".htmlcov"
                                                ".pytest_cache"
                                                "_build"
                                                "__pycache__"
                                                "deps"
                                                "node_modules"
                                                )
      x-mouse-click-focus-ignore-position t               ;; Makes switching windows with mouse work on X-Window system
      )

;;; Org config
(setq org-directory                 "~/Dropbox/org")
(setq org-roam-directory            (concat (file-name-as-directory org-directory) "roam")
      org-roam-v2-ack               t)
(setq org-default-notes-file        (concat (file-name-as-directory org-directory) "misc.org")
      org-work-notes-file           (concat (file-name-as-directory org-directory) "work.org")
      org-projects-file             (concat (file-name-as-directory org-directory) "projects.org")
      org-download-image-dir        (concat (file-name-as-directory org-directory) "images")
      org-roam-index-file           (concat (file-name-as-directory org-roam-directory) "index.org")
      org-agenda-files              (file-expand-wildcards (concat (file-name-as-directory org-directory) "*.org"))
      org-todo-keywords             '((sequence "TODO" "IN PROGRESS"
                                                "|"
                                                "DONE" "NEVERMIND"))
      org-use-property-inheritance  t
      org-log-done-with-time        t
      org-export-in-background      t
      org-catch-invisible-edits     'smart
      org-babel-default-header-args '((:session . "none")
                                      (:results . "replace")
                                      (:exports . "code")
                                      (:cache   . "no")
                                      (:noweb   . "no")
                                      (:hlines  . "no")
                                      (:tangle  . "no")
                                      (:comment . "link")))

;; Make sure org-roam is available on start
(org-roam-db-autosync-mode)

;;; Newsticker
(setq newsticker-dir "~/.emacs.d/.cache/newsticker")

;;; -*- lexical-binding: t -*-

;; Display mofidications for `solarized-light', `doom-gruvbox' and `zenburn' applied here.

;; Try out changes with `spacemacs/update-theme' to see theme updates
;; or alternatively run `spacemacs/cycle-spacemacs-theme' with 'SPC T n'.

;; Theming updates are structured and modularized where possible.

;; Changes of note:
;; 1. All outline/org-level heading styling
;; 2. Comments/strings are italicized
;; 3. Transparent active and monochrome inactive modelines
;; 4. Various small gradient changes to core font-lock-faces

;;; Configuration
;;;; Core
(let* ((variable-tuple
        (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color      (face-foreground 'default nil 'default))
       (headline            `(:inherit default :weight bold :foreground ,base-font-color))
       (variable-pitch      `(:family "Source Code Pro" :height 180 :weight light))
       (fixed-pitch         `(:family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal))))

(setq solarized-use-variable-pitch t)
(setq face-remapping-alist '(;; Headers - outlines match org
                             (outline-1 org-level-1)
                             (outline-2 org-level-2)
                             (outline-3 org-level-3)

                             ;; Modeline - invis. active, monochrome inactive
                             (powerline-active1        mode-line)
                             (powerline-active2        mode-line)
                             (spaceline-highlight-face mode-line)

                             (powerline-active0        mode-line)
                             (mode-line-active         mode-line)
                             (mode-line-inactive       mode-line)
                             (powerline-inactive0      mode-line)
                             (powerline-inactive1      mode-line)
                             (powerline-inactive2      mode-line)
                             ))

;;;; Styling
;;;;; Headers
(setq display/headers/common '(@headline, @variable-tuple :underline nil :inherit nil))
(setq display/headers/zenburn
      `((org-document-title
         ,@display/headers/common
         :height 2)
        (org-level-1
         ,@display/headers/common
         :height 1.75
         :foreground "#DFAF8F")
        (org-level-2
         ,@display/headers/common
         :height 1.5
         :foreground "#BFEBBF")
        (org-level-3
         ,@display/headers/common
         :height 1.25
         :foreground "#7CB8BB")
        (org-level-4
         ,@display/headers/common
         :height 1.1)
        (org-level-5
         ,@display/headers/common)
        (org-level-6
         ,@display/headers/common)
        (org-level-7
         ,@display/headers/common)
        (org-level-8
         ,@display/headers/common)))

(setq display/headers/solarized-light
      `((org-document-title
         ,@display/headers/common
         :height 2)
        (org-level-1
         ,@display/headers/common
         :height 1.75
         :foreground "#a71d31")
        (org-level-2
         ,@display/headers/common
         :height 1.5
         :foreground "#8D6B94")
        (org-level-3
         ,@display/headers/common
         :height 1.25)
        (org-level-4
         ,@display/headers/common
         :height 1.1)
        (org-level-5
         ,@display/headers/common)
        (org-level-6
         ,@display/headers/common)
        (org-level-7
         ,@display/headers/common)
        (org-level-8
         ,@display/headers/common)))

(setq display/headers/doom-gruvbox
      `((org-document-title
         ,@display/headers/common
         :height 2)
        (org-level-1
         ,@display/headers/common
         :height 1.75
         :foreground "#fbf1c7")
        (org-level-2
         ,@display/headers/common
         :height 1.5
         :foreground "#BFEBBF")
        (org-level-3
         ,@display/headers/common
         :height 1.25
         :foreground "#83a598")
        (org-level-4
         ,@display/headers/common
         :height 1.1)
        (org-level-5
         ,@display/headers/common)
        (org-level-6
         ,@display/headers/common)
        (org-level-7
         ,@display/headers/common)
        (org-level-8
         ,@display/headers/common)))

;;;;; Org
(setq display/org-blocks/common '(:inherit fixed-pitch :italic nil :underline nil :box t))
(setq display/org-blocks
      `((org-block-begin-line
         ,@display/org-blocks/common)
        (org-block-end-line
         ,@display/org-blocks/common)
        ))
(setq display/org-code/common                  '(:inherit (shadow fixed-pitch)))
(setq display/org-code                         `((org-code ,@display/org-code/common)))
(setq display/org-document-info/common         '(:foreground "dark orange"))
(setq display/org-document-info                `((org-document-info ,@display/org-document-info/common)))
(setq display/org-document-info-keyword/common '(:inherit (shadow fixed-pitch)))
(setq display/org-document-info-keyword        `((org-document-info-keyword ,@display/org-document-info-keyword/common)))
(setq display/org-indent/common                '(:inherit (org-hide fixed-pitch)))
(setq display/org-indent                       `((org-indent ,@display/org-indent/common)))
(setq display/org-link/common                  '(:foreground "royal blue" :underline t))
(setq display/org-link                         `((org-link ,@display/org-link/common)))
(setq display/org-meta-line/common             '(:inherit (font-lock-comment-face fixed-pitch)))
(setq display/org-meta-line                    `((org-meta-line ,@display/org-meta-line/common)))
(setq display/org-property-value/common        '(:inherit fixed-pitch))
(setq display/org-property-value               `((org-property-value ,@display/org-property-value/common)))
(setq display/org-special-keyword/common       '(:inherit (font-lock-comment-face fixed-pitch)))
(setq display/org-special-keyword              `((org-special-keyword ,@display/org-special-keyword/common)))
(setq display/org-table/common                 '(:inherit fixed-pitch :foreground "#83a598"))
(setq display/org-table                        `((org-table ,@display/org-table/common)))
(setq display/org-tag/common                   '(:inherit (shadow fixed-pitch) :weight bold :height 0.8))
(setq display/org-tag                          `((org-tag ,@display/org-tag/common)))
(setq display/org-verbatim/common              '(:inherit (shadow fixed-pitch)))
(setq display/org-verbatim                     `((org-verbatim ,@display/org-verbatim/common)))

;;;;; Company
(setq display/company/common '(:weight bold :underline nil))
(setq display/company
      `((company-tooltip-common
         ,@display/company/common
         :inherit company-tooltip)
        (company-tooltip-common-selection
         ,@display/company/common
         :inherit company-tooltip-selection)))

;;;;; Mode-line
(setq display/mode-line/common '(:box nil :underline nil))
(setq display/mode-line
      `((mode-line
         ,@display/mode-line/common
         :background nil)
        (mode-line-inactive
         ,@display/mode-line/common)))

;;;;; Font-locks
(setq display/font-locks
      `((font-lock-comment-face
         :italic t
         :weight normal)
        (font-lock-doc-face
         :italic t
         :weight normal)))

;;;;; Git-gutter+
(setq display/git-gutter+
      `((set-face-attribute 'git-gutter+-added :background nil :foreground "green")
        (set-face-attribute 'git-gutter+-deleted :background nil :foreground "red")
        (set-face-attribute 'git-gutter+-modified :background nil :foreground "blue")
        (setq git-gutter+-modified-sign "!")))

;;; Theming
;;;; Common

(setq display/common-theming
      `(,@display/company
        ,@display/mode-line
        ,@display/git-gutter+
        ,@display/org-blocks
        ,@display/org-code
        ,@display/org-document-info
        ,@display/org-document-info-keyword
        ,@display/org-indent
        ,@display/org-link
        ,@display/org-meta-line
        ,@display/org-property-value
        ,@display/org-special-keyword
        ,@display/org-table
        ,@display/org-tag
        ,@display/org-verbatim

        (avy-background-face :italic nil)
        (fringe :background nil)))

;;;; Themes
;;;;; Solarized-Light
(setq display/solarized-light-theming
      `(;; Overwrites
        (mode-line-inactive :background "#eee8d5"
                            ,@(alist-get 'mode-line-inactive
                                         display/mode-line))

        (font-lock-comment-face :foreground "#586e75"
                                ,@(alist-get 'font-lock-comment-face
                                             display/font-locks))
        (font-lock-doc-face :foreground "#2aa198"
                            ,@(alist-get 'font-lock-doc-face
                                         display/font-locks))

        ;; Extra
        (sp-show-pair-match-face :background  "CadetBlue3")
        (auto-dim-other-buffers-face :background "#fcf4df")

        ;; ... Experiments ...
        ))

;;;;; Zenburn
(setq display/zenburn-theming
      `(;; Overwrites
        (font-lock-comment-face :foreground "gray50"
                                ,@(alist-get 'font-lock-comment-face
                                             display/font-locks))
        (font-lock-doc-face :foreground "gray65"
                            ,@(alist-get 'font-lock-doc-face
                                         display/font-locks))

        ;; Extra
        (font-lock-comment-delimiter-face :foreground "gray35")
        (font-lock-function-name-face     :foreground "CadetBlue2")
        (font-lock-type-face              :foreground "LightCoral")
        (auto-dim-other-buffers-face      :background "gray22")

        ;; ... Experiments ...
        ))

;;;;; Doom-Gruvbox
(setq display/doom-gruvbox-theming
      `(;; Overwrites
        ;; (font-lock-comment-face :foreground "gray50"
        ;;                         ,@(alist-get 'font-lock-comment-face
        ;;                                      display/font-locks))
        ;; (font-lock-doc-face :foreground "gray65"
        ;;                     ,@(alist-get 'font-lock-doc-face
        ;;                                  display/font-locks))

        ;; ;; Extra
        ;; (font-lock-comment-delimiter-face :foreground "gray35")
        ;; (font-lock-function-name-face     :foreground "CadetBlue2")
        ;; (font-lock-type-face              :foreground "LightCoral")
        ;; (auto-dim-other-buffers-face      :background "gray22")

        ;; ;; ... Experiments ...
        ))

;;;; Set Modifications
;; This variable is the only `theming' layer requirement to enable our theming

(setq theming-modifications
      `((zenburn         ,@display/common-theming
                         ,@display/headers/zenburn
                         ,@display/zenburn-theming)
        (solarized-light ,@display/common-theming
                         ,@display/headers/solarized-light
                         ,@display/solarized-light-theming)
        (doom-gruvbox    ,@display/common-theming
                         ,@display/headers/doom-gruvbox
                         ,@display/doom-gruvbox-theming)))

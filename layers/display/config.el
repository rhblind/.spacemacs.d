;;; -*- lexical-binding: t -*-

;; Display mofidications for `solarized-light' and `zenburn' applied here.

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
;; Taken from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(let* ((variable-tuple
        (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
              ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color))
       (fixed-pitch        `(:family "Fira Code Retina" :height 160)))

  (setq-local variable-tuple variable-tuple
              fixed-pitch    fixed-pitch
              headline       headline))

(setq solarized-use-variable-pitch nil)
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

;; Support Emojis for MacOS
(when (spacemacs/system-is-mac)
  (set-fontset-font t 'symbol "Apple Color Emoji"))

;;;; Styling
;;;;; Headers
;;;;;; Zenburn
(setq display/headers/common `(,@headline ,@variable-tuple :underline nil :inherit nil))
(setq display/headers/zenburn
      `((org-document-title
         ,@display/headers/common
         :height 2.0)
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

;;;;;; Solarized-Light
(setq display/headers/solarized-light
      `((org-document-title
         ,@display/headers/common
         :height 1.5
         :foreground "#d33682")
        (org-level-1
         ,@display/headers/common
         :height 1.5
         :foreground "#586e75")
        (org-level-2
         ,@display/headers/common
         :height 1.3
         :foreground "#586e75")
        (org-level-3
         ,@display/headers/common
         :height 1.2
         :foreground "#657b83")
        (org-level-4
         ,@display/headers/common
         :height 1.1
         :foreground "#839496")
        (org-level-5
         ,@display/headers/common
         :foreground "#839496")
        (org-level-6
         ,@display/headers/common
         :foreground "#839496")
        (org-level-7
         ,@display/headers/common
         :foreground "#839496")
        (org-level-8
         ,@display/headers/common
         :foreground "#93a1a1")))

;;;;; Org
(setq display/org-code/common                  '(:inherit (shadow fixed-pitch :weight normal)))
(setq display/org-code                         `((org-code ,@display/org-code/common)))
(setq display/org-blocks/common                '(:inherit fixed-pitch :italic nil :underline nil :overline nil :box nil))
(setq display/org-blocks                       `((org-block            ,@display/org-code/common)
                                                 (org-block-begin-line ,@display/org-blocks/common)
                                                 (org-block-end-line   ,@display/org-blocks/common)))
(setq display/org-document-info/common         '(:foreground "#cb4b16"))
(setq display/org-document-info                `((org-document-info ,@display/org-document-info/common)))
(setq display/org-document-info-keyword/common '(:inherit (shadow fixed-pitch)))
(setq display/org-document-info-keyword        `((org-document-info-keyword ,@display/org-document-info-keyword/common)))
(setq display/org-indent/common                '(:inherit (org-hide fixed-pitch)))
(setq display/org-indent                       `((org-indent ,@display/org-indent/common)))
(setq display/org-link/common                  '(:foreground "#0087ff" :underline t))
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
         :inherit company-tooltip-selection))
      )

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

(custom-theme-set-faces
 'user
 `(variable-pitch ((t (,@variable-tuple :height 190 :weight thin))))
 `(fixed-pitch    ((t (,@fixed-pitch)))))

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
        (ahs-plugin-default-face :foreground "#d33682"
                                 ,@(alist-get 'ahs-plugin-default-face
					                                    display/font-locks))
        (ahs-plugin-default-face-unfocused :foreground "#d33682"
                                           ,@(alist-get 'ahs-plugin-default-face-unfocused
					                                              display/font-locks))

        (org-block            :background "#faf1d9" ,@display/org-code/common)
        (org-block-begin-line :background "#f7edd0" ,@display/org-blocks/common)
        (org-block-end-line   :background "#f7edd0" ,@display/org-blocks/common)

        ;; Extra
        (sp-show-pair-match-face :background "CadetBlue3")
        (auto-dim-other-buffers-face :background "#fcf4df")

        ;; ... Experiments ...
        ))

;;;;; Doom Solarized-light
(setq display/doom-solarized-light-theming
      `(;; Overwrites
        (ahs-plugin-default-face :foreground "#d33682"
                                 ,@(alist-get 'ahs-plugin-default-face
					                                    display/font-locks))
        (ahs-plugin-default-face-unfocused :foreground "#d33682"
                                           ,@(alist-get 'ahs-plugin-default-face-unfocused
					                                              display/font-locks))))
;;;;; Doom One
(setq display/doom-one-theming
      `(;; Overwrites
        (ahs-plugin-default-face :foreground "#d33682"
                                 ,@(alist-get 'ahs-plugin-default-face
					                                    display/font-locks))
        (ahs-plugin-default-face-unfocused :foreground "#d33682"
                                           ,@(alist-get 'ahs-plugin-default-face-unfocused
					                                              display/font-locks))
        (ahs-plugin-whole-buffer-face :foreground "White"
                                      :background "#d33682"
                                      ,@(alist-get 'ahs-plugin-whole-buffer-face
                                                   display/font-locks))
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

;;;; Set Modifications
;; This variable is the only `theming' layer requirement to enable our theming

(setq theming-modifications
      `((zenburn         ,@display/common-theming
                         ,@display/headers/zenburn
                         ,@display/zenburn-theming)
        (solarized-light ,@display/common-theming
                         ,@display/headers/solarized-light
                         ,@display/solarized-light-theming)
        (doom-one        ,@display/doom-one-theming)
        (doom-solarized-light ,@display/doom-solarized-light-theming)))

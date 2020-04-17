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

;;;; Styling
;;;;; Headers

(setq display/headers/common '(:underline t :inherit nil))
(setq display/headers/zenburn
      `((org-level-1
         ,@display/headers/common
         :height 1.35
         :foreground "#DFAF8F")
        (org-level-2
         ,@display/headers/common
         :height 1.25
         :foreground "#BFEBBF")
        (org-level-3
         ,@display/headers/common
         :height 1.15
         :foreground "#7CB8BB")))
(setq display/headers/solarized-light
      `((org-level-1
         ,@display/headers/common
         :height 1.35
         :foreground "#a71d31")
        (org-level-2
         ,@display/headers/common
         :height 1.25
         :foreground "#8D6B94")
        (org-level-3
         ,@display/headers/common
         :height 1.15)))
(setq display/headers/doom-gruvbox
      `((org-level-1
         ,@display/headers/common
         :height 1.35
         :foreground "#fbf1c7")
        (org-level-2
         ,@display/headers/common
         :height 1.25
         :foreground "#BFEBBF")
        (org-level-3
         ,@display/headers/common
         :height 1.15
         :foreground "#83a598")))

;;;;; Org-blocks

;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light))))
;;  '(fixed-pitch ((t ( :family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal)))))

;; (custom-theme-set-faces
;;  'user
;;  '(org-block ((t (:inherit fixed-pitch))))
;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
;;  '(org-document-info ((t (:foreground "dark orange"))))
;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
;;  '(org-link ((t (:foreground "royal blue" :underline t))))
;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))


(setq display/org-blocks/common '(:italic nil :underline nil :box t))
(setq display/org-blocks
      `((org-block-begin-line
         ,@display/org-blocks/common)
        (org-block-end-line
         ,@display/org-blocks/common)
        ))

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
        ,@display/org-blocks
        ,@display/git-gutter+

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

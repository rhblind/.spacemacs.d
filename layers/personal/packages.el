;;; Personal Layer -*- lexical-binding: t; -*-

(setq personal-packages
      '((outline-ivy :location local)
        (personal    :location local)))

;;; Outline-ivy

(defun personal/init-outline-ivy ()
  (use-package outline-ivy
    :defer t
    :bind ("C-j" . oi-jump)))

;;; Personal

(defun personal/init-personal ()
  (use-package personal))

;;; Funcs -*- lexical-binding: t; -*-
;;; Unowned Packages
;;;; Eshell

(defun eshell-pop-eshell ()
  "Eshell popup straight to insert mode."
  (interactive)
  (spacemacs/shell-pop-eshell nil)
  (if (string= major-mode "eshell-mode")
      (evil-insert 1)
    (evil-escape)))

;;;; Evil

(defun evil-execute-q-macro ()
  "Execute macro stores in q-register, ie. run `@q'."
  (interactive)
  (evil-execute-macro 1 "@q"))

(defun evil-scroll-to-center-advice (&rest args)
  "Scroll line to center, for advising functions."
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun evil-end-of-line-interactive ()
  "Wrap `evil-end-of-line' in interactive, fix point being 1+ in vis state."
  (interactive)
  (evil-end-of-line))

(defun evil-insert-advice (&rest args)
  "Tack on after eg. heading insertion for `evil-insert' mode."
  (evil-insert 1))

(defun evil-scroll-other-window-interactive ()
  "Wrap `scroll-other-window' in interactive."
  (interactive)
  (scroll-other-window '-))

(defun evil-scroll-other-window-down-interactive ()
  "Wrap `scroll-other-window-down' in interactive."
  (interactive)
  (scroll-other-window))

;;;; Ispell

(defun cycle-ispell-languages ()
  "Cycle between languages"
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))


;;;; Org

(defun org-sort-entries-priorities ()
  "Run `org-sort-entries' for priorities."
  (interactive)
  (org-sort-entries nil ?p))

;;; Owned Packages
;;;; Outshine

(when (configuration-layer/package-used-p 'outshine)
  (defun outshine-fix-narrow-pos (&rest args)
    "Narrowing works within the headline rather than requiring to be on it."
    (unless (outline-on-heading-p t)
      (outline-previous-visible-heading 1)))

  (defun outshine-fix-insert-pos (&rest args)
    "Advise outshine heading insertion newlining to my organization workflow.

Newline insertion now won't match org-mode, will act like block insertion.

If on a heading, insert on new next line.
If not on a heading, insert newline before current-line first."
    (unless (outline-on-heading-p t)
      (forward-line -1)
      (end-of-line) (newline))
    (end-of-line) (newline))

  (defun org-fix-heading-pos (&rest args)
    "Advise org heading insertion, on heading -> no roll-over text after point."
    (when (outline-on-heading-p) (end-of-line)))

  (defun outshine-insert-subheading ()
    "A subheading variation on `outshine-insert-heading'.

Due to a bug with `outline-demote', this function only inserts
the subheading, rather than the heading, correctly when the
subheading level already exists within the buffer."
    (interactive)
    (evil-with-single-undo
      (outshine-insert-heading)
      (set-mark (line-beginning-position)) (goto-char (line-end-position))
      (outline-demote 'region))))

;;; Misc

;;;; Utilities

(defun align-whitespace (start end)
  "Align columns by whitespace
https://www.emacswiki.org/emacs/AlignCommands"
  (interactive "r")
  (align-regexp start end
                "\\(\\s-*\\)\\s-" 1 0 t))

(defun sort-words (reverse start end)
  "Sort words in region alphabetically, in REVERSE if negative.
Prefixed with negative \\[universal-argument], sorts in reverse.
The variable `sort-fold-case' determines whether alphabetic case
affects the sort order.
See `sort-regexp-fields'.
https://www.emacswiki.org/emacs/SortWords"
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\w+" "\\&" start end))

(defun sort-symbols (reverse start end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" start end))

(defun delete-trailing-crlf ()
  "Remove trailing crlf (^M) end-of-line in the current buffer"
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

(defun pyvenv-projectile-workon-venv ()
  "Call pyenv-workon with the current projectile project name.
This will return the full path of the associated virtual
environment found in $WORKON_HOME, or nil if the environment does
not exist."
  (let ((pname (projectile-project-name)))
    (pyvenv-workon pname)
    (if (file-directory-p pyvenv-virtual-env)
        pyvenv-virtual-env
      (pyvenv-deactivate))))

;; (defun pyvenv-auto-lsp ()
;;   "Turn on lsp mode in a Python project with some automated logic.
;; Try to automatically determine which pyenv virtual environment to
;; activate based on the project name, using
;; `pyvenv-projectile-workon-venv'. If successful, call `lsp'. If we
;; cannot determine the virtualenv automatically, first call the
;; interactive `pyvenv-workon' function before `lsp'"
;;   (interactive)
;;   (let ((pvenv (pyvenv-projectile-workon-venv)))
;;     (if pvenv
;;         (lsp)
;;       (progn
;;         (call-interactively #'pyvenv-workon)
;;         (lsp)))))

;;;; Overrides

(defun sp-react--after-equals-p (_id action _context)
  "Allow ES6 arrow '=>' in react-mode"
  (when (memq action '(insert navigate))
    (sp--looking-back-p "=>" 2)))

(defun sp-react--after-equals-skip (ms mb _me)
  (when (eq ms ">")
    (save-excursion
      (goto-char mb)
      (sp--looking-back-p "=" 1))))

;;;; Keybindings

(defun backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""           ;; cursor in begin of buffer
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
            ;; when backword contains space
            (when (and backword
                       (s-contains? " " backword))
              (setq space-pos (1+ (ignore-errors (search-backward-regexp "[[:space:]][[:word:]]"))))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (s-blank? (s-trim substr)))
                        (s-contains? "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      ;; word is non-english word
      (kill-region cp (- cp 1)))
    ))

(defun backward-kill-word-fixed ()
  "Smarter backward-kill-word
https://stackoverflow.com/a/60826269"
  (interactive)
  (if (looking-back "[ \n]")
      ;; delete horizontal space before us and then check to see if we
      ;; are looking at a newline
      (progn (delete-horizontal-space 't)
             (while (looking-back "[ \n]")
               (backward-delete-char 1)))
    ;; otherwise, just do the normal kill word.
    (backward-kill-word 1)))

(defun newline-above()
  "Inserts a new line above the current line"
  (interactive)
  (let ((oldpos (point)))
    (beginning-of-line)
    (open-line 1)
    (indent-according-to-mode)))

(defun newline-below ()
  "Insert an empty line below the current line."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

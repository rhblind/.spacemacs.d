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
      (kill-region cp (- cp 1)))         ;; word is non-english word
    ))
;; (provide 'aborn/backward-kill-word)

(defun newline-above()
  "Inserts a new line above the current line"
  (interactive)
  (let ((oldpos (point)))
    (beginning-of-line)
    (open-line 1)
    (indent-according-to-mode)))
;; (provide 'newline-above)

(defun newline-below ()
  "Insert an empty line below the current line."
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))
;; (provide 'newline-below)

;;;; Timestamp to *Messages* buffer
(defun current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d]" now-ms))))

(defun ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add 'message :before 'sh/ad-timestamp-message)"
   (unless (string-equal FORMAT-STRING "%s%s")
     (let ((deactivate-mark nil)
           (inhibit-read-only t))
       (with-current-buffer "*Messages*"
         (goto-char (point-max))
         (if (not (bolp))
             (newline))
         (insert (current-time-microseconds) " ")))))

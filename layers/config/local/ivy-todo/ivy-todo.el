;;; ivy-todo.el --- Display TODO, FIXME, etc in an Ivy buffer. -*- lexical-binding: t; -*-

(require 'projectile)
(require 'ivy)

;;; Commentary
;; This is a personalized version of todo-ivy (https://github.com/jsmestad/ivy-todo-todo-ivy).

;;; Ivy-todo
;;;; Config

(defvar ivy-todo/ivy-buffer-icons nil
  "If non-nil, show buffer mode icons in `ivy-switch-buffer' and the like.")

(defvar ivy-todo/ivy-task-tags
  '(("NOTE"  . warning)
    ("TODO"  . warning)
    ("FIXME" . error))
  "An list of tags for `ivy-todo/task-list' to search for.")

(defun ivy-todo/ivy--tasks-candidates (tasks)
  "Generate a list of task candidates from TASKS."
  (let* ((max-type-width
          (cl-loop for task in ivy-todo/ivy-task-tags maximize (length (car task))))
         (max-desc-width
          (cl-loop for task in tasks maximize (length (cl-cdadr task))))
         (max-width (max (+ max-desc-width 3)
                         25)))
    (cl-loop
     with fmt = (format "%%-%ds %%-%ds%%s:%%s" max-type-width max-width)
     for alist in tasks
     collect
     (let-alist alist
       (list (format fmt
                     (propertize .type 'face (cdr (assoc .type ivy-todo/ivy-task-tags)))
                     (substring .desc 0 (min max-desc-width (length .desc)))
                     (propertize (abbreviate-file-name .file) 'face 'font-lock-keyword-face)
                     (propertize .line 'face 'font-lock-constant-face))
             .type .file .line)))))

(defun ivy-todo/ivy--tasks (target)
  "Search TARGET for a list of tasks."
  (let* (case-fold-search
         (task-tags (mapcar #'car ivy-todo/ivy-task-tags))
         (cmd
          (format "%s -H -S --no-heading -- %s %s"
                  (or (when-let* ((bin (executable-find "rg")))
                        (concat bin " --line-number"))
                      (when-let* ((bin (executable-find "ag")))
                        (concat bin " --numbers"))
                      (error "Cannot find executables: ripgrep or the_silver_searcher"))
                  (shell-quote-argument
                   (concat "\\s("
                           (string-join task-tags "|")
                           ")([\\s:]|\\([^)]+\\):?)"))
                  target)))
    (save-match-data
      (cl-loop with out = (shell-command-to-string cmd)
               for x in (and out (split-string out "\n" t))
               when (condition-case-unless-debug ex
                        (string-match
                         (concat "^\\([^:]+\\):\\([0-9]+\\):.+\\("
                                 (string-join task-tags "\\|")
                                 "\\):?\\s-*\\(.+\\)")
                         x)
                      (error
                       (message "Error matching task in file: (%s) %s"
                                (error-message-string ex)
                                (car (split-string x ":")))
                       nil))
               collect `((type . ,(match-string 3 x))
                         (desc . ,(match-string 4 x))
                         (file . ,(match-string 1 x))
                         (line . ,(match-string 2 x)))))))


(defun ivy-todo/ivy--tasks-open-action (x)
  "Jump to the file X and line of the current task."
  (cl-destructuring-bind (label type file line) x
    (with-ivy-window
      (find-file (expand-file-name file (projectile-project-root)))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line)))
      (when (search-forward type (line-end-position) t)
        (backward-char (length type)))
      (recenter))))

;;;###autoload
(defun ivy-todo/task-list (&optional arg)
  "Search through all TODO/FIXME tags in the current project. Optional ARG will search only that file."
  (interactive "P")
  (ivy-read (format "Tasks (%s): "
                    (if arg
                        (concat "in: " (file-relative-name buffer-file-name))
                      "project"))
            (ivy-todo/ivy--tasks-candidates
             (ivy-todo/ivy--tasks (if arg buffer-file-name (projectile-project-root))))
            :action #'ivy-todo/ivy--tasks-open-action
            :caller 'ivy-todo/task-list))

(provide 'ivy-todo)

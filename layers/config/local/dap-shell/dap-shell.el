;;; dap-shell.el --- Debug Adapter Protocol shell config for Elixir      -*- lexical-binding: t; -*-

;;; Code:

(require 'dap-mode)

(defun dap-shell--populate-start-file-args (conf)
  "Populate the conf with shell configuration args"
  (-> conf
    (dap--put-if-absent :dap-server-path '("debugger.sh"))
    (dap--put-if-absent :type "shell")
    (dap--put-if-absent :projectDir (lsp-find-session-folder (lsp-session) (buffer-file-name)))
    (dap--put-if-absent :cwd (lsp-find-session-folder (lsp-session) (buffer-file-name))))

  (dap-register-debug-provider "Shell" 'dap-shell--populate-start-file-args))
(provide 'dap-shell)
;;; dap-shell.el ends here

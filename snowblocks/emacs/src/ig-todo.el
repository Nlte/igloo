;;; ig-todo.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(use-package todoist
  :straight t
  :config
  (setq todoist-backing-buffer "~/org/todoist.org"))

(provide 'ig-todo)

;;; ig-todo.el ends here

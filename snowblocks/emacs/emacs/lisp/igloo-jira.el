;;; igloo-jira.el --- Jira config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-jira
  :config
  (make-directory "~/.org-jira")
  (setq jiralib-url "https://bynder.atlassian.net"))


(provide 'igloo-jira)

;;; igloo-jira.el ends here

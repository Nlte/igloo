;;; ig-lang-markdown.el --- summary -*- lexical-binding: t -*-

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(provide 'ig-lang-markdown)

;;; ig-lang-markdown.el ends here

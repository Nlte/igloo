;;; ig-lang-cpp.el --- summary -*- lexical-binding: t -*-

(use-package cmake-mode
  :straight t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

(use-package ccls
  :straight t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(provide 'ig-lang-cpp)

;;; ig-lang-cpp.el ends here

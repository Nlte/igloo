;;; ui.el --- UI configuration -*- lexical-binding: t -*-

(straight-use-package '(nano-theme :type git :host github
                                   :repo "rougier/nano-theme"))

(load-theme 'nano t)

(provide 'ui)
;;; ui.el ends here

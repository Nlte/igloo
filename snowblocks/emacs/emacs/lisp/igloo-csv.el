;;; igloo-csv.el --- CSV files -*- lexical-binding: t -*-
(use-package csv-mode
  :init
  (setq-default
   csv-separators '("," ";" "|")))

(provide 'igloo-csv)
;;; igloo-csv.el ends here

(import (liii check) (liii string) (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

(let ((text (pi-format-file-operations '("/a") '("/b"))))
  (check-true (string-contains? text "<read-files>\n/a\n</read-files>"))
  (check-true (string-contains? text "<modified-files>\n/b\n</modified-files>"))
) ;let

;; 空列表不产生段落
(check (pi-format-file-operations '() '()) => "")

(check-report)

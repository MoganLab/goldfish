(import (liii check) (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; 只读文件 = read - modified，且排序
(let* ((ops '((read "/z" "/b" "/a") (written "/b") (edited "/c")))
       (lists (pi-compute-file-lists ops))
      ) ;
  (check (cdr (assoc 'read-files lists)) => '("/a" "/z"))
  (check (cdr (assoc 'modified-files lists)) => '("/b" "/c"))
) ;let*

(check-report)

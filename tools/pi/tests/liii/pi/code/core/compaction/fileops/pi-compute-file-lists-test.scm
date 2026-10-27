(import (liii check) (liii pi code core compaction fileops))

(check-set-mode! 'report-failed)

;; 只读文件 = read - modified，且排序
(let* ((ops (make-pi-file-ops '("/z" "/b" "/a") '("/b") '("/c")))
       (lists (pi-compute-file-lists ops))
      ) ;
  (check (pi-file-lists-read-files lists) => '("/a" "/z"))
  (check (pi-file-lists-modified-files lists) => '("/b" "/c"))
) ;let*

(check-report)

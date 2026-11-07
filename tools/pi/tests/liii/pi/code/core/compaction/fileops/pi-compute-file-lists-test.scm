;; pi-compute-file-lists
;; 根据文件操作记录汇总读取文件与修改文件列表。
;;
;; 语法
;; ----
;; (pi-compute-file-lists file-ops)
;;
;; 参数
;; ----
;; file-ops : pi-file-ops
;; 文件操作记录。
;;
;; 返回值
;; ----
;; pi-file-lists
;; 文件列表记录。

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

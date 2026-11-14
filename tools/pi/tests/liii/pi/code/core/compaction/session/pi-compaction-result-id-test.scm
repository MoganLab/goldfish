;; pi-compaction-result-id
;; 获取压缩结果记录的条目 ID。
;;
;; 语法
;; ----
;; (pi-compaction-result-id res)
;;
;; 参数
;; ----
;; res : pi-compaction-result?
;; 压缩结果记录。
;;
;; 返回值
;; ----
;; string
;; 压缩条目 ID。

(import (scheme base) (liii check) (liii pi code core compaction session))

(check-set-mode! 'report-failed)

(check (pi-compaction-result-id (make-pi-compaction-result "c-1" "sum" "e-1" 100 1 2 '())
       ) ;pi-compaction-result-id
  =>
  "c-1"
) ;check

(check-report)

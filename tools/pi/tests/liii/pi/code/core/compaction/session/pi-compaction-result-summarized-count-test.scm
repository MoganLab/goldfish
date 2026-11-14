;; pi-compaction-result-summarized-count
;; 获取被摘要的消息条数。
;;
;; 语法
;; ----
;; (pi-compaction-result-summarized-count res)
;;
;; 参数
;; ----
;; res : pi-compaction-result?
;; 压缩结果记录。
;;
;; 返回值
;; ----
;; integer
;; 被摘要的消息条数。

(import (scheme base) (liii check) (liii pi code core compaction session))

(check-set-mode! 'report-failed)

(check (pi-compaction-result-summarized-count (make-pi-compaction-result "c-1" "sum" "e-1" 100 1 2 '())
       ) ;pi-compaction-result-summarized-count
  =>
  1
) ;check

(check-report)

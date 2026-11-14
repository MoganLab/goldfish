;; pi-compaction-result-summary
;; 获取压缩结果记录的摘要文本。
;;
;; 语法
;; ----
;; (pi-compaction-result-summary res)
;;
;; 参数
;; ----
;; res : pi-compaction-result?
;; 压缩结果记录。
;;
;; 返回值
;; ----
;; string
;; 压缩摘要文本。

(import (scheme base) (liii check) (liii pi code core compaction session))

(check-set-mode! 'report-failed)

(check (pi-compaction-result-summary (make-pi-compaction-result "c-1" "sum" "e-1" 100 1 2 '())
       ) ;pi-compaction-result-summary
  =>
  "sum"
) ;check

(check-report)

;; pi-compaction-result-usage
;; 获取压缩过程中的模型用量信息。
;;
;; 语法
;; ----
;; (pi-compaction-result-usage res)
;;
;; 参数
;; ----
;; res : pi-compaction-result?
;; 压缩结果记录。
;;
;; 返回值
;; ----
;; any
;; 模型用量信息。

(import (scheme base) (liii check) (liii pi code core compaction session))

(check-set-mode! 'report-failed)

(check
  (pi-compaction-result-usage (make-pi-compaction-result "c-1" "sum" "e-1" 100 1 2 '((total
                                                                                       . 50)))
  ) ;pi-compaction-result-usage
  =>
  '((total . 50))
) ;check

(check-report)

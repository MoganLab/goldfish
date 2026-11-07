;; PI_COMPACTION_SUMMARY_PREFIX
;; 代码压缩摘要提示词常量。
;;
;; 语法
;; ----
;; PI_COMPACTION_SUMMARY_PREFIX
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string
;; 固定的提示词文本常量。

(import (liii check)
  (liii pi code core compaction prompt)
) ;import

(check-set-mode! 'report-failed)

;; 验证常量为非空字符串
(check-true (string? PI_COMPACTION_SUMMARY_PREFIX))
(check-true (> (string-length PI_COMPACTION_SUMMARY_PREFIX) 0))

(check-report)

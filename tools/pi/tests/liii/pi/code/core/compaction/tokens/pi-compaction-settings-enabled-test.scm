(import (scheme base)
  (liii check)
  (liii pi code core compaction tokens))

(check-set-mode! 'report-failed)

;; pi-compaction-settings-enabled
;; 返回是否启用自动上下文压缩。
;;
;; 语法
;; ----
;; (pi-compaction-settings-enabled settings)
;;
;; 参数
;; ----
;; settings : pi-compaction-settings
;; 自动压缩设置 record。
;;
;; 返回值
;; ----
;; boolean
;; 自动压缩开关值。
;;
;; 示例
;; ----
;; (pi-compaction-settings-enabled settings) => #t

(check-true (pi-compaction-settings-enabled (make-pi-compaction-settings #t 1 1)))
(check-false (pi-compaction-settings-enabled (make-pi-compaction-settings #f 1 1)))

(check-report)

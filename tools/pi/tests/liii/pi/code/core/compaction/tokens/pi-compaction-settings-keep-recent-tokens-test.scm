(import (scheme base)
  (liii check)
  (liii pi code core compaction tokens))

(check-set-mode! 'report-failed)

;; pi-compaction-settings-keep-recent-tokens
;; 返回压缩后保留最近消息的 token 预算。
;;
;; 语法
;; ----
;; (pi-compaction-settings-keep-recent-tokens settings)
;;
;; 参数
;; ----
;; settings : pi-compaction-settings
;; 自动压缩设置 record。
;;
;; 返回值
;; ----
;; integer
;; 最近消息保留预算。
;;
;; 示例
;; ----
;; (pi-compaction-settings-keep-recent-tokens settings) => 20000

(check (pi-compaction-settings-keep-recent-tokens
         (make-pi-compaction-settings #t 16384 20000)) => 20000)

(check-report)

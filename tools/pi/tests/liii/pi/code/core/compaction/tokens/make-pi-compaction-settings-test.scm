(import (scheme base)
  (liii check)
  (liii pi code core compaction tokens))

(check-set-mode! 'report-failed)

;; make-pi-compaction-settings
;; 构造自动上下文压缩设置 record。
;;
;; 语法
;; ----
;; (make-pi-compaction-settings enabled reserve-tokens keep-recent-tokens)
;;
;; 参数
;; ----
;; enabled : boolean
;; 是否启用自动压缩。
;; reserve-tokens : integer
;; 上下文窗口中预留的 token 数量。
;; keep-recent-tokens : integer
;; 压缩时保留的最近消息 token 数量。
;;
;; 返回值
;; ----
;; pi-compaction-settings
;; 自动压缩设置 record。
;;
;; 示例
;; ----
;; (pi-compaction-settings-reserve-tokens
;;   (make-pi-compaction-settings #t 16384 20000)) => 16384

(let ((settings (make-pi-compaction-settings #t 16384 20000)))
  (check-true (pi-compaction-settings? settings))
  (check-true (pi-compaction-settings-enabled settings))
  (check (pi-compaction-settings-reserve-tokens settings) => 16384)
  (check (pi-compaction-settings-keep-recent-tokens settings) => 20000))

(check-report)

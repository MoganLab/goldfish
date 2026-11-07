;; pi-compaction-default-settings
;; 获取默认的压缩配置。
;;
;; 语法
;; ----
;; (pi-compaction-default-settings)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; pi-compaction-settings
;; 默认压缩配置记录。

(import (liii check) (liii pi code core compaction))

(check-set-mode! 'report-failed)

;; 与 pi SETTINGS_DEFAULTS.compaction 一致
(let ((s (pi-compaction-default-settings)))
  (check (pi-compaction-settings-enabled s) => #t)
  (check (pi-compaction-settings-reserve-tokens s) => 16384)
  (check (pi-compaction-settings-keep-recent-tokens s) => 20000)
) ;let

(check-report)

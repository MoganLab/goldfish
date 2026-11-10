;; pi-compaction-settings-reserve-tokens
;; 返回上下文窗口预留 token 数量。
;;
;; 语法
;; ----
;; (pi-compaction-settings-reserve-tokens settings)
;;
;; 参数
;; ----
;; settings : pi-compaction-settings
;; 自动压缩设置 record。
;;
;; 返回值
;; ----
;; integer
;; 触发压缩时从上下文窗口中扣除的预留量。
;;

(import (scheme base) (liii check) (liii pi code core compaction tokens))

(check-set-mode! 'report-failed)


(check (pi-compaction-settings-reserve-tokens (make-pi-compaction-settings #t 16384 20000)
       ) ;pi-compaction-settings-reserve-tokens
  =>
  16384
) ;check

(check-report)

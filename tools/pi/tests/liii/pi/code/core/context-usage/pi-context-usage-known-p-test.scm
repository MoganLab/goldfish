;; pi-context-usage-known?
;; 判断上下文占用 record 的占比是否已知（压缩后尚无新响应时为 #f）。
;;
;; 语法
;; ----
;; (pi-context-usage-known? ctx)
;;
;; 参数
;; ----
;; ctx : pi-context-usage
;; 上下文占用 record。
;;
;; 返回值
;; ----
;; boolean
;; 占比已知返回 #t，未知返回 #f。

(import (scheme base) (liii check) (liii pi code core context-usage))

(check-set-mode! 'report-failed)

;; 1. 占比已知
(check (pi-context-usage-known? (make-pi-context-usage 12.3 1048576 #t)) => #t)

;; 2. 压缩后尚无新响应：占比未知
(check (pi-context-usage-known? (make-pi-context-usage #f 1048576 #f)) => #f)

;; 3. 已知态与百分比 #f 相互独立：百分比缺失但 known? 为 #t 时仍返回 #t
(check (pi-context-usage-known? (make-pi-context-usage #f 1048576 #t)) => #t)

(check-report)

;; pi-ms->timeout-seconds
;; 将正数毫秒转换为秒数，0 或 #f 返回 #f（表示不设置）。
;;
;; 语法
;; ----
;; (pi-ms->timeout-seconds ms)
;;
;; 参数
;; ----
;; ms : number? | #f
;; 毫秒数。
;;
;; 返回值
;; ------
;; real? | #f
;; 对应的秒数，非正数或 #f 返回 #f。

(import (liii check) (liii pi ai retry))

(check-set-mode! 'report-failed)

(check (pi-ms->timeout-seconds 300000) => 300.0)
(check (pi-ms->timeout-seconds 1500) => 1.5)
(check (pi-ms->timeout-seconds 0) => #f)
(check (pi-ms->timeout-seconds -1) => #f)
(check (pi-ms->timeout-seconds #f) => #f)

(check-report)

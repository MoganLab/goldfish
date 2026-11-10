;; pi-context-usage-percent
;; 读取上下文占用 record 中的上下文占比百分比。
;;
;; 语法
;; ----
;; (pi-context-usage-percent ctx)
;;
;; 参数
;; ----
;; ctx : pi-context-usage
;; 上下文占用 record。
;;
;; 返回值
;; ----
;; number | #f
;; 上下文 Token 数占窗口的百分比；占比未知时为 #f。

(import (scheme base) (liii check) (liii pi code core context-usage))

(check-set-mode! 'report-failed)

;; 1. 已知占比：原样返回百分比
(check (pi-context-usage-percent (make-pi-context-usage 12.3 1048576 #t))
  =>
  12.3
) ;check

;; 2. 占比未知：返回 #f（页脚据此显示 "?"）
(check (pi-context-usage-percent (make-pi-context-usage #f 1048576 #f)) => #f)

;; 3. 百分比与窗口相互独立：窗口变化不影响百分比
(check (pi-context-usage-percent (make-pi-context-usage 42.0 200000 #t))
  =>
  42.0
) ;check

(check-report)

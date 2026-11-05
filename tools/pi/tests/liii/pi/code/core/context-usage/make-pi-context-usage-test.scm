;; make-pi-context-usage
;; 创建上下文占用 record（百分比、窗口容量、是否已知）。
;;
;; 语法
;; ----
;; (make-pi-context-usage percent window known?)
;;
;; 参数
;; ----
;; percent : number | #f
;; 上下文 Token 数占窗口的百分比；#f 表示占比未知。
;; window : number | #f
;; 上下文窗口容量。
;; known? : boolean
;; 占比是否已知（压缩后尚无新响应时为 #f）。
;;
;; 返回值
;; ----
;; pi-context-usage
;; 上下文占用 record。

(import (scheme base)
  (liii check)
  (liii pi code core context-usage)
) ;import

(check-set-mode! 'report-failed)

;; 1. 已知占比：三个字段均可读回
(let ((ctx (make-pi-context-usage 12.3 1048576 #t)))
  (check-true (pi-context-usage? ctx))
  (check (pi-context-usage-percent ctx) => 12.3)
  (check (pi-context-usage-window ctx) => 1048576)
  (check (pi-context-usage-known? ctx) => #t))

;; 2. 占比未知（压缩后尚无新响应）：百分比为 #f，窗口容量仍保留
(let ((ctx (make-pi-context-usage #f 1048576 #f)))
  (check-true (pi-context-usage? ctx))
  (check (pi-context-usage-percent ctx) => #f)
  (check (pi-context-usage-window ctx) => 1048576)
  (check (pi-context-usage-known? ctx) => #f))

(check-report)

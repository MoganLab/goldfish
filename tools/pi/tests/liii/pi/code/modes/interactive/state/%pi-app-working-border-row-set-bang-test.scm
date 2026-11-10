;; %pi-app-working-border-row-set!
;; 设置应用实例缓存的 Working 边框行号。
;;
;; 语法
;; ----
;; (%pi-app-working-border-row-set! app row)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; row : exact-integer? 或 #f
;; 新的 Working 边框行号，或 #f 表示无有效 Working 行。
;;
;; 返回值
;; ----
;; unspecified
;; 返回值未指定。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 写入数值后可读回
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-working-border-row-set! app 12)
  (check (pi-app-working-border-row app) => 12)
) ;let

;; 2. 覆盖写入与清空为 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-working-border-row-set! app 5)
  (check (pi-app-working-border-row app) => 5)
  (%pi-app-working-border-row-set! app #f)
  (check (pi-app-working-border-row app) => #f)
) ;let

(check-report)

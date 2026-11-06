;; component-data-set!
;; 设置组件携带的内部私有状态或扩展数据。
;;
;; 语法
;; ----
;; (component-data-set! comp val)
;;
;; 参数
;; ----
;; comp : component?
;; 目标组件。
;;
;; val : any
;; 要存入组件的新数据。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui component))

(check-set-mode! 'report-failed)

(let ((c (make-spacer 1)))
  (component-data-set! c 'custom-state)
  (check (component-data c) => 'custom-state)
) ;let

;; 类型错误
(check-catch 'type-error (component-data-set! "not-a-comp" 123))

(check-report)

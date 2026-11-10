;; component-data
;; 获取组件携带的内部私有状态或扩展数据。
;;
;; 语法
;; ----
;; (component-data comp)
;;
;; 参数
;; ----
;; comp : component?
;; 目标组件。
;;
;; 返回值
;; ----
;; any
;; 组件的内部数据。

(import (scheme base)
  (liii check)
  (liii tui component)
) ;import

(check-set-mode! 'report-failed)

(let ((c (make-text "hello")))
  (check-false (component-data c))
  (component-data-set! c 'custom-state)
  (check (component-data c) => 'custom-state)
) ;let

;; 类型错误
(check-catch 'type-error (component-data "not-a-comp"))

(check-report)

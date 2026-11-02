;; %pi-app-active-selector-set!
;; 设置应用状态字段 %pi-app-active-selector-set!。
;;
;; 语法
;; ----
;; (%pi-app-active-selector-set! app val)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; val : any
;; 新的状态值。
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

(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-active-selector-set! app "test-value")
  (check-true #t)
) ;let

(check-report)

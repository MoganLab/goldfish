;; pi-app-flash-clear!
;; 立即清空应用的瞬态浮层提示（Flash）状态。
;;
;; 语法
;; ----
;; (pi-app-flash-clear! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; -----
;; unspecified
;; 无返回值。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  ;; 未设置 flash 时调用 clear 无副作用
  (pi-app-flash-clear! app)
  (check-false (pi-app-flash-message app))

  ;; 设置长时间 flash 后立即清空
  (pi-app-flash! app "Copied!" 10000)
  (check (pi-app-flash-message app) => "Copied!")
  (pi-app-flash-clear! app)
  (check-false (pi-app-flash-message app))
  ;; 清空后 tick 不再触发重绘
  (check-false (pi-app-flash-tick! app))
) ;let*

(check-report)

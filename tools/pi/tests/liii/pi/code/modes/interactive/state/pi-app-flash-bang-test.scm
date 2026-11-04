;; pi-app-flash!
;; 设置、读取与检测应用瞬态浮层提示（Flash），到期自动清除。
;;
;; 语法
;; ----
;; (pi-app-flash! app message [duration-ms])
;; (pi-app-flash-message app)
;; (pi-app-flash-tick! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; message : string?
;; duration-ms : number?（可选，默认 1000）
;;
;; 返回值
;; -----
;; any

(import (scheme base)
  (liii check)
  (liii time)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  ;; 初始状态无浮层消息
  (check-false (pi-app-flash-message app))
  (check-false (pi-app-flash-tick! app))

  ;; 设置 100ms 短暂浮层
  (pi-app-flash! app "Copied!" 100)
  (check (pi-app-flash-message app) => "Copied!")
  ;; 未到期时 tick 返回 #f
  (check-false (pi-app-flash-tick! app))

  ;; 睡眠等待过期
  (sleep 0.15)
  ;; 已过期：message 返回 #f
  (check-false (pi-app-flash-message app))
  ;; tick 探测到已过期，执行清空并返回 #t（触发重绘）
  (check-true (pi-app-flash-tick! app))
  ;; 再次 tick 返回 #f（已清空，无需重复重绘）
  (check-false (pi-app-flash-tick! app))
) ;let*

(check-report)

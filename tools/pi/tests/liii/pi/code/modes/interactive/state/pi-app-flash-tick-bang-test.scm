;; pi-app-flash-tick!
;; 检测瞬态浮层提示（Flash）是否已过期；过期时清空状态并返回 #t（触发重绘）。
;;
;; 语法
;; ----
;; (pi-app-flash-tick! app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; -----
;; boolean?
;; flash 已过期且被本次调用清空时返回 #t，否则返回 #f。

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
  ;; 未设置 flash 时返回 #f
  (check-false (pi-app-flash-tick! app))

  ;; 设置长时间 flash，未到期时返回 #f
  (pi-app-flash! app "Copied!" 10000)
  (check-false (pi-app-flash-tick! app))

  ;; 设置极短 flash 并等待过期：首次 tick 清空并返回 #t
  (pi-app-flash! app "Gone!" 20)
  (sleep 0.1)
  (check-true (pi-app-flash-tick! app))
  ;; 已清空后再次 tick 返回 #f（避免重复重绘）
  (check-false (pi-app-flash-tick! app))
) ;let*

(check-report)

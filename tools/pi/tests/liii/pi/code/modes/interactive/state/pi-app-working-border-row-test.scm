;; pi-app-working-border-row
;; 获取应用实例缓存的最近一次整帧布局确定的 Working 边框行号。
;;
;; 语法
;; ----
;; (pi-app-working-border-row app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; exact-integer? 或 #f
;; Working 边框行在帧中的下标；尚未整帧渲染或 idle/选择器激活时为 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi agent queue)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始状态：尚未整帧渲染，返回 #f
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (pi-app-working-border-row app) => #f)
) ;let

;; 2. idle 状态整帧渲染：无 Working 边框，返回 #f
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (pi-app-render! app)
  (check (pi-app-working-border-row app) => #f)
) ;let*

;; 3. busy 状态整帧渲染：记录 Working 边框行号
(let* ((vt (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
       (q (pi-app-queue app))
      ) ;
  (pi-queue-set-busy! q)
  (pi-app-render! app)
  (check-true (exact-integer? (pi-app-working-border-row app)))
  (check-true (>= (pi-app-working-border-row app) 0))
) ;let*

(check-report)

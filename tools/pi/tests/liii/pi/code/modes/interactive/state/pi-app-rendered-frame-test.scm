;; pi-app-rendered-frame
;; 获取应用实例缓存的最近一次整帧渲染结果。
;;
;; 语法
;; ----
;; (pi-app-rendered-frame app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ----
;; pair? 或 #f
;; 缓存的 (帧行列表 . 光标位置)；尚未渲染过时为 #f。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive state)
) ;import

(check-set-mode! 'report-failed)

;; 1. 初始状态：尚无渲染缓存
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (check (pi-app-rendered-frame app) => #f)
) ;let

;; 2. 整帧渲染后写入缓存（帧行与光标位置）
(let* ((vt (make-virtual-terminal :width 20 :height 5))
       (app (make-pi-app :terminal vt :handler (lambda (a m) #t)))
      ) ;
  (pi-app-render! app)
  (let ((cached (pi-app-rendered-frame app)))
    (check-true (pair? cached))
    (check-true (list? (car cached)))
    (check-true (pair? (cdr cached)))
  ) ;let
) ;let*

(check-report)

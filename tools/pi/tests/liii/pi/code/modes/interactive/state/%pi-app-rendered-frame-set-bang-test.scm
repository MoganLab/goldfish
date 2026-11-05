;; %pi-app-rendered-frame-set!
;; 设置应用实例缓存的整帧渲染结果（帧行列表与光标位置）。
;;
;; 语法
;; ----
;; (%pi-app-rendered-frame-set! app cached)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; cached : pair?
;; 新的缓存值，形如 (帧行列表 . 光标位置)。
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

;; 1. 写入后可读回
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-rendered-frame-set! app (cons '("line1" "line2") '(0 . 3)))
  (check (pi-app-rendered-frame app) => (cons '("line1" "line2") '(0 . 3)))
) ;let

;; 2. 写入 #f 表示清空缓存
(let ((app (make-pi-app :terminal (make-virtual-terminal :width 80 :height 24))))
  (%pi-app-rendered-frame-set! app (cons '("line1") '(0 . 0)))
  (check-true (pair? (pi-app-rendered-frame app)))
  (%pi-app-rendered-frame-set! app #f)
  (check (pi-app-rendered-frame app) => #f)
) ;let

(check-report)

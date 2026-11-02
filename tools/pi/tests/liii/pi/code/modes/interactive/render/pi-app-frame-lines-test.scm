;; pi-app-frame-lines
;; 计算应用当前帧需要渲染的完整行列表以及终端光标位置。
;;
;; 语法
;; ----
;; (pi-app-frame-lines app)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;;
;; 返回值
;; ------
;; (values list pair)
;; 返回两个值：当前帧的文本行列表，以及光标坐标 (cons x y)。

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
) ;import

(check-set-mode! 'report-failed)

;; 1. 24 行终端下帧总行数为 24，光标坐标合法
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (app (make-pi-app :terminal vterm))
      ) ;
  (let-values (((lines cur) (pi-app-frame-lines app)))
    (check (length lines) => 24)
    (check-true (pair? cur))
    (check-true (integer? (car cur)))
    (check-true (integer? (cdr cur)))
  ) ;let-values
) ;let*

(check-report)

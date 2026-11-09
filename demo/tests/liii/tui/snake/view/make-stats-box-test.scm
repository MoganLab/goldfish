;; make-stats-box
;; 构造分数/长度/状态侧栏组件
;;
;; 语法
;; ----
;; (make-stats-box game spinner)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态；spinner : spinner?\n心跳指示器
;;
;; 返回值
;; ------
;; component?\n带 STATS 标题的边框侧栏组件

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui loader)
  (liii tui snake logic)
  (liii tui snake view)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

;; 1. 无色模式：可渲染且内容非空
(with-color-mode #f
  (lambda ()
    (let* ((game (make-snake-game :score 30 :high-score 50))
           (spinner (make-spinner))
           (stats-box (make-stats-box game spinner))
           (stats-lines (component-render stats-box 25))
          ) ;
      (check (component? stats-box) => #t)
      (check (pair? stats-lines) => #t)
      (check (> (length stats-lines) 0) => #t)
    ) ;let*
  ) ;lambda
) ;with-color-mode

;; 2. 带颜色模式：仍为合法组件
(with-color-mode #t
  (lambda ()
    (let* ((game (make-snake-game :score 30 :high-score 50))
           (stats-box (make-stats-box game (make-spinner)))
          ) ;
      (check (component? stats-box) => #t)
    ) ;let*
  ) ;lambda
) ;with-color-mode

(check-report)

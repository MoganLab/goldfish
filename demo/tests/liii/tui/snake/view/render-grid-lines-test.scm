;; render-grid-lines
;; 把游戏状态渲染为网格行列表（每字符一格）
;;
;; 语法
;; ----
;; (render-grid-lines game)
;;
;; 参数
;; ----
;; game : snake-game?\n当前游戏状态
;;
;; 返回值
;; ------
;; list?\n每行为 string? 的行列表，行数为棋盘高度

(import (scheme base)
  (liii check)
  (liii tui text-width)
  (liii tui snake logic)
  (liii tui snake view)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

;; 1. 无色模式：网格字符与游戏状态严格对应
(with-color-mode #f
  (lambda ()
    (let* ((game (make-snake-game :width 10 :height 6 :snake '((3 2) (2 2) (1 2)) :food '(5 2)))
           (lines (render-grid-lines game))
          ) ;
      (check (length lines) => 6)
      (let ((row2 (list-ref lines 2)))
        (check (string-length row2) => 10)
        (check (string-ref row2 3) => #\@)
        (check (string-ref row2 2) => #\o)
        (check (string-ref row2 1) => #\o)
        (check (string-ref row2 5) => #\*)
      ) ;let
    ) ;let*
  ) ;lambda
) ;with-color-mode

;; 2. 长蛇拐弯与 S 形走位
(with-color-mode #f
  (lambda ()
    (let* ((coords '((5 3) (5 4) (5 5) (4 5) (3 5) (2 5) (2 6) (2 7)))
           (game (make-snake-game :width 20 :height 10 :snake coords :food '(15 8)))
           (grid (render-grid-lines game))
          ) ;
      (check (string-ref (list-ref grid (cadar coords)) (caar coords)) => #\@)
      (for-each
        (lambda (seg)
          (check (string-ref (list-ref grid (cadr seg)) (car seg)) => #\o)
        ) ;lambda
        (cdr coords)
      ) ;for-each
    ) ;let*
  ) ;lambda
) ;with-color-mode

;; 3. 带颜色模式：包含 SGR 且 strip-ansi 后与无色版一致
(with-color-mode #t
  (lambda ()
    (let* ((game (make-snake-game :width 10 :height 6 :snake '((3 2) (2 2) (1 2)) :food '(5 2)))
           (lines (render-grid-lines game))
           (row2 (list-ref lines 2))
          ) ;
      (check (length lines) => 6)
      (check (display-width row2) => 10)
      (check (string=? (strip-ansi row2) " oo@ *    ") => #t)
      (check (> (string-length row2) 10) => #t)
    ) ;let*
  ) ;lambda
) ;with-color-mode

(check-report)

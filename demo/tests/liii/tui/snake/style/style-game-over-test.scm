;; style-game-over
;; 游戏结束文本染色（粗体红色）
;;
;; 语法
;; ----
;; (style-game-over s)
;;
;; 参数
;; ----
;; s : string?\n待染色文本
;;
;; 返回值
;; ------
;; string?\n颜色开启时为 ANSI 着色文本，关闭时原样返回

(import (scheme base)
  (liii check)
  (liii tui text-width)
  (liii tui snake style)
) ;import

(check-set-mode! 'report-failed)

(with-color-mode #f (lambda () (check (style-game-over "GAME OVER") => "GAME OVER")))

(with-color-mode #t
  (lambda ()
    (let ((s (style-game-over "GAME OVER")))
      (check (display-width s) => 9)
      (check (string=? (strip-ansi s) "GAME OVER") => #t)
    ) ;let
  ) ;lambda
) ;with-color-mode

(check-catch 'type-error (style-game-over 'over))

(check-report)

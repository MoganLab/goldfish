;; style-dim
;; 弱化提示文本染色（暗灰）
;;
;; 语法
;; ----
;; (style-dim s)
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

(with-color-mode #f (lambda () (check (style-dim "Arrows: Move") => "Arrows: Move")))

(with-color-mode #t
  (lambda ()
    (let ((s (style-dim "Arrows: Move")))
      (check (display-width s) => 12)
      (check (string=? (strip-ansi s) "Arrows: Move") => #t)
    ) ;let
  ) ;lambda
) ;with-color-mode

(check-catch 'type-error (style-dim 'dim))

(check-report)

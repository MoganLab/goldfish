;; style-title
;; 标题文本染色（粗体青色）
;;
;; 语法
;; ----
;; (style-title s)
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

(with-color-mode #f (lambda () (check (style-title " STATS ") => " STATS ")))

(with-color-mode #t
  (lambda ()
    (let ((s (style-title " STATS ")))
      (check (display-width s) => 7)
      (check (string=? (strip-ansi s) " STATS ") => #t)
    ) ;let
  ) ;lambda
) ;with-color-mode

(check-catch 'type-error (style-title '(1 2)))

(check-report)

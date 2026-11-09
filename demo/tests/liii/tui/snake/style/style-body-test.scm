;; style-body
;; 蛇身字符染色（绿色）
;;
;; 语法
;; ----
;; (style-body s)
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

(with-color-mode #f (lambda () (check (style-body "o") => "o")))

(with-color-mode #t
  (lambda ()
    (let ((s (style-body "o")))
      (check (string? s) => #t)
      (check (> (string-length s) 1) => #t)
      (check (display-width s) => 1)
      (check (string=? (strip-ansi s) "o") => #t)
    ) ;let
  ) ;lambda
) ;with-color-mode

(check-catch 'type-error (style-body #f))

(check-report)

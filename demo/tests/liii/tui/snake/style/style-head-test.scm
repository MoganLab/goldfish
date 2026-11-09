;; style-head
;; 蛇头字符染色（粗体绿色）
;;
;; 语法
;; ----
;; (style-head s)
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

;; 关色模式：原样返回
(with-color-mode #f (lambda () (check (style-head "@") => "@")))

;; 开色模式：包含 SGR 码且显示宽度不变
(with-color-mode #t
  (lambda ()
    (let ((s (style-head "@")))
      (check (string? s) => #t)
      (check (> (string-length s) 1) => #t)
      (check (display-width s) => 1)
      (check (string=? (strip-ansi s) "@") => #t)
    ) ;let
  ) ;lambda
) ;with-color-mode

;; 类型错误
(check-catch 'type-error (style-head 123))

(check-report)

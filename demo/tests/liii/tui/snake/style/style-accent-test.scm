;; style-accent
;; 强调文本染色（粗体黄色）
;;
;; 语法
;; ----
;; (style-accent s)
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

(with-color-mode #f (lambda () (check (style-accent "100") => "100")))

(with-color-mode #t
  (lambda ()
    (let ((s (style-accent "100")))
      (check (display-width s) => 3)
      (check (string=? (strip-ansi s) "100") => #t)
    ) ;let
  ) ;lambda
) ;with-color-mode

(check-catch 'type-error (style-accent 42))

(check-report)

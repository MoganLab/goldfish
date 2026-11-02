;; apply-card-bg
;; 为文本行应用背景色与前景色，并按宽度补齐空格。
;;
;; 语法
;; ----
;; (apply-card-bg line width bg-code fg-code)
;;
;; 参数
;; ----
;; line : string?
;; 待渲染的单行内容。
;; width : integer?
;; 目标总列宽。
;; bg-code : string?
;; 背景色 ANSI 控制码。
;; fg-code : string?
;; 前景色 ANSI 控制码。
;;
;; 返回值
;; ----
;; string?
;; 包裹了颜色控制码并补齐宽度的渲染行。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui text-width)
  (liii pi code modes interactive views theme)
) ;import

(check-set-mode! 'report-failed)

(let* ((bg "\x1b;[48;2;10;20;30m")
       (fg "\x1b;[38;2;40;50;60m")
       (res (apply-card-bg "hello" 10 bg fg))
      ) ;
  (check-true (string-contains? res bg))
  (check-true (string-contains? res fg))
  (check (display-width (strip-ansi res)) => 10)
) ;let*

(check-report)

;; border-style-br
;; 获取边框风格的右下角字符。
;;
;; 语法
;; ----
;; (border-style-br b)
;;
;; 参数
;; ----
;; b : border-style?
;; 边框风格实例。
;;
;; 返回值
;; ----
;; string?
;; 右下角字符。

(import (scheme base)
  (liii check)
  (liii tui box border)
) ;import

(check-set-mode! 'report-failed)

(let ((b (border-style-resolve 'single)))
  (check (border-style-br b) => "┘")
) ;let

(check-report)

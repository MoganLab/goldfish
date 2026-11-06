;; border-style-v
;; 获取边框风格的垂直线字符。
;;
;; 语法
;; ----
;; (border-style-v b)
;;
;; 参数
;; ----
;; b : border-style?
;; 边框风格实例。
;;
;; 返回值
;; ----
;; string?
;; 垂直线字符。

(import (scheme base)
  (liii check)
  (liii tui box border)
) ;import

(check-set-mode! 'report-failed)

(let ((b (border-style-resolve 'single)))
  (check (border-style-v b) => "│")
) ;let

(check-report)

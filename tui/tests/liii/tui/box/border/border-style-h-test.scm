;; border-style-h
;; 获取边框风格的水平线字符。
;;
;; 语法
;; ----
;; (border-style-h b)
;;
;; 参数
;; ----
;; b : border-style?
;; 边框风格实例。
;;
;; 返回值
;; ----
;; string?
;; 水平线字符。

(import (scheme base)
  (liii check)
  (liii tui box border)
) ;import

(check-set-mode! 'report-failed)

(let ((b (border-style-resolve 'single)))
  (check (border-style-h b) => "─")
) ;let

(check-report)

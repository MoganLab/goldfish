;; border-style-name
;; 获取边框风格的名称标识。
;;
;; 语法
;; ----
;; (border-style-name b)
;;
;; 参数
;; ----
;; b : border-style?
;; 边框风格实例。
;;
;; 返回值
;; ----
;; symbol?
;; 边框风格名称。

(import (scheme base)
  (liii check)
  (liii tui box border)
) ;import

(check-set-mode! 'report-failed)

(let ((b (border-style-resolve 'single)))
  (check (border-style-name b) => 'single)
) ;let

(check-report)

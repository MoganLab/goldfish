;; pi-resolve-path
;; 解析工具参数中的路径为绝对路径
;;
;; 语法
;; ----
;; (pi-resolve-path ...)
;;
;; 参数
;; ----
;; 见各断言场景：本测试按用例组织，覆盖典型入参。
;;
;; 返回值
;; ------
;; 见各断言场景：按被测函数实际返回值断言。

(import (liii check) (liii path) (liii pi code core tools read))

(check-set-mode! 'report-failed)

;; pi-resolve-path
;; 将相对路径或带 ~ 的路径解析为规范的绝对路径。
(let ((p (pi-resolve-path "tools/pi/tests/fixtures/sample.txt" "/tmp")))
  (check-true (string? p))
  (check-true (path-absolute? (path p)))
) ;let

(check-report)

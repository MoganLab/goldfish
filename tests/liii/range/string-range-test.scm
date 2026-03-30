(import (liii check)
        (srfi srfi-196)
) ;import

(check-set-mode! 'report-failed)

;; string-range
;; 从字符串创建 range。
;;
;; 语法
;; ----
;; (string-range s)
;;
;; 参数
;; ----
;; s : string
;; 源字符串。
;;
;; 返回值
;; ----
;; range
;; 包含字符串字符的 range 对象。
;;
;; 注意
;; ----
;; 将字符串转换为字符向量后再创建 range。
;;
;; 示例
;; ----
;; (string-range "hello") => 包含 #\h,#\e,#\l,#\l,#\o 的 range
;;
;; 错误处理
;; ----
;; 无

(let ((r (string-range "hello")))
  (check (range-length r) => 5)
  (check (range-ref r 0) => #\h)
  (check (range-ref r 4) => #\o)
) ;let

(check-report)

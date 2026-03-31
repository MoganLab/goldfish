(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

;; range->string
;; 将字符 range 转换为字符串。
;;
;; 语法
;; ----
;; (range->string r)
;;
;; 参数
;; ----
;; r : range
;; 字符 range 对象。
;;
;; 返回值
;; ----
;; string
;; 包含 range 所有字符的字符串。
;;
;; 示例
;; ----
;; (range->string (string-range "hello")) => "hello"
;;
;; 错误处理
;; ----
;; 无

(let ((r (string-range "hello")))
  (check (range->string r) => "hello")
) ;let

(let ((r (string-range "")))
  (check (range->string r) => "")
) ;let

(let ((r (string-range "abc")))
  (check (range->string r) => "abc")
) ;let

(check-report)

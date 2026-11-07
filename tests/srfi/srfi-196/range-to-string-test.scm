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
;; 错误处理
;; ----
;; - type-error：当 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (string-range "hello")))
  (check (range->string r) => "hello")
) ;let


(let ((r (string-range "")))
  (check (range->string r) => "")
) ;let


(let ((r (string-range "abc")))
  (check (range->string r) => "abc")
) ;let



;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range->string 42))
(check-catch 'type-error (range->string "not-a-range"))
(check-catch 'type-error (range->string '(0 1 2)))

(check-report)

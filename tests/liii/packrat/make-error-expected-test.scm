(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; make-error-expected
;; 构造期望类型的解析错误对象。
;;
;; 语法
;; ----
;; (make-error-expected position expected)
;;
;; 参数
;; ----
;; position : parse-position
;;   解析位置
;; expected : any
;;   期望的对象
;;
;; 返回值
;; ----
;; parse-error
;;   表示期望错误的解析错误对象
;;
;; 描述
;; ----
;; 当解析器期望某个特定 token 但实际不匹配时，
;; 使用此函数构造错误对象，与 make-expected-result 不同
;; 的是它返回的是 error 类型而非 result 类型。

(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (define error-ex (make-error-expected pos "open-paren"))
  (check-true (parse-error? error-ex))
) ;let

(let ()
  (define pos (make-parse-position "expr.gf" 5 3))
  (define error2 (make-error-expected pos 'identifier))
  (check-true (parse-error? error2))
) ;let

(let ()
  (define pos (make-parse-position #f 1 0))
  (define error3 (make-error-expected pos "num"))
  (check-true (parse-error? error3))
) ;let

(check-report)

(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; make-expected-result
;; 构造表示期望失败的 parse-result 对象。
;;
;; 语法
;; ----
;; (make-expected-result position expected)
;;
;; 参数
;; ----
;; position : parse-position
;;   解析位置
;; expected : any
;;   期望的对象（如 token 类型）
;;
;; 返回值
;; ----
;; parse-result
;;   表示失败的解析结果
;;
;; 描述
;; ----
;; 当解析器期望某个特定 token 但实际不匹配时，
;; 使用此函数构造失败结果。

(let ()
  (define fail (make-expected-result (make-parse-position #f 1 0) "num"))
  (check-false (parse-result-successful? fail))
  (check-true (parse-result? fail))
) ;let

(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (define fail2 (make-expected-result pos "open-paren"))
  (check-false (parse-result-successful? fail2))
) ;let

(let ()
  (define pos (make-parse-position "expr.scm" 5 3))
  (define fail3 (make-expected-result pos 'identifier))
  (check-true (parse-result? fail3))
) ;let

(check-report)

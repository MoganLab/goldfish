(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-result-successful?
;; 判断解析结果是否表示成功解析。
;;
;; 语法
;; ----
;; (parse-result-successful? result)
;;
;; 参数
;; ----
;; result : parse-result
;;   待判断的 parse-result 对象
;;
;; 返回值
;; ----
;; bool
;;   若为成功解析则返回 #t，否则返回 #f
;;
;; 描述
;; ----
;; 成功解析是指通过 make-result 构造的结果，失败解析包括
;; make-expected-result 和 make-message-result 构造的结果。

(let ()
  (define success (make-result 42 #f))
  (check-true (parse-result-successful? success))
) ;let

(let ()
  (define fail (make-expected-result (make-parse-position #f 1 0) "num"))
  (check-false (parse-result-successful? fail))
) ;let

(let ()
  (define pos (make-parse-position "test.scm" 1 5))
  (define msg-fail (make-message-result pos "syntax error"))
  (check-false (parse-result-successful? msg-fail))
) ;let

(check-report)

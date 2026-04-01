(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; make-result
;; 构造表示成功解析的 parse-result 对象。
;;
;; 语法
;; ----
;; (make-result semantic-value next-parse-results)
;;
;; 参数
;; ----
;; semantic-value : any
;;   作为语义值使用的对象
;; next-parse-results : parse-results or #f
;;   表示继续解析位置的 parse-results 对象，或 #f 表示输入结束
;;
;; 返回值
;; ----
;; parse-result
;;   表示成功的解析结果
;;
;; 描述
;; ----
;; 构造一个成功的解析结果，包含语义值和后续解析位置信息。
;; 这是 packrat 解析器的基础构造函数。

(let ()
  (define success (make-result 42 #f))
  (check-true (parse-result? success))
  (check (parse-result-semantic-value success) => 42)
  (check (parse-result-next success) => #f)
) ;let

(let ()
  (define pos (make-parse-position "test.scm" 1 0))
  (define next-results (base-generator->results (lambda () (values pos '(num . 1)))))
  (define result (make-result "hello" next-results))
  (check (parse-result-semantic-value result) => "hello")
  (check-true (parse-results? (parse-result-next result)))
) ;let

(check-report)

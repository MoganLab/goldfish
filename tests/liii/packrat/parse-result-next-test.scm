(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-result-next
;; 获取解析后剩余输入流的位置信息。
;;
;; 语法
;; ----
;; (parse-result-next result)
;;
;; 参数
;; ----
;; result : parse-result
;;   待查询的 parse-result 对象
;;
;; 返回值
;; ----
;; parse-results or #f
;;   后续输入流的 parse-results 对象，或 #f 表示输入结束
;;
;; 描述
;; ----
;; 返回解析后的下一个位置，用于继续后续解析。
;; #f 表示已经到达输入流的末尾。

(let ()
  (define success (make-result 42 #f))
  (check (parse-result-next success) => #f)
) ;let

(let ()
  (define pos (make-parse-position "test.scm" 1 0))
  (define gen (lambda () (values pos '(num . 100))))
  (define next-results (base-generator->results gen))
  (define result (make-result "first" next-results))
  (check-true (parse-results? (parse-result-next result)))
  (check (parse-results-token-value (parse-result-next result)) => 100)
) ;let

(check-report)

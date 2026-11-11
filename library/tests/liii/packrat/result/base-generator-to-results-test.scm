;; base-generator->results
;; 把取词生成器转换为解析结果流。
;;
;; 语法
;; ----
;; (base-generator->results generator)
;;
;; 参数
;; ----
;; generator : procedure
;; 无参过程，每次返回 (values 位置 记号)；
;; 输入结束时返回 (values 位置 #f)。
;;
;; 返回值
;; ----
;; <parse-results>
;; 解析结果流记录。

(import (liii check) (liii packrat result) (liii packrat error) (liii packrat position))

(check-set-mode! 'report-failed)

(let* ((gen (lambda () (values (make-parse-position "test" 1 0) #f)))
       (results (base-generator->results gen))
      ) ;
  (check-true (parse-results? results))
  (check-false (parse-results-base results))
) ;let*

;; 有记号时 base 为 (记号类型 . 记号值)
(let ()
  (define gen
    (let ((tokens '((num . 100) (id . x))))
      (lambda ()
        (if (null? tokens)
          (values #f #f)
          (let ((token (car tokens)))
            (set! tokens (cdr tokens))
            (values #f token)
          ) ;let
        ) ;if
      ) ;lambda
    ) ;let
  ) ;define
  (define results (base-generator->results gen))
  (check (parse-results-token-kind results) => 'num)
  (check (parse-results-token-value results) => 100)
) ;let

(check-report)

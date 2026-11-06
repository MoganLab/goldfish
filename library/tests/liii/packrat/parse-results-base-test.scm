;; parse-results-base
;; 读取解析结果流的当前基础记号。
;;
;; 语法
;; ----
;; (parse-results-base results)
;;
;; 参数
;; ----
;; results : <parse-results>
;; 解析结果流记录。
;;
;; 返回值
;; ----
;; pair 或 #f
;; 形如 (记号类型 . 记号值) 的基础记号；没有当前记号时为 #f。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

(let ()
  (define gen
    (let ((tokens '((num . 100))))
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
  (check (parse-results-base results) => '(num . 100))
) ;let

;; 没有记号时为 #f
(let* ((gen (lambda () (values (make-parse-position "test" 1 0) #f)))
       (results (base-generator->results gen))
      ) ;
  (check-false (parse-results-base results))
) ;let*

(check-report)

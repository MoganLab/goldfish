;; parse-results-next
;; 读取解析结果流的后续状态。
;;
;; 语法
;; ----
;; (parse-results-next results)
;;
;; 参数
;; ----
;; results : <parse-results>
;; 解析结果流记录。
;;
;; 返回值
;; ----
;; <parse-results> 或 #f
;; 后续解析结果流；输入结束时为 #f。
;;
;; 说明
;; ----
;; 后续状态以零参过程保存，首次读取时求值并缓存。

(import (liii check) (liii packrat))

(check-set-mode! 'report-failed)

;; 输入结束：没有后续状态
(let* ((gen (lambda () (values (make-parse-position "test" 1 0) #f)))
       (results (base-generator->results gen))
      ) ;
  (check-false (parse-results-next results))
) ;let*

;; 有记号：后续状态是新的 parse-results
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
  (check-true (parse-results? (parse-results-next results)))
) ;let

(check-report)

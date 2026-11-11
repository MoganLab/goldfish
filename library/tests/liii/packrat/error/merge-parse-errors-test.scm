;; merge-parse-errors
;; 合并两个解析错误。
;;
;; 语法
;; ----
;; (merge-parse-errors e1 e2)
;;
;; 参数
;; ----
;; e1 : <parse-error> 或 #f
;; 左侧错误。
;; e2 : <parse-error> 或 #f
;; 右侧错误。
;;
;; 返回值
;; ----
;; <parse-error> 或 #f
;; 合并后的错误记录。
;;
;; 说明
;; ----
;; 1. 位置更靠后的错误优先；位置相同时合并期望记号与消息集合。
;; 2. 空错误不会覆盖非空错误。

(import (liii check) (liii packrat error) (liii packrat position))

(check-set-mode! 'report-failed)

(let ((pos (make-parse-position "test.scm" 1 0)))
  ;; 空错误不覆盖非空错误
  (check (merge-parse-errors (make-error-expected pos "num") #f)
    =>
    (make-error-expected pos "num")
  ) ;check
  (check (merge-parse-errors #f (make-error-expected pos "num"))
    =>
    (make-error-expected pos "num")
  ) ;check
  ;; 位置靠后的错误优先
  (check (merge-parse-errors (make-error-expected pos "num")
           (make-error-expected (make-parse-position "test.scm" 5 0) "id")
         ) ;merge-parse-errors
    =>
    (make-error-expected (make-parse-position "test.scm" 5 0) "id")
  ) ;check
  ;; 位置相同时合并期望集合
  (let ((merged (merge-parse-errors (make-error-expected pos "num")
                  (make-error-expected pos "id")
                ) ;merge-parse-errors
        ) ;merged
       ) ;
    (check (pair? (member "num" (parse-error-expected merged))) => #t)
    (check (pair? (member "id" (parse-error-expected merged))) => #t)
  ) ;let
) ;let

(check-report)

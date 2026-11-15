;; test-error
;; 断言表达式求值过程中抛出异常。
;;
;; 语法
;; ----
;; (test-error expression)
;; (test-error error-type expression)
;; (test-error test-name error-type expression)
;;
;; 参数
;; ----
;; test-name : string? (可选)
;; 测试用例名称。
;; error-type : any (可选)
;; 期望异常类型或谓词，#t 表示任意异常。
;; expression : any
;; 待求值表达式。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base) (srfi srfi-64))

(define (%run-tests thunk)
  (let ((r (test-runner-null)))
    (test-runner-on-final! r
      (lambda (runner)
        (when (or (> (test-runner-fail-count runner) 0)
                (> (test-runner-xpass-count runner) 0)
              ) ;or
          (error "SRFI-64 test failed")
        ) ;when
      ) ;lambda
    ) ;test-runner-on-final!
    (test-with-runner r (thunk))
  ) ;let
) ;define

(%run-tests (lambda ()
              (test-begin "err")
              (test-error (vector-ref '#(1 2) 5))
              (test-error "named" #t (error "boom"))
              (test-end "err")
            ) ;lambda
) ;%run-tests

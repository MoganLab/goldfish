;; test-eqv
;; 使用 eqv? 断言两个表达式等价。
;;
;; 语法
;; ----
;; (test-eqv expected expression)
;; (test-eqv test-name expected expression)
;;
;; 参数
;; ----
;; test-name : string? (可选)
;; 测试用例名称。
;; expected : any
;; 期望值。
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
              (test-begin "eqv")
              (test-eqv 42 (+ 40 2))
              (test-eqv "named" 'a 'a)
              (test-end "eqv")
            ) ;lambda
) ;%run-tests

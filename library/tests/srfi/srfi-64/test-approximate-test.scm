;; test-approximate
;; 断言数值在给定误差范围内近似相等。
;;
;; 语法
;; ----
;; (test-approximate expected expression error)
;; (test-approximate test-name expected expression error)
;;
;; 参数
;; ----
;; test-name : string? (可选)
;; 测试用例名称。
;; expected : number?
;; 期望数值。
;; expression : number?
;; 待求值数值表达式。
;; error : real?
;; 允许的最大误差。
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
              (test-begin "approx")
              (test-approximate 3.14 3.14159 0.01)
              (test-approximate "named" 10.0 10.05 0.1)
              (test-end "approx")
            ) ;lambda
) ;%run-tests

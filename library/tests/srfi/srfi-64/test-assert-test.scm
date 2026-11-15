;; test-assert
;; 断言表达式求值为真值。
;;
;; 语法
;; ----
;; (test-assert expression)
;; (test-assert test-name expression)
;;
;; 参数
;; ----
;; test-name : string? (可选)
;; 测试用例名称。
;; expression : any
;; 待求值的测试表达式。
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
              (test-begin "assert")
              (test-assert #t)
              (test-assert "named" (= 1 1))
              (test-end "assert")
            ) ;lambda
) ;%run-tests

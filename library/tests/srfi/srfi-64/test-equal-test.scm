;; test-equal
;; 使用 equal? 断言两个表达式结构相等。
;;
;; 语法
;; ----
;; (test-equal expected expression)
;; (test-equal test-name expected expression)
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
              (test-begin "equal")
              (test-equal '(1 2 3) (list 1 2 3))
              (test-equal "named" "abc" (string-append "a" "bc"))
              (test-end "equal")
            ) ;lambda
) ;%run-tests

;; test-group
;; 定义并执行一个带独立作用域的测试组。
;;
;; 语法
;; ----
;; (test-group suite-name body ...)
;;
;; 参数
;; ----
;; suite-name : string?
;; 测试组名称。
;; body : any
;; 测试组内的表达式。
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

(%run-tests
  (lambda () (test-group "grp" (test-assert "a" #t) (test-equal 2 (+ 1 1))))
) ;%run-tests

;; test-runner-group-stack!
;; 设置测试运行器的测试组名称栈。
;;
;; 语法
;; ----
;; (test-runner-group-stack! runner value)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;; value : list?
;; 要设置的新值。
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

(let ((r (test-runner-null)))
  (test-runner-group-stack! r '("a"))
  (if (not (equal? '("a") (test-runner-group-stack r)))
    (error "test-runner-group-stack! failed")
  ) ;if
) ;let

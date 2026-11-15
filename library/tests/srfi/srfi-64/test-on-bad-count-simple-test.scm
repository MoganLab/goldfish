;; test-on-bad-count-simple
;; 简单测试运行器的测试数量不匹配默认回调。
;;
;; 语法
;; ----
;; (test-on-bad-count-simple runner count expected-count)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器。
;; count : integer?
;; 实际测试数。
;; expected-count : integer?
;; 预期测试数。
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

(if (not (procedure? test-on-bad-count-simple)) (error "not a procedure"))

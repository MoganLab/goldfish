;; test-on-group-begin-simple
;; 简单测试运行器的组开始默认回调。
;;
;; 语法
;; ----
;; (test-on-group-begin-simple runner suite-name count)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器。
;; suite-name : string?
;; 测试组名称。
;; count : integer? | #f
;; 预期测试数。
;;
;; 返回值
;; ----
;; any

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

(if (not (procedure? test-on-group-begin-simple)) (error "not a procedure"))

;; test-on-group-end-simple
;; 简单测试运行器的组结束默认回调。
;;
;; 语法
;; ----
;; (test-on-group-end-simple runner)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器。
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

(let ((r (test-runner-null)))
  (test-on-group-end-simple r)
) ;let

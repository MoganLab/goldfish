;; test-runner-reset
;; 重置测试运行器的所有计数器和状态。
;;
;; 语法
;; ----
;; (test-runner-reset runner)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
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
  (test-runner-pass-count! r 5)
  (test-runner-reset r)
  (if (not (= 0 (test-runner-pass-count r))) (error "test-runner-reset failed"))
) ;let

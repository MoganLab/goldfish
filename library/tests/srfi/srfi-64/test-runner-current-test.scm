;; test-runner-current
;; 获取或设置当前的测试运行器参数。
;;
;; 语法
;; ----
;; (test-runner-current)
;; (test-runner-current runner)
;;
;; 参数
;; ----
;; runner : test-runner? | #f (可选)
;; 要设置的测试运行器。
;;
;; 返回值
;; ----
;; test-runner? | #f

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
  (test-with-runner r
    (if (not (eq? r (test-runner-current))) (error "test-runner-current failed"))
  ) ;test-with-runner
) ;let

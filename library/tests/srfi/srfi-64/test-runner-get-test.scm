;; test-runner-get
;; 获取当前已初始化的测试运行器，未初始化时报错。
;;
;; 语法
;; ----
;; (test-runner-get)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; test-runner?

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
    (if (not (eq? r (test-runner-get))) (error "test-runner-get failed"))
  ) ;test-with-runner
) ;let

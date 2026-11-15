;; test-runner-skip-count
;; 获取测试运行器的跳过测试计数。
;;
;; 语法
;; ----
;; (test-runner-skip-count runner)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;;
;; 返回值
;; ----
;; integer?

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
  (test-runner-skip-count! r 5)
  (if (not (equal? 5 (test-runner-skip-count r)))
    (error "test-runner-skip-count failed")
  ) ;if
) ;let

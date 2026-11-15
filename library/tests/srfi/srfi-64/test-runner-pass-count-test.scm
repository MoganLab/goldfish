;; test-runner-pass-count
;; 获取测试运行器的预期通过测试计数。
;;
;; 语法
;; ----
;; (test-runner-pass-count runner)
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
  (test-runner-pass-count! r 3)
  (if (not (equal? 3 (test-runner-pass-count r)))
    (error "test-runner-pass-count failed")
  ) ;if
) ;let

;; test-runner-xfail-count
;; 获取测试运行器的预期失败测试计数。
;;
;; 语法
;; ----
;; (test-runner-xfail-count runner)
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
  (test-runner-xfail-count! r 4)
  (if (not (equal? 4 (test-runner-xfail-count r)))
    (error "test-runner-xfail-count failed")
  ) ;if
) ;let

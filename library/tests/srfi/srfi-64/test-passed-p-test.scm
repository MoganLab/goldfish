;; test-passed?
;; 判断最近一次测试是否通过（pass 或 xpass）。
;;
;; 语法
;; ----
;; (test-passed?)
;; (test-passed? runner)
;;
;; 参数
;; ----
;; runner : test-runner? (可选)
;; 测试运行器对象，默认为当前运行器。
;;
;; 返回值
;; ----
;; boolean?

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

(%run-tests
  (lambda ()
    (test-begin "passed")
    (test-assert #t)
    (let ((ok (test-passed?)))
      (test-assert ok)
    ) ;let
    (test-end "passed")
  ) ;lambda
) ;%run-tests

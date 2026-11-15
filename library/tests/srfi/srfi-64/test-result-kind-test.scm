;; test-result-kind
;; 获取最近一次测试的结果类型符号。
;;
;; 语法
;; ----
;; (test-result-kind)
;; (test-result-kind runner)
;;
;; 参数
;; ----
;; runner : test-runner? (可选)
;; 测试运行器对象，默认为当前运行器。
;;
;; 返回值
;; ----
;; symbol?

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
    (test-begin "kind")
    (test-assert #t)
    (let ((k (test-result-kind)))
      (test-equal 'pass k)
    ) ;let
    (test-end "kind")
  ) ;lambda
) ;%run-tests

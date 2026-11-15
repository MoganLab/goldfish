;; test-runner-group-path
;; 返回从最外层到最内层的当前测试组名称列表。
;;
;; 语法
;; ----
;; (test-runner-group-path runner)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;;
;; 返回值
;; ----
;; list?

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
    (test-begin "outer")
    (test-begin "inner")
    (test-equal '("outer" "inner") (test-runner-group-path (test-runner-current)))
    (test-end "inner")
    (test-end "outer")
  ) ;lambda
) ;%run-tests

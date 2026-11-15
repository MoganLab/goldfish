;; test-end
;; 结束当前测试组。
;;
;; 语法
;; ----
;; (test-end)
;; (test-end suite-name)
;;
;; 参数
;; ----
;; suite-name : string? (可选)
;; 需要匹配的测试组名称。
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

(%run-tests (lambda ()
              (test-begin "g1")
              (test-assert #t)
              (test-end "g1")
              (test-begin "g2")
              (test-end)
            ) ;lambda
) ;%run-tests

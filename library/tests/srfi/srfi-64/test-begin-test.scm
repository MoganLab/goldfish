;; test-begin
;; 开始一个测试组。
;;
;; 语法
;; ----
;; (test-begin suite-name)
;; (test-begin suite-name count)
;;
;; 参数
;; ----
;; suite-name : string?
;; 测试组名称。
;; count : integer? (可选)
;; 期望执行的测试用例数量。
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

(%run-tests (lambda () (test-begin "g1" 1) (test-assert "ok" #t) (test-end "g1"))
) ;%run-tests

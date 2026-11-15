;; test-skip
;; 在当前测试组中跳过匹配指定条件的测试。
;;
;; 语法
;; ----
;; (test-skip pred ...)
;;
;; 参数
;; ----
;; pred : any
;; 测试匹配器、测试名字符串或整数。
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
              (test-begin "skip")
              (test-skip "s1")
              (test-assert "s1" #f)
              (test-assert "p1" #t)
              (test-end "skip")
            ) ;lambda
) ;%run-tests

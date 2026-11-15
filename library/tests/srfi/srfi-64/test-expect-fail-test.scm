;; test-expect-fail
;; 在当前测试组中标记匹配指定条件的测试预期失败。
;;
;; 语法
;; ----
;; (test-expect-fail pred ...)
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
              (test-begin "xfail")
              (test-expect-fail "xf1")
              (test-assert "xf1" #f)
              (test-end "xfail")
            ) ;lambda
) ;%run-tests

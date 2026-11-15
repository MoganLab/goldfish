;; test-runner-test-name
;; 获取运行器当前测试的名称，未设置时返回空字符串。
;;
;; 语法
;; ----
;; (test-runner-test-name runner)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;;
;; 返回值
;; ----
;; string?

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
  (if (not (equal? "" (test-runner-test-name r))) (error "expected empty name"))
  (test-result-set! r 'test-name "abc")
  (if (not (equal? "abc" (test-runner-test-name r))) (error "expected abc"))
) ;let

;; test-with-runner
;; 在指定的 test-runner 上下文中执行表单。
;;
;; 语法
;; ----
;; (test-with-runner runner form ...)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 要临时设为当前运行器的对象。
;; form : any
;; 在上下文中求值的表单。
;;
;; 返回值
;; ----
;; any

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
  (test-with-runner r
    (test-begin "with-runner")
    (test-assert "ok" #t)
    (test-end "with-runner")
  ) ;test-with-runner
  (if (not (= 1 (test-runner-pass-count r))) (error "unexpected pass count"))
) ;let

;; test-runner-on-group-end!
;; 设置测试运行器的测试组结束回调函数。
;;
;; 语法
;; ----
;; (test-runner-on-group-end! runner value)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;; value : procedure?
;; 要设置的新值。
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

(let ((r (test-runner-null)) (cb (lambda (r) 'ok)))
  (test-runner-on-group-end! r cb)
  (if (not (eq? cb (test-runner-on-group-end r)))
    (error "test-runner-on-group-end! failed")
  ) ;if
) ;let

;; test-runner-on-bad-end-name
;; 获取测试运行器的组名不匹配回调函数。
;;
;; 语法
;; ----
;; (test-runner-on-bad-end-name runner)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;;
;; 返回值
;; ----
;; procedure?

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

(let ((r (test-runner-null)) (cb (lambda (r b e) 'ok)))
  (test-runner-on-bad-end-name! r cb)
  (if (not (eq? cb (test-runner-on-bad-end-name r)))
    (error "test-runner-on-bad-end-name failed")
  ) ;if
) ;let

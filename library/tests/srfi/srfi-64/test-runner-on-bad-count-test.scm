;; test-runner-on-bad-count
;; 获取测试运行器的计数不匹配回调函数。
;;
;; 语法
;; ----
;; (test-runner-on-bad-count runner)
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

(let ((r (test-runner-null)) (cb (lambda (r c e) 'ok)))
  (test-runner-on-bad-count! r cb)
  (if (not (eq? cb (test-runner-on-bad-count r)))
    (error "test-runner-on-bad-count failed")
  ) ;if
) ;let

;; test-runner-aux-value
;; 获取测试运行器的运行器附加自定义值。
;;
;; 语法
;; ----
;; (test-runner-aux-value runner)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
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
  (test-runner-aux-value! r 'aux)
  (if (not (equal? 'aux (test-runner-aux-value r)))
    (error "test-runner-aux-value failed")
  ) ;if
) ;let

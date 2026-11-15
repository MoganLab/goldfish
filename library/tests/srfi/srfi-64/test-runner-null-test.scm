;; test-runner-null
;; 创建一个不产生任何输出的空测试运行器。
;;
;; 语法
;; ----
;; (test-runner-null)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; test-runner?

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
  (if (not (test-runner? r)) (error "test-runner-null failed"))
) ;let

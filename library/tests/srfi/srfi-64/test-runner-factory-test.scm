;; test-runner-factory
;; 获取或设置创建默认测试运行器的工厂函数参数。
;;
;; 语法
;; ----
;; (test-runner-factory)
;; (test-runner-factory factory)
;;
;; 参数
;; ----
;; factory : procedure? (可选)
;; 返回 test-runner? 的无参过程。
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

(let ((saved (test-runner-factory)))
  (test-runner-factory test-runner-null)
  (let ((ok (eq? (test-runner-factory) test-runner-null)))
    (test-runner-factory saved)
    (if (not ok) (error "test-runner-factory failed"))
  ) ;let
) ;let

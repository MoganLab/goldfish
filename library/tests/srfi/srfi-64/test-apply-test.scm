;; test-apply
;; 在指定的匹配器过滤下执行测试过程。
;;
;; 语法
;; ----
;; (test-apply specifier ... thunk)
;; (test-apply runner specifier ... thunk)
;;
;; 参数
;; ----
;; runner : test-runner? (可选)
;; 测试运行器。
;; specifier : any
;; 测试匹配器或名称。
;; thunk : procedure?
;; 要执行的测试无参过程。
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

(%run-tests
  (lambda ()
    (test-begin "apply")
    (test-apply (test-match-name "p")
      (lambda () (test-assert "x" #f) (test-assert "p" #t))
    ) ;test-apply
    (test-end "apply")
  ) ;lambda
) ;%run-tests

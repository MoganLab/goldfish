;; test-group-with-cleanup
;; 定义带清理动作的测试组，异常退出时也会执行清理。
;;
;; 语法
;; ----
;; (test-group-with-cleanup suite-name form ... cleanup-form)
;;
;; 参数
;; ----
;; suite-name : string?
;; 测试组名称。
;; form : any
;; 测试体表达式。
;; cleanup-form : any
;; 清理表达式。
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

(let ((cleaned #f))
  (%run-tests (lambda () (test-group-with-cleanup "clean" (test-assert #t) (set! cleaned #t)))
  ) ;%run-tests
  (if (not cleaned) (error "cleanup not executed"))
) ;let

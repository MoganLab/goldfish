;; test-on-bad-end-name-simple
;; 简单测试运行器的组结束名称不匹配默认回调。
;;
;; 语法
;; ----
;; (test-on-bad-end-name-simple runner begin-name end-name)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器。
;; begin-name : string?
;; 开始组名。
;; end-name : string?
;; 结束组名。
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

(if (not (procedure? test-on-bad-end-name-simple)) (error "not a procedure"))

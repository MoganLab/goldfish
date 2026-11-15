;; test-result-remove
;; 从测试结果属性表中移除指定属性。
;;
;; 语法
;; ----
;; (test-result-remove runner pname)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;; pname : symbol?
;; 要移除的属性名。
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

(let ((r (test-runner-null)))
  (test-result-set! r 'foo 1)
  (test-result-set! r 'bar 2)
  (test-result-remove r 'foo)
  (if
    (or (test-result-ref r 'foo) (not (= 2 (test-result-ref r 'bar))))
    (error "test-result-remove failed")
  ) ;if
) ;let

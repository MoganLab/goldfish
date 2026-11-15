;; test-result-set!
;; 设置测试结果属性表中的指定属性值。
;;
;; 语法
;; ----
;; (test-result-set! runner pname value)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;; pname : symbol?
;; 属性名。
;; value : any
;; 要设置的属性值。
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
  (test-result-set! r 'foo 123)
  (if (not (= 123 (test-result-ref r 'foo))) (error "test-result-set! failed"))
) ;let

;; test-result-alist!
;; 设置测试运行器的测试结果关联列表。
;;
;; 语法
;; ----
;; (test-result-alist! runner value)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;; value : list?
;; 要设置的新值。
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
  (test-result-alist! r '((a . 1)))
  (if
    (not (equal? '((a . 1)) (test-result-alist r)))
    (error "test-result-alist! failed")
  ) ;if
) ;let

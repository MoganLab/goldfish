;; test-result-alist
;; 获取测试运行器的测试结果关联列表。
;;
;; 语法
;; ----
;; (test-result-alist runner)
;;
;; 参数
;; ----
;; runner : test-runner?
;; 测试运行器对象。
;;
;; 返回值
;; ----
;; list?

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
    (error "test-result-alist failed")
  ) ;if
) ;let

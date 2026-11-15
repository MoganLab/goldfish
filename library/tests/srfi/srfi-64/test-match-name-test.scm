;; test-match-name
;; 构造按测试名称精确匹配的匹配器。
;;
;; 语法
;; ----
;; (test-match-name name)
;;
;; 参数
;; ----
;; name : string?
;; 要匹配的测试名称。
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

(%run-tests (lambda ()
              (test-begin "name")
              (test-skip (test-match-name "skip-it"))
              (test-assert "skip-it" #f)
              (test-assert "keep-it" #t)
              (test-end "name")
            ) ;lambda
) ;%run-tests

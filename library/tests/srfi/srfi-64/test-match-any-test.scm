;; test-match-any
;; 构造逻辑或组合匹配器，任一子匹配器返回真时匹配。
;;
;; 语法
;; ----
;; (test-match-any pred ...)
;;
;; 参数
;; ----
;; pred : any
;; 测试匹配器、字符串名或整数计数。
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
              (test-begin "any")
              (test-skip (test-match-any "a" "b"))
              (test-assert "a" #f)
              (test-assert "b" #f)
              (test-assert "c" #t)
              (test-end "any")
            ) ;lambda
) ;%run-tests

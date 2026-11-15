;; test-match-all
;; 构造逻辑与组合匹配器，所有子匹配器均返回真时匹配。
;;
;; 语法
;; ----
;; (test-match-all pred ...)
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

(%run-tests
  (lambda ()
    (test-begin "all")
    (test-skip (test-match-all (test-match-nth 1 2) (test-match-name "a")))
    (test-assert "a" #f)
    (test-assert "b" #t)
    (test-end "all")
  ) ;lambda
) ;%run-tests

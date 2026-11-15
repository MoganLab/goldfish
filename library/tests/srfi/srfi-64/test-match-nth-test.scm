;; test-match-nth
;; 构造匹配第 n 个及之后共 count 个测试的匹配器。
;;
;; 语法
;; ----
;; (test-match-nth n)
;; (test-match-nth n count)
;;
;; 参数
;; ----
;; n : integer?
;; 起始序号（从 1 开始）。
;; count : integer? (可选，默认 1)
;; 匹配的连续测试数量。
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
              (test-begin "nth")
              (test-skip (test-match-nth 1))
              (test-assert "skip-me" #f)
              (test-assert "run-me" #t)
              (test-end "nth")
            ) ;lambda
) ;%run-tests

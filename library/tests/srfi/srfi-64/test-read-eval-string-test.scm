;; test-read-eval-string
;; 读取字符串中的单个 Scheme 表达式并求值。
;;
;; 语法
;; ----
;; (test-read-eval-string string)
;;
;; 参数
;; ----
;; string : string?
;; 包含单个 Scheme 表达式的字符串。
;;
;; 返回值
;; ----
;; any

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
              (test-begin "read-eval")
              (test-equal 6 (test-read-eval-string "(+ 1 2 3)"))
              (test-end "read-eval")
            ) ;lambda
) ;%run-tests

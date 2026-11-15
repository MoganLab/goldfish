;; test-log-to-file
;; 控制简单测试运行器是否将日志写入文件。
;;
;; 语法
;; ----
;; test-log-to-file
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; boolean? | string?

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

(if (not (or (boolean? test-log-to-file) (string? test-log-to-file)))
  (error "invalid test-log-to-file")
) ;if

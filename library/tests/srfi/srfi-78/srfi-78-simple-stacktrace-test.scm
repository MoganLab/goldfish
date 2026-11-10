;; check-report
;; 验证断言失败时输出的栈回溯定位到实际断言位置，而非 check-report 调用处。
;;
;; 语法
;; ----
;; (check-report)
;;
;; 参数
;; ----
;; 无
;; 该函数不接受任何参数。
;;
;; 返回值
;; ----
;; unspecified
;; 存在失败断言时以状态码 -1 退出进程。
;;
;; 说明
;; ----
;; 仅当环境变量 GOLDFISH_TEST_STACKTRACE 非空且不为 "0" 时执行断言，
;; 避免在常规测试运行中产生预期内的失败输出。

(import (liii check) (liii os))

;; Only run this test when GOLDFISH_TEST_STACKTRACE is set
(define stacktrace-test-enabled?
  (let ((env (getenv "GOLDFISH_TEST_STACKTRACE")))
    (and env (not (equal? env "0")))
  ) ;let
) ;define

(when stacktrace-test-enabled?
  ;; Simple test for stacktrace display on failure
  (check-set-mode! 'report-failed)

  ;; Test basic failure
  (check (+ 1 1) => 3)
) ;when

(check-report)

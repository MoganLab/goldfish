;; run-test-file
;; TODO: 补充 run-test-file 的简要描述。
;;
;; 语法
;; ----
;; (run-test-file ...)
;;
;; 参数
;; ----
;; TODO: 参数说明。
;;
;; 返回值
;; ------
;; TODO: 返回值说明。

(import (scheme base) (liii check) (liii goldtest-runner))

(check-set-mode! 'report-failed)

;; TODO: 补充 run-test-file 的断言用例。
(check-true (procedure? run-test-file))

;; 从项目根目录聚合测试时，子进程仍进入 gfproject.json 配置的 domain root。
(let ((result (run-test-file "tools/test/tests/liii/goldtest/main-test.scm")))
  (check (car result) => "tools/test/tests/liii/goldtest/main-test.scm")
  (check (cdr result) => 0)
) ;let

(check-report)

;; run-tests-parallel
;; TODO: 补充 run-tests-parallel 的简要描述。
;;
;; 语法
;; ----
;; (run-tests-parallel ...)
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

;; TODO: 补充 run-tests-parallel 的断言用例。
(check-true (procedure? run-tests-parallel))

;; 并行聚合运行也必须为每个 domain 设置独立的子进程工作目录。
(let ((results (run-tests-parallel '("tools/test/tests/liii/goldtest/main-test.scm"
                                     "tools/test/tests/liii/goldtest-args/classify-test-arg-test.scm")
                 2
               ) ;run-tests-parallel
      ) ;results
     ) ;
  (check (length results) => 2)
  (check (map car results)
    =>
    '("tools/test/tests/liii/goldtest/main-test.scm"
      "tools/test/tests/liii/goldtest-args/classify-test-arg-test.scm")
  ) ;check
  (check (map cdr results) => '(0 0))
) ;let

(check-report)

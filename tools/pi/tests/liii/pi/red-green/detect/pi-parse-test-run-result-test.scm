(import (liii check)
        (liii string)
        (liii pi red-green detect)
) ;import

(check-set-mode! 'report-failed)

(define (count key result)
  (and result (cdr (assoc key result)))
) ;define

;; pi-tdd-parse-test-run-result
;; 解析 pytest / vitest / jest / go test / cargo test 摘要。

;; pytest
(let ((r (pi-tdd-parse-test-run-result
           "===== 3 passed, 1 failed, 2 errors in 0.42s ====="
           1
         ) ;pi-tdd-parse-test-run-result
      )) ;
  (check (count 'passed r) => 3)
  (check (count 'failed r) => 1)
  (check (count 'errors r) => 2)
  (check (count 'exit-code r) => 1)
) ;let

;; pytest 全通过
(let ((r (pi-tdd-parse-test-run-result "=== 6 passed in 0.11s ===" 0)))
  (check (count 'passed r) => 6)
  (check (count 'failed r) => 0)
) ;let

;; vitest: "Tests  1 failed | 23 passed (24)"
(let ((r (pi-tdd-parse-test-run-result "Tests  1 failed | 23 passed (24)" 1)))
  (check (count 'failed r) => 1)
  (check (count 'passed r) => 23)
) ;let

;; vitest 全通过: "Tests  24 passed (24)"
(let ((r (pi-tdd-parse-test-run-result "Tests  24 passed (24)" 0)))
  (check (count 'passed r) => 24)
  (check (count 'failed r) => 0)
) ;let

;; jest: "Tests:       1 failed, 2 passed, 3 total"
(let ((r (pi-tdd-parse-test-run-result "Tests:       1 failed, 2 passed, 3 total" 1)))
  (check (count 'failed r) => 1)
  (check (count 'passed r) => 2)
) ;let

;; go test（-v 输出）
(let ((text (string-append "=== RUN   TestAdd\n"
              "--- PASS: TestAdd (0.00s)\n"
              "=== RUN   TestSub\n"
              "--- FAIL: TestSub (0.00s)\n"
              "FAIL\texample.com/calc\t0.003s\n"
            ) ;string-append
      ) ;text
     ) ;
  (let ((r (pi-tdd-parse-test-run-result text 1)))
    (check (count 'passed r) => 1)
    (check (count 'failed r) => 1)
  ) ;let
) ;let

;; go test（非 -v，仅有包级结果）
(let ((r (pi-tdd-parse-test-run-result "ok  \texample.com/calc\t0.003s\n" 0)))
  (check (count 'passed r) => 1)
  (check (count 'failed r) => 0)
) ;let

;; go test 编译失败 → errors
(let ((r (pi-tdd-parse-test-run-result
           "# example.com/calc\n./calc_test.go:3:2: undefined: foo\nFAIL\texample.com/calc [build failed]\n"
           1
         ) ;pi-tdd-parse-test-run-result
      )) ;
  (check (count 'errors r) => 1)
) ;let

;; cargo test
(let ((r (pi-tdd-parse-test-run-result
           "test result: FAILED. 1 passed; 2 failed; 0 ignored; 0 measured; 0 filtered out"
           101
         ) ;pi-tdd-parse-test-run-result
      )) ;
  (check (count 'passed r) => 1)
  (check (count 'failed r) => 2)
) ;let

;; goldfish bin/gf test 摘要
(let ((r (pi-tdd-parse-test-run-result "=== Summary ===\n  Total:  1\n  Passed: 1\n" 0)))
  (check (count 'passed r) => 1)
) ;let

;; 非测试输出不算测试运行
(check (pi-tdd-parse-test-run-result "hello world\n" 0) => #f)

;; 非零退出但没有任何计数 → errors 1（例如命令不存在）
(let ((r (pi-tdd-parse-test-run-result "bash: pytest: command not found" 127)))
  (check (count 'errors r) => 1)
) ;let

;; pi-tdd-test-run-trigger
(check (pi-tdd-test-run-trigger '((passed . 0) (failed . 2) (errors . 0))) => 'tests-fail)
(check (pi-tdd-test-run-trigger '((passed . 0) (failed . 0) (errors . 1))) => 'tests-error)
(check (pi-tdd-test-run-trigger '((passed . 3) (failed . 0) (errors . 0))) => 'tests-pass)
(check (pi-tdd-test-run-trigger '((passed . 0) (failed . 0) (errors . 0))) => #f)

;; pi-tdd-exit-code-from-text
(check (pi-tdd-exit-code-from-text "3 failed\n\nCommand exited with code 1" #t) => 1)
(check (pi-tdd-exit-code-from-text "Command timed out after 120 seconds" #t) => -1)
(check (pi-tdd-exit-code-from-text "3 passed" #f) => 0)

(check-report)

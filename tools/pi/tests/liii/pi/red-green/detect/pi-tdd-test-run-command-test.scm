(import (liii check)
        (liii pi red-green detect)
) ;import

(check-set-mode! 'report-failed)

;; pi-tdd-test-run-command?
;; 只把测试运行器命令视为测试运行，避免普通输出误触发阶段推进。

(check (pi-tdd-test-run-command? "pytest -q") => #t)
(check (pi-tdd-test-run-command? "python -m pytest tests/") => #t)
(check (pi-tdd-test-run-command? "npx vitest run") => #t)
(check (pi-tdd-test-run-command? "npx jest") => #t)
(check (pi-tdd-test-run-command? "go test ./...") => #t)
(check (pi-tdd-test-run-command? "cargo test") => #t)
(check (pi-tdd-test-run-command? "bin/gf test tests/liii/foo-test.scm") => #t)
(check (pi-tdd-test-run-command? "npm test") => #t)
(check (pi-tdd-test-run-command? "make test") => #t)

(check (pi-tdd-test-run-command? "git status") => #f)
(check (pi-tdd-test-run-command? "ls -la") => #f)
(check (pi-tdd-test-run-command? "cat tests/foo.py") => #f)
(check (pi-tdd-test-run-command? "") => #f)

;; 大小写不敏感
(check (pi-tdd-test-run-command? "PYTEST -q") => #t)

(check-report)

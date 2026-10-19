(import (liii check)
        (liii string)
        (liii pi red-green classify)
) ;import

(check-set-mode! 'report-failed)

;; pi-classify-tdd-file
;; 按语言模式表判定 test / implementation / other。

;; scheme
(check (pi-classify-tdd-file "tools/pi/tests/liii/pi/foo-test.scm") => 'test)
(check (pi-classify-tdd-file "tests/liii/base/string-test.scm") => 'test)
(check (pi-classify-tdd-file "liii/pi/tests/helper.scm") => 'test)
(check (pi-classify-tdd-file "tools/pi/liii/pi/foo.scm") => 'implementation)
(check (pi-classify-tdd-file "goldfish/liii/string.scm") => 'implementation)

;; python
(check (pi-classify-tdd-file "test_calc.py") => 'test)
(check (pi-classify-tdd-file "pkg/calc_test.py") => 'test)
(check (pi-classify-tdd-file "pkg/tests/test_calc.py") => 'test)
(check (pi-classify-tdd-file "pkg/conftest.py") => 'test)
(check (pi-classify-tdd-file "pkg/calc.py") => 'implementation)

;; typescript / javascript
(check (pi-classify-tdd-file "src/calc.test.ts") => 'test)
(check (pi-classify-tdd-file "src/calc.spec.tsx") => 'test)
(check (pi-classify-tdd-file "src/calc.ts") => 'implementation)
(check (pi-classify-tdd-file "src/calc.js") => 'implementation)

;; go / rust / java / php
(check (pi-classify-tdd-file "pkg/calc_test.go") => 'test)
(check (pi-classify-tdd-file "pkg/calc.go") => 'implementation)
(check (pi-classify-tdd-file "tests/integration.rs") => 'test)
(check (pi-classify-tdd-file "src/lib.rs") => 'implementation)
(check (pi-classify-tdd-file "src/CalcTest.java") => 'test)
(check (pi-classify-tdd-file "src/Calc.java") => 'implementation)
(check (pi-classify-tdd-file "src/CalcTest.php") => 'test)

;; 配置文件与文档一律 other，不受 RED 门禁限制
(check (pi-classify-tdd-file "package.json") => 'other)
(check (pi-classify-tdd-file "tsconfig.build.json") => 'other)
(check (pi-classify-tdd-file "pyproject.toml") => 'other)
(check (pi-classify-tdd-file "Makefile") => 'other)
(check (pi-classify-tdd-file "README.md") => 'other)

;; 未知语言与无扩展名归 other
(check (pi-classify-tdd-file "notes.unknown") => 'other)
(check (pi-classify-tdd-file "LICENSE") => 'other)

;; 绝对路径与 Windows 分隔符
(check (pi-classify-tdd-file "/home/me/proj/test_calc.py") => 'test)
(check (pi-classify-tdd-file "src\\calc.test.ts") => 'test)

;; pi-tdd-glob-match?
(check (pi-tdd-glob-match? "a/b/c.py" "**/test_*.py") => #f)
(check (pi-tdd-glob-match? "a/b/test_c.py" "**/test_*.py") => #t)
(check (pi-tdd-glob-match? "tests/x/y.rs" "**/tests/**/*.rs") => #t)
(check (pi-tdd-glob-match? "src/x.rs" "**/tests/**/*.rs") => #f)
(check (pi-tdd-glob-match? "a.ts" "*.ts") => #t)
(check (pi-tdd-glob-match? "dir/a.ts" "*.ts") => #t)

;; pi-tdd-language-of
(check (pi-tdd-language-of "x.ts") => 'typescript)
(check (pi-tdd-language-of "x.py") => 'python)
(check (pi-tdd-language-of "x.scm") => 'scheme)
(check (pi-tdd-language-of "x.unknown") => #f)

(check-report)

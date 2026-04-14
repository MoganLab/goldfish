;; 添加 tools/goldtest 到 load path，以便导入 (liii goldtest)
(set! *load-path* (cons "tools/goldtest" *load-path*))

(import (liii check)
        (liii goldtest)
) ;import

(check (failed-test-files '()) => '())

(check (failed-test-files '(("tests/a-test.scm" . 0)
                            ("tests/b-test.scm" . 1)
                            ("tests/c-test.scm" . 0)
                            ("tests/d-test.scm" . 255)))
  => '("tests/b-test.scm" "tests/d-test.scm")
) ;check

(check (failed-test-files '(("tests/only-pass-test.scm" . 0)))
  => '()
) ;check

(check-report)

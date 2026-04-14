;; 添加 tools/goldtest 到 load path，以便导入 (liii goldtest)
(set! *load-path* (cons "tools/goldtest" *load-path*))

(import (liii check)
        (liii goldtest)
) ;import

(check (split-tests-target "tools/test/tests")
  => '("tools/test" . "tests/")
) ;check

(check (split-tests-target "tools/test/tests/")
  => '("tools/test" . "tests/")
) ;check

(check (split-tests-target "tools/test/tests/liii/goldtest")
  => '("tools/test" . "tests/liii/goldtest")
) ;check

(check (split-tests-target "tools\\test\\tests")
  => '("tools\\test" . "tests\\")
) ;check

(check (split-tests-target "tests/liii/goldtest")
  => #f
) ;check

(check (split-tests-target "tools/test/testsuite")
  => #f
) ;check

(check-report)

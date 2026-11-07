;; split-tests-target
;; 拆分测试目标参数为目录与文件列表
;;
;; 语法
;; ----
;; (split-tests-target ...)
;;
;; 参数
;; ----
;; 见各断言场景
;;
;; 返回值
;; ------
;; 见各断言场景

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

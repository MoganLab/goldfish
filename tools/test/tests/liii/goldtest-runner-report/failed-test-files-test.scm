;; failed-test-files
;; 从测试结果列表提取失败文件
;;
;; 语法
;; ----
;; (failed-test-files results)
;;
;; 参数
;; ----
;; results : list?，测试结果列表
;;
;; 返回值
;; ------
;; list of string，失败文件路径

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

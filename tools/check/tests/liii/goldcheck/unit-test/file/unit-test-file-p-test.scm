;; unit-test-file?
;; 判断文件是否为应遵循单元测试规范的测试文件。
;;
;; 语法
;; ----
;; (unit-test-file? file-path)
;;
;; 参数
;; ----
;; file-path : string   待判定的文件路径
;;
;; 返回值
;; ------
;; boolean
;; 为合规的单测文件路径返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldcheck unit-test file))

(check-set-mode! 'report-failed)

;; 1. 测试目录下的 -test.scm 文件应返回 #t
(check (unit-test-file? "tests/scheme/base/assoc-test.scm") => #t)
(check (unit-test-file? "tools/check/tests/liii/goldcheck/check-file-test.scm")
  =>
  #t
) ;check

;; 2. 非 -test.scm 结尾返回 #f
(check (unit-test-file? "tests/scheme/base/README.md") => #f)
(check (unit-test-file? "goldfish/scheme/base.scm") => #f)

;; 3. 源码目录下命名为 -test.scm 的文件排除
(check (unit-test-file? "goldfish/foo-test.scm") => #f)
(check (unit-test-file? "tools/check/liii/goldcheck/unit-test.scm") => #f)

;; 4. 夹具目录下的测试文件排除
(check (unit-test-file? "tests/resources/demo-test.scm") => #f)

(check-report)

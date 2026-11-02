;; unit-test-file?
;; 判断文件是否为应遵循单元测试规范的测试文件（来自 (liii goldcheck unit-test) 导出）。
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

(import (scheme base) (liii check) (liii goldcheck unit-test))

(check-set-mode! 'report-failed)

;; 1. 测试文件命中
(check (unit-test-file? "tests/scheme/base/assoc-test.scm") => #t)

;; 2. 非测试文件排除
(check (unit-test-file? "goldfish/scheme/base.scm") => #f)

(check-report)

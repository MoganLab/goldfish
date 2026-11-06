;; positionals-only-unit-test-files?
;; 判断命令行位置参数列表是否仅包含单元测试文件。
;;
;; 语法
;; ----
;; (positionals-only-unit-test-files? positionals)
;;
;; 参数
;; ----
;; positionals : list  命令行位置参数列表（路径字符串）
;;
;; 返回值
;; ------
;; boolean
;; 当且仅当列表非空且所有路径均为非目录的单元测试文件时返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldcheck only))

(check-set-mode! 'report-failed)

;; 1. 空列表返回 #f
(check (positionals-only-unit-test-files? '()) => #f)

;; 2. 非列表返回 #f
(check (positionals-only-unit-test-files? #f) => #f)

;; 3. 包含目录返回 #f
(check (positionals-only-unit-test-files? '("tests")) => #f)

;; 4. 包含非测试文件返回 #f
(check (positionals-only-unit-test-files? '("README.md")) => #f)

;; 5. 仅包含单元测试文件返回 #t
(check
  (positionals-only-unit-test-files?
    '("r7rs/tests/scheme/base/char-p-test.scm")
  ) ;positionals-only-unit-test-files?
  =>
  #t
) ;check

;; 6. 包含单元测试文件与非测试文件混合返回 #f
(check
  (positionals-only-unit-test-files?
    '("r7rs/tests/scheme/base/char-p-test.scm" "r7rs/scheme/base.scm")
  ) ;positionals-only-unit-test-files?
  =>
  #f
) ;check

(check-report)

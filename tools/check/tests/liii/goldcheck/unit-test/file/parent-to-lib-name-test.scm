;; parent->lib-name
;; 将测试目录相对路径转换为对应的 Scheme 库名形式。
;;
;; 语法
;; ----
;; (parent->lib-name rel-parent)
;;
;; 参数
;; ----
;; rel-parent : string   测试目录相对路径（如 tests/scheme/base）
;;
;; 返回值
;; ------
;; string
;; 库名形式字符串，如 "(scheme base)"。

(import (scheme base) (liii check) (liii goldcheck unit-test file))

(check-set-mode! 'report-failed)

;; 1. 去除 tests/ 前缀并转换
(check (parent->lib-name "tests/scheme/base") => "(scheme base)")
(check (parent->lib-name "tests/liii/string") => "(liii string)")

;; 2. 板块测试目录去除 <板块>/tests/ 前缀
(check (parent->lib-name "r7rs/tests/scheme/base") => "(scheme base)")
(check (parent->lib-name "library/tests/liii/path") => "(liii path)")

;; 3. 非 tests/ 开头的路径直接按段拼接
(check (parent->lib-name "tools/check/unit-test") => "(tools check unit-test)")

(check-report)

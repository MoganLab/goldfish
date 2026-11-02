;; split-slash
;; 依据正斜杠 '/' 分割字符串为子串列表。
;;
;; 语法
;; ----
;; (split-slash s)
;;
;; 参数
;; ----
;; s : string  待分割的字符串
;;
;; 返回值
;; ------
;; list of string
;; 分割后的子串列表。

(import (scheme base) (liii check) (liii goldcheck type-hint util))

(check-set-mode! 'report-failed)

;; 1. 多段路径分割
(check (split-slash "foo/bar/baz") => '("foo" "bar" "baz"))

;; 2. 无斜杠字符串
(check (split-slash "single") => '("single"))

;; 3. 首尾含斜杠
(check (split-slash "/a/b/") => '("" "a" "b" ""))

(check-report)

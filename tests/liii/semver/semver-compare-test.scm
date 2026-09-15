(import (liii check) (liii semver))

(check-set-mode! 'report-failed)

;; semver-compare
;; 按 SemVer 2.0.0 规范比较两个版本字符串的优先级。
;;
;; 语法
;; ----
;; (semver-compare s1 s2)
;;
;; 参数
;; ----
;; s1 : string
;; 左侧版本字符串。
;;
;; s2 : string
;; 右侧版本字符串。
;;
;; 返回值
;; ----
;; integer 或 #f
;; s1 < s2 返回 -1；s1 > s2 返回 1；s1 等于 s2 返回 0。
;; 任一版本非法时返回 #f。核心段缺省的段视为 0（故 1.0 等于 1.0.0）；
;; 预发布版本低于同核心段的正式版；构建元数据不参与比较。
;;
;; 错误处理
;; ----
;; 任一版本非法时不抛错，返回 #f。
;;
;; 示例
;; ----
;; (semver-compare "1.0.0-alpha" "1.0.0") => -1
;; (semver-compare "1.0" "1.0.0") => 0

(check (semver-compare "1.0.0" "2.0.0") => -1)
(check (semver-compare "2.0.0" "1.0.0") => 1)
(check (semver-compare "1.0.0" "1.0.0") => 0)
(check (semver-compare "1.0" "1.0.0") => 0)
(check (semver-compare "1" "1.0.0") => 0)
(check (semver-compare "1.0.0-alpha" "1.0.0") => -1)
(check (semver-compare "1.0.0" "1.0.0-alpha") => 1)
;; 构建元数据在比对时不影响优先级
(check (semver-compare "1.0.0+20130313144700" "1.0.0+exp.sha.5114f85") => 0)
;; 非法版本一律返回 #f
(check (semver-compare "1.0.0" "invalid") => #f)
(check (semver-compare "invalid" "1.0.0") => #f)
(check (semver-compare "invalid" "also-invalid") => #f)

(check-report)

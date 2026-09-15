(import (liii check) (liii semver))

(check-set-mode! 'report-failed)

;; semver-valid?
;; 判断字符串是否为合法的语义化版本（SemVer 2.0.0）。
;;
;; 语法
;; ----
;; (semver-valid? s)
;;
;; 参数
;; ----
;; s : string
;; 待校验的版本字符串，允许首尾空白与前导 v/V。
;;
;; 返回值
;; ----
;; boolean
;; 合法返回 #t，非法返回 #f。核心段允许 1~3 段（比较时缺省段补零），
;; 不允许前导零；预发布与构建元数据须为点分标识符，仅限 ASCII 字母、
;; 数字与连字符，且不得为空；纯数字预发布标识符不允许前导零。
;;
;; 错误处理
;; ----
;; 非法格式不抛错，返回 #f。
;;
;; 示例
;; ----
;; (semver-valid? "1.0.0-rc.1+build.123") => #t
;; (semver-valid? "1.0.0-rc.01") => #f

(check (semver-valid? "2026.3.5") => #t)
(check (semver-valid? "v2026.3.5") => #t)
(check (semver-valid? "2026.3.2-rc.1") => #t)
(check (semver-valid? "2026.3.2-rc.1+build.123") => #t)
(check (semver-valid? "1.0.0-alpha") => #t)
(check (semver-valid? "1.0.0-alpha.1") => #t)
(check (semver-valid? "1.0.0-0.3.7") => #t)
(check (semver-valid? "1.0.0-x.7.z.92") => #t)
;; 非法格式
(check (semver-valid? "") => #f)
(check (semver-valid? "abc") => #f)
(check (semver-valid? "1.0.0-") => #f)
(check (semver-valid? "1.0.0-rc.01") => #f)
(check (semver-valid? "01.2.3") => #f)
;; 构建元数据同样受规范约束：点分标识符，仅限 ASCII 字母数字与连字符，且不得为空
(check (semver-valid? "1.0.0+build.1") => #t)
(check (semver-valid? "1.0.0+20130313144700") => #t)
(check (semver-valid? "1.0.0+exp.sha.5114f85") => #t)
(check (semver-valid? "1.0.0+!!!") => #f)
(check (semver-valid? "1.0.0+build..1") => #f)
;; 核心段允许 1~3 段（缺省段补零），但不得超过三段
(check (semver-valid? "0") => #t)
(check (semver-valid? "1") => #t)
(check (semver-valid? "1.0") => #t)
(check (semver-valid? "1.2.3.4") => #f)
;; 合法：1~3 段核心 + 预发布 + 构建元数据
(check (semver-valid? "1") => #t)
(check (semver-valid? "1.0") => #t)
(check (semver-valid? "1.2.3-alpha.1-1.0+exp.sha.5114f85-001") => #t)
(check (semver-valid? "1.0.0-alpha+exp.sha.5114f85") => #t)
(check (semver-valid? "1.0.0-alpha-0.0.0+exp.sha.5114f85") => #t)
(check (semver-valid? "1.0.0-alpha-0.0.0+exp.sha.5114f85-001") => #t)
(check (semver-valid? "1.0.0+001") => #t)
(check (semver-valid? "1.0.0-x.7.z.92") => #t)
;; 非法字符串
(check (semver-valid? "-1") => #f)
(check (semver-valid? "1.") => #f)
(check (semver-valid? "1.-0") => #f)
(check (semver-valid? "1.0.0-") => #f)
(check (semver-valid? "1.0.0-alpha.01") => #f)
(check (semver-valid? "1.0.0-alpha..1") => #f)
(check (semver-valid? "1.0.0-alpha%") => #f)
(check (semver-valid? "1.0.0-alpha+") => #f)
(check (semver-valid? "1.0.0+") => #f)
(check (semver-valid? "1.0.0+exp%") => #f)
(check (semver-valid? "1.0.0+exp.sha.5114f85+") => #f)
(check (semver-valid? "1.0.0.0") => #f)

(check-report)

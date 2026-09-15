(import (liii check) (liii semver))

(check-set-mode! 'report-failed)

;; semver-clean
;; 去除版本字符串的首尾空白与前导 v/V 前缀。
;;
;; 语法
;; ----
;; (semver-clean s)
;;
;; 参数
;; ----
;; s : string 或任意值
;; 待清洗的版本字符串；非字符串输入按空字符串处理。
;;
;; 返回值
;; ----
;; string
;; 清洗后的字符串。仅当 v/V 后紧跟数字时才剥离前缀，其余仅去除首尾空白。
;;
;; 错误处理
;; ----
;; 非字符串输入不抛错，返回空字符串 ""。
;;
;; 示例
;; ----
;; (semver-clean "  v1.0.0-rc.1\r\n") => "1.0.0-rc.1"
;; (semver-clean "valid") => "valid"

(check (semver-clean "2026.3.5") => "2026.3.5")
(check (semver-clean "v2026.3.5") => "2026.3.5")
(check (semver-clean "V2026.3.5") => "2026.3.5")
(check (semver-clean "  v2026.3.5  ") => "2026.3.5")
(check (semver-clean "\n\tv1.0.0-rc.1\r\n") => "1.0.0-rc.1")
(check (semver-clean "valid") => "valid")
(check (semver-clean "") => "")
(check (semver-clean #f) => "")

(check-report)

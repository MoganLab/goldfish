(import (liii check) (liii semver))

(check-set-mode! 'report-failed)

;; semver=?
;; 判断两个版本是否相等（SemVer 2.0.0 优先级，忽略构建元数据）。
;;
;; 语法
;; ----
;; (semver=? s1 s2)
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
;; boolean 或 #f
;; s1 等于 s2 返回 #t，否则返回 #f；任一版本非法时返回 #f。
;; 核心段缺省的段视为 0（故 1.0 等于 1.0.0）；
;; 构建元数据不参与比较；前导 v/V 会先被归一化。
;;
;; 错误处理
;; ----
;; 任一版本非法时不抛错，返回 #f。
;;
;; 示例
;; ----
;; (semver=? "v2026.3.5" "2026.3.5") => #t
;; (semver=? "1.0.0+build.1" "1.0.0+build.2") => #t

;; 构建元数据在比对时不影响优先级
(check (semver=? "1.0.0+20130313144700" "1.0.0+exp.sha.5114f85") => #t)
(check (semver=? "2026.3.5+001" "2026.3.5+002") => #t)
;; 带前导 'v' 归一后相等
(check (semver=? "v2026.3.5" "2026.3.5") => #t)
(check (semver=? "V2026.3.5" "v2026.3.5") => #t)
;; 相同版本
(check (semver=? "2026.3.5" "2026.3.5") => #t)
(check (semver=? "1.0.0" "2.0.0") => #f)
;; 核心段缺省补零，构建元数据不参与比较
(check (semver=? "1.0" "1.0.0") => #t)
(check (semver=? "1.0-rc" "1.0.0-rc") => #t)
(check (semver=? "1.0+exp.sha.5114f85" "1.0.0+001") => #t)
;; 非法版本返回 #f
(check (semver=? "1.0.0" "invalid") => #f)

(check-report)

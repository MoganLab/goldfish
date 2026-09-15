(import (liii check) (liii semver))

(check-set-mode! 'report-failed)

;; semver<?
;; 判断版本 s1 是否严格小于版本 s2（SemVer 2.0.0 优先级）。
;;
;; 语法
;; ----
;; (semver<? s1 s2)
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
;; s1 < s2 返回 #t，否则返回 #f；任一版本非法时返回 #f。
;; 数值段按数值而非字典序比较；预发布版本低于同核心段的正式版。
;;
;; 错误处理
;; ----
;; 任一版本非法时不抛错，返回 #f。
;;
;; 示例
;; ----
;; (semver<? "2026.3.2-rc.1" "2026.3.5") => #t
;; (semver<? "2.0.0" "10.0.0") => #t

;; 经典用户场景：2026.3.2-rc.1 vs 2026.3.5
(check (semver<? "2026.3.2-rc.1" "2026.3.5") => #t)
;; SemVer 2.0.0 规范第 11 条示例链条：
;; 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta
;; < 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0
(check (semver<? "1.0.0-alpha" "1.0.0-alpha.1") => #t)
(check (semver<? "1.0.0-alpha.1" "1.0.0-alpha.beta") => #t)
(check (semver<? "1.0.0-alpha.beta" "1.0.0-beta") => #t)
(check (semver<? "1.0.0-beta" "1.0.0-beta.2") => #t)
(check (semver<? "1.0.0-beta.2" "1.0.0-beta.11") => #t)
(check (semver<? "1.0.0-beta.11" "1.0.0-rc.1") => #t)
(check (semver<? "1.0.0-rc.1" "1.0.0") => #t)
;; 核心段比较
(check (semver<? "1.0.0" "2.0.0") => #t)
(check (semver<? "2.0.0" "2.1.0") => #t)
(check (semver<? "2.1.0" "2.1.1") => #t)
;; 预发布纯数字标识符按数值大小比较（防止 rc.10 与 rc.2 倒挂）
(check (semver<? "2026.3.2-rc.2" "2026.3.2-rc.10") => #t)
;; 数值段按数值比较，而非字典序
(check (semver<? "2.0.0" "10.0.0") => #t)
(check (semver<? "2.2.0" "2.10.0") => #t)
(check (semver<? "2.2.2" "2.2.10") => #t)
(check (semver<? "1.0.0" "1.0.1") => #t)
;; 预发布段：数值 vs 数值、数值 vs 非数值
(check (semver<? "1.0.0-rc.1.0" "1.0.0-rc.1.1") => #t)
(check (semver<? "1.0.0-alpha.2" "1.0.0-alpha.10") => #t)
(check (semver<? "1.0.0-alpha.2" "1.0.0-alpha.beta") => #t)
;; 非法版本返回 #f
(check (semver<? "1.0.0" "invalid") => #f)

(check-report)

(import (liii check) (liii semver))

(check-set-mode! 'report-failed)

;; semver-parse
;; 将语义化版本字符串解析为 (semver core-nums pre-list) 结构。
;;
;; 语法
;; ----
;; (semver-parse s)
;;
;; 参数
;; ----
;; s : string
;; 待解析的版本字符串，允许首尾空白与前导 v/V。
;;
;; 返回值
;; ----
;; list 或 #f
;; 合法时返回 (semver (MAJOR MINOR PATCH) (PRE ...)) 结构：核心段为
;; 1~3 个数字，缺省段不补零；PRE 元素为 (num . 数值) 或 (str . 字符串)。
;; 构建元数据仅校验合法性，不保留在结果中。非法输入返回 #f。
;;
;; 错误处理
;; ----
;; 非法格式（空串、前导零、空预发布段、核心段超三段等）不抛错，返回 #f。
;;
;; 示例
;; ----
;; (semver-parse "1.0.0-rc.1") => (semver (1 0 0) ((str . "rc") (num . 1)))
;; (semver-parse "01.2.3") => #f

(check (semver-parse "1.2.3") => '(semver (1 2 3) ()))
(check (semver-parse "1.0") => '(semver (1 0) ()))
(check (semver-parse "v1.0.0-rc.1") => '(semver (1 0 0) ((str . "rc") (num . 1))))
(check (semver-parse "1.2.3+build.1") => '(semver (1 2 3) ()))
(check (semver-parse "invalid") => #f)
(check (semver-parse "1.0.0-") => #f)
(check (semver-parse "01.2.3") => #f)

(check-report)

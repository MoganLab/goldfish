(import (liii check) (liii goldpr))

(check-set-mode! 'report-failed)

;; parse-pr-args
;; 解析 gf pr 命令行参数，提取 PR 编号。
;;
;; 语法
;; ----
;; (parse-pr-args args)
;;
;; 参数
;; ----
;; args : list?
;; 完整命令行参数列表，第一个元素是可执行文件路径。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 参数合法（恰好一个纯数字编号）时返回编号字符串，否则返回 #f。

(check (parse-pr-args '("bin/gf" "pr" "933")) => "933")
(check (parse-pr-args '("bin/gf" "pr" "1")) => "1")
(check (parse-pr-args '("bin/gf" "-m" "r7rs" "pr" "930")) => "930")

;; 缺少编号
(check (parse-pr-args '("bin/gf" "pr")) => #f)

;; 编号必须是纯数字
(check (parse-pr-args '("bin/gf" "pr" "abc")) => #f)
(check (parse-pr-args '("bin/gf" "pr" "93a")) => #f)
(check (parse-pr-args '("bin/gf" "pr" "")) => #f)
(check (parse-pr-args '("bin/gf" "pr" "-1")) => #f)

;; 多余的参数
(check (parse-pr-args '("bin/gf" "pr" "1" "2")) => #f)

(check-report)

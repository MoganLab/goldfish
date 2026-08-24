(import (liii check) (liii goldcode))

(check-set-mode! 'report-failed)

;; parse-profile-args
;; 解析 gf code 命令行参数，提取 --profile / -p 指定的配置名。
;;
;; 语法
;; ----
;; (parse-profile-args args)
;;
;; 参数
;; ----
;; args : list?
;; 完整命令行参数列表，第一个元素是可执行文件路径。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 指定了 --profile NAME 或 -p NAME 时返回 NAME，否则返回 #f。

;; --profile 长选项
(check (parse-profile-args '("bin/gf" "code" "--profile" "bigmodel"))
  =>
  "bigmodel"
) ;check
(check (parse-profile-args '("bin/gf" "code" "--profile" "deepseek"))
  =>
  "deepseek"
) ;check

;; -p 短选项
(check (parse-profile-args '("bin/gf" "code" "-p" "bigmodel")) => "bigmodel")

;; 未指定 profile
(check (parse-profile-args '("bin/gf" "code")) => #f)
(check (parse-profile-args '("bin/gf")) => #f)

;; 选项缺少值时视为未指定
(check (parse-profile-args '("bin/gf" "code" "--profile")) => #f)
(check (parse-profile-args '("bin/gf" "code" "-p")) => #f)

(check-report)

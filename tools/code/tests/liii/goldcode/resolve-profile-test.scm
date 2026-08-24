(import (liii check) (liii goldcode))

(check-set-mode! 'report-failed)

;; resolve-profile
;; 解析最终生效的配置名：未指定 --profile 时使用默认配置 "default"。
;;
;; 语法
;; ----
;; (resolve-profile profile)
;;
;; 参数
;; ----
;; profile : string? 或 #f
;; parse-profile-args 的返回值。
;;
;; 返回值
;; ----
;; string?
;; 指定了 profile 时原样返回，否则返回 "default"。
;; 即 gf code 默认加载 ~/.claude/settings.json.default。

(check (resolve-profile #f) => "default")
(check (resolve-profile "bigmodel") => "bigmodel")
(check (resolve-profile "deepseek") => "deepseek")

(check-report)

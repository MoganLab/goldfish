;; parse-remove-args
;; 解析 gf remove 命令行参数。
;;
;; 语法
;; ----
;; (parse-remove-args args)
;;
;; 参数
;; ----
;; args : list?
;; gf remove 之后的参数列表（字符串）。
;;
;; 返回值
;; ----
;; list
;; '(help)、'(remove id) 或 '(error message)。

(import (liii check) (liii goldremove))

(check-set-mode! 'report-failed)

;; 帮助
(check (parse-remove-args '("--help")) => '(help))
(check (parse-remove-args '("-h")) => '(help))

;; 基本卸载
(check (parse-remove-args '("-g" "pi@liii")) => '(remove "pi@liii"))
(check (parse-remove-args '("pi@liii" "-g")) => '(remove "pi@liii"))

;; 缺少 -g / 缺少包名 / 多余包名 / 未知选项
(check (car (parse-remove-args '("pi@liii"))) => 'error)
(check (car (parse-remove-args '("-g"))) => 'error)
(check (car (parse-remove-args '("-g" "a@b" "c@d"))) => 'error)
(check (car (parse-remove-args '("-g" "--bogus"))) => 'error)
(check (car (parse-remove-args '("-g" "pi@liii" "--force"))) => 'error)

(check-report)

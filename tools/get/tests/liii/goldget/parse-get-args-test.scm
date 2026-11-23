;; parse-get-args
;; 解析 gf get 命令行参数。
;;
;; 语法
;; ----
;; (parse-get-args args)
;;
;; 参数
;; ----
;; args : list?
;; gf get 之后的参数列表（字符串）。
;;
;; 返回值
;; ----
;; list
;; '(help)、'(install arg from force?) 或 '(error message)。

(import (liii check) (liii goldget))

(check-set-mode! 'report-failed)

;; 帮助
(check (parse-get-args '("--help")) => '(help))
(check (parse-get-args '("-h")) => '(help))

;; 基本安装
(check (parse-get-args '("-g" "~/git/gf-highlight"))
  => '(install "~/git/gf-highlight" #f #f)
) ;check

;; --from 与 --force
(check (parse-get-args '("-g" "pi@liii" "--from" "https://example.com/r.git" "--force"))
  => '(install "pi@liii" "https://example.com/r.git" #t)
) ;check

;; 缺少 -g / 缺少来源 / 多余来源 / 未知选项 / --from 缺值
(check (car (parse-get-args '("pi@liii"))) => 'error)
(check (car (parse-get-args '("-g"))) => 'error)
(check (car (parse-get-args '("-g" "a@b" "c@d"))) => 'error)
(check (car (parse-get-args '("-g" "--bogus"))) => 'error)
(check (car (parse-get-args '("-g" "pi@liii" "--from"))) => 'error)

(check-report)

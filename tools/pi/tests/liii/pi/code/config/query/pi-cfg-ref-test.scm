;; pi-cfg-ref
;; 从配置对象中读取指定键的取值。
;;
;; 语法
;; ----
;; (pi-cfg-ref cfg key [default-val]) -> any
;;
;; 参数
;; ----
;; cfg : alist
;; 运行时配置。
;;
;; key : symbol
;; 配置键名。
;;
;; default-val : any（可选）
;; 键缺失时的默认值，缺省为 #f。
;;
;; 返回值
;; ------
;; any
;; 键对应的取值，或默认值。

(import (liii check) (liii pi code config query))

(check-set-mode! 'report-failed)

(let ((cfg '((provider . "deepseek") (max-turns . 10))))
  (check (pi-cfg-ref cfg 'provider) => "deepseek")
  (check (pi-cfg-ref cfg 'max-turns) => 10)
  (check (pi-cfg-ref cfg 'missing) => #f)
  (check (pi-cfg-ref cfg 'missing "fallback") => "fallback")
  (check (pi-cfg-ref #f 'provider "google") => "google")
) ;let

(check-report)

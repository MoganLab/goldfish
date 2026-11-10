;; pi-args-ref
;; 从参数字典中读取指定键的取值。
;;
;; 语法
;; ----
;; (pi-args-ref parsed key . default-val)
;;
;; 参数
;; ----
;; parsed : alist
;; 解析后的参数字典。
;;
;; key : symbol
;; 参数键。
;;
;; default-val : any（可选）
;; 键缺失时的默认值，缺省为 #f。
;;
;; 返回值
;; ----
;; any
;; 键对应的取值。

(import (liii check) (liii pi code cli args query))

(check-set-mode! 'report-failed)

(let ((args '((key1 . "val1") (key2 . 42))))
  (check (pi-args-ref args 'key1) => "val1")
  (check (pi-args-ref args 'key2) => 42)
  (check (pi-args-ref args 'not-found) => #f)
  (check (pi-args-ref args 'not-found "default") => "default")
) ;let

(check-report)

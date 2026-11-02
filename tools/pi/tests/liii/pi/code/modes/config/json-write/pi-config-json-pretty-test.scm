;; pi-config-json-pretty
;; 以 2 空格缩进、保序的方式序列化 JSON 值。
;;
;; 语法
;; ----
;; (pi-config-json-pretty v . opt-level)
;;
;; 参数
;; ----
;; v : any
;; 要序列化的 JSON 值。
;; opt-level : integer
;; 可选的初始缩进层级。
;;
;; 返回值
;; ----
;; string
;; 格式化后的 JSON 文本。

(import (liii check) (liii pi code modes config json-write))

(check-set-mode! 'report-failed)

(check (pi-config-json-pretty (vector "a" "b")) => "[\"a\", \"b\"]")

(check (pi-config-json-pretty (list (cons "n" #t) (cons "m" 1)))
  =>
  "{\n  \"n\": true,\n  \"m\": 1\n}"
) ;check

(check-report)

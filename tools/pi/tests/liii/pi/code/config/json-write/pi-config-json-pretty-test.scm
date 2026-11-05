;; pi-config-json-pretty
;; 以 2 空格缩进、保序的方式序列化 JSON 值。
;; 委托给 (liii json) 的 json->string :indent 2（1244 引入的格式化选项）。
;;
;; 语法
;; ----
;; (pi-config-json-pretty v) -> string
;;
;; 参数
;; ----
;; v : any
;; 要序列化的 JSON 值（alist / vector / 标量）。
;;
;; 返回值
;; ----
;; string
;; 格式化后的多行 JSON 文本（2 空格缩进、键序保持、数组逐元素展开）。

(import (liii check)
  (liii json)
  (liii pi code config json-write)
)

(check-set-mode! 'report-failed)

;; 空表序列化为空对象（沿用旧契约）
(check (pi-config-json-pretty '()) => "{}")

;; 数组逐元素展开，不再内联标量数组
(check (pi-config-json-pretty (vector "a" "b")) => "[\n  \"a\",\n  \"b\"\n]")

;; 对象保序、两空格缩进
(check (pi-config-json-pretty (list (cons "n" #t) (cons "m" 1)))
  =>
  "{\n  \"n\": true,\n  \"m\": 1\n}"
) ;check

;; 嵌套数组随层级递增缩进
(check (pi-config-json-pretty (list (cons "k" (vector "a"))))
  =>
  "{\n  \"k\": [\n    \"a\"\n  ]\n}"
) ;check

;; 解析产物 true/false 符号原样输出为 JSON 布尔
(check (pi-config-json-pretty (list (cons "hide" 'true) (cons "off" 'false)))
  =>
  "{\n  \"hide\": true,\n  \"off\": false\n}"
) ;check

;; 嵌套空对象保持 {}
(check (pi-config-json-pretty (string->json "{\"a\":{},\"b\":[]}"))
  =>
  "{\n  \"a\": {},\n  \"b\": []\n}"
) ;check

(check-report)

;; json-dict-ref
;; 从 JSON 字典对象中按符号或字符串键安全读取字段值。
;;
;; 语法
;; ----
;; (json-dict-ref dict key default) -> any
;;
;; 参数
;; ----
;; dict : alist
;; JSON 解析得到的字典对象。
;; key : string | symbol
;; 字段名。
;; default : any
;; 字段缺失时返回的默认值。
;;
;; 返回值
;; ----
;; any
;; 字段值；缺失时返回 default。

(import (liii check) (liii pi code config settings))

(check-set-mode! 'report-failed)
(let ((j '(("foo" . 1) (bar . 2))))
  (check (json-dict-ref j "foo" 0) => 1)
  (check (json-dict-ref j 'foo 0) => 1)
  (check (json-dict-ref j "bar" 0) => 2)
  (check (json-dict-ref j "baz" 99) => 99))

;; 空 JSON 对象在 (liii json) 中解析为 (())，不应触发 assoc 断言错误
(check (json-dict-ref '(()) "foo" 99) => 99)
(check (json-dict-ref '(()) 'foo 7) => 7)
(check (json-dict-ref '(#f) "foo" 7) => 7)

(check-report)

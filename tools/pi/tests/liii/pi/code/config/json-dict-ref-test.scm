;; json-dict-ref
;; 从 JSON 字典对象中按键读取字段值，键可为字符串或符号。
;;
;; 语法
;; ----
;; (json-dict-ref obj key default)
;;
;; 参数
;; ----
;; obj : alist | #f
;; JSON 解析得到的字典对象。
;; key : string | symbol
;; 要读取的键。
;; default : any
;; 键不存在时返回的默认值。
;;
;; 返回值
;; ----
;; any
;; 键对应的值；对象为空或键缺失时返回 default。

(import (liii check) (liii pi code config))

(check-set-mode! 'report-failed)

;; json-dict-ref
;; 从 JSON 字典对象中按符号或字符串键安全读取字段值。
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

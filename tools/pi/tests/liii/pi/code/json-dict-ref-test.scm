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

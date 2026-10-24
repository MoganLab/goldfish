(import (liii check) (liii pi config))

(check-set-mode! 'report-failed)

;; json-dict-ref
;; 从 JSON 字典对象中按符号或字符串键安全读取字段值。
(let ((j '(("foo" . 1) (bar . 2))))
  (check (json-dict-ref j "foo" 0) => 1)
  (check (json-dict-ref j 'foo 0) => 1)
  (check (json-dict-ref j "bar" 0) => 2)
  (check (json-dict-ref j "baz" 99) => 99))

(check-report)

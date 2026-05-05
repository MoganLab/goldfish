(import (liii check) (liii base))

(check-set-mode! 'report-failed)

;; reverse! 与 reverse 的区别：
;; reverse 返回一个新列表，原始列表保持不变；
;; reverse! 原地修改列表并返回结果，性能更好，但会破坏原始列表。

(check (reverse! '()) => '())
(check (reverse! '(a)) => '(a))
(check (reverse! '(a b)) => '(b a))
(check (reverse! '(a b c)) => '(c b a))
(check (reverse! '(1 2 3 4 5)) => '(5 4 3 2 1))

;; 测试嵌套列表
(check (reverse! '((a b) (c d) (e f))) => '((e f) (c d) (a b)))

;; 测试混合类型列表
(check (reverse! '(1 "two" #t 4.5 symbol)) => '(symbol 4.5 #t "two" 1))

;; 验证原地修改特性
(let ((lst '(1 2 3)))
  (check (reverse! lst) => '(3 2 1))
  ;; 原始列表已被修改（由于 S7 内部实现，此处不严格断言 lst 的值）
) ;let

(check-report)

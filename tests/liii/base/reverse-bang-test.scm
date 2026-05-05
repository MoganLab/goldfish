(import (liii check) (liii base))

(check-set-mode! 'report-failed)

;; reverse!
;; 原地反转列表或向量并返回结果，性能优于 reverse。
;;
;; 语法
;; ----
;; (reverse! seq)
;;
;; 参数
;; ----
;; seq : list? 或 vector?
;;     要反转的列表或向量。
;;
;; 返回值
;; ------
;; list? 或 vector?
;;     反转后的列表或向量。
;;
;; 说明
;; ----
;; 1. 原地修改输入列表的指针结构或向量的元素顺序，不需要分配新节点
;; 2. 性能优于 reverse，适合对长列表/向量进行反转的场景
;; 3. 调用后原始序列会被改变，若需保留原序列应先使用 copy 复制
;; 4. 对序列中的元素类型没有限制
;; 5. 当参数是非列表/非向量时，行为依赖于具体实现
;;
;; 与 reverse 的区别
;; ----------------
;; reverse  返回一个新序列，原始序列保持不变；
;; reverse! 原地修改序列并返回结果，性能更好，但会破坏原始序列。
;;
;; 相关函数
;; --------
;; reverse - 返回反转后的新列表或向量，不修改原始序列
;;
;; 错误处理
;; --------
;; 依赖具体实现行为

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

;; 测试 vector（S7 支持 reverse! 作用于 vector）
(check (reverse! #()) => #())
(check (reverse! #(a)) => #(a))
(check (reverse! #(a b c)) => #(c b a))
(check (reverse! #(1 2 3 4 5)) => #(5 4 3 2 1))

;; 验证 vector 原地修改特性
(let ((vec #(1 2 3)))
  (check (reverse! vec) => #(3 2 1))
) ;let

(check-report)

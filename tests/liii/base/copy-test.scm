(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; copy
;; 复制序列或对象。
;;
;; 语法
;; ----
;; (copy obj)
;; (copy source target)
;; (copy source target start end)
;;
;; 参数
;; ----
;; obj - 任意对象
;; source - 源序列（列表、向量、字符串等）
;; target - 目标序列（可选）
;; start - 起始索引（可选，默认为 0）
;; end - 结束索引（可选，默认为序列长度）
;;
;; 返回值
;; ------
;; 复制后的对象或目标序列。
;;
;; 说明
;; ----
;; copy 是 S7 内置函数，但不在 R7RS 标准中定义。
;; 当只提供一个参数时，返回源序列的一个浅拷贝。
;; 当提供 target 时，将 source 的元素复制到 target 中并返回 target。
;; 当提供 start 和 end 时，只复制指定范围的元素。
;;
;; 示例
;; ----
;; (copy '(1 2 3))              => (1 2 3)
;; (copy #(1 2 3))              => #(1 2 3)
;; (copy "abc")                 => "abc"
;; (copy 123)                   => 123


;; 基本复制测试
(check (copy '(1 2 3)) => '(1 2 3))
(check (copy #(1 2 3)) => #(1 2 3))
(check (copy "abc") => "abc")

;; 复制原子对象返回自身
(check (copy 123) => 123)
(check (copy 'a) => 'a)
(check (copy #t) => #t)

;; 复制后对象独立性
(check-false (eq? (copy '(1 2 3)) '(1 2 3)))
(check-false (eq? (copy '(1 2 3)) (copy '(1 2 3))))

;; 浅拷贝特性
(let ((orig '(1 (2 3) 4)))
  (let ((c (copy orig)))
    (check (equal? c orig) => #t)
    (check-false (eq? c orig))
    ;; 子对象共享
    (check (eq? (cadr c) (cadr orig)) => #t)
  ) ;let
) ;let

;; 复制到目标
(let ((target (make-string 3)))
  (copy "abc" target 0 3)
  (check target => "abc")
) ;let

(let ((target (make-vector 3)))
  (copy #(1 2 3) target 0 3)
  (check target => #(1 2 3))
) ;let

;; 空序列
(check (copy '()) => '())
(check (copy "") => "")


(check-report)

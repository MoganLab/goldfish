(import (liii check) (liii json schema equal))

(check-set-mode! 'report-failed)

;; json-equal?
;; 按 JSON 语义比较两个值是否相等。
;;
;; 语法
;; ----
;; (json-equal? one two)
;;
;; 参数
;; ----
;; one : any?
;; 第一个待比较的值。
;;
;; two : any?
;; 第二个待比较的值。
;;
;; 返回值
;; ----
;; boolean
;; 两个值按 JSON 语义相等返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 相等判定遵循 JSON 而非 Scheme：
;; 数字按数值比较（1 与 1.0 相等），
;; 布尔与数字严格区分（#t 与 1 不相等），
;; 对象是无序映射，数组是有序序列。
;;
;; 示例
;; ----
;; (json-equal? 1 1.0) => #t
;; (json-equal? '(("a" . 1) ("b" . 2)) '(("b" . 2) ("a" . 1))) => #t
;;
;; 错误处理
;; ----
;; 无。

;; 标量相等遵循 JSON，而不是 Scheme 的布尔/数值等价。
(check-true (json-equal? 1 1.0))
(check-false (json-equal? #t 1))
(check-false (json-equal? #f 0))
(check-true (json-equal? 'null 'null))
(check-false (json-equal? 'null '()))
(check-true (json-equal? "x" "x"))
(check-false (json-equal? "x" 'x))

;; 对象是无序映射，数组保持有序并递归比较。
(check-true (json-equal? '(("a" . 1) ("b" . 2)) '(("b" . 2) ("a" . 1))))
(check-false (json-equal? '(("a" . 1)) '(("a" . #t))))
(check-true (json-equal? #(1 #(2 3)) #(1.0 #(2 3))))
(check-false (json-equal? #(1 2) #(2 1)))
(check-false (json-equal? #(#t 1) #(1 #t)))
(check-true (json-equal? '(("x" . #(#t 1))) '(("x" . #(#t 1)))))
(check-false (json-equal? '(("x" . #(#t 1))) '(("x" . #(1 #t)))))

;; 长度不同的对象与数组不相等。
(check-false (json-equal? '(("a" . 1)) '(("a" . 1) ("b" . 2))))
(check-false (json-equal? #(1) #(1 2)))

(check-report)

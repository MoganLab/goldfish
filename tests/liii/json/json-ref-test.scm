(import (liii check) (liii json) (liii base) (liii error))

(check-set-mode! 'report-failed)

;; json-ref
;; 按键路径访问 JSON 对象或数组中的值。
;;
;; 语法
;; ----
;; (json-ref json key)
;; (json-ref json key1 key2 ...)
;;
;; 参数
;; ----
;; json : any?
;; JSON 对象、数组或空列表。
;;
;; key : symbol? | string? | integer? | boolean?
;; 用于访问当前层级值的键或索引。
;;
;; 返回值
;; ----
;; any?
;; 返回键路径对应的值；若路径不存在则返回空列表 `()`。
;;
;; 注意
;; ----
;; 空列表 `()` 会被当作“未找到”透传，以支持安全导航。
;;
;; 示例
;; ----
;; (json-ref bob-j 'bob 'age) => 18
;; (json-ref bob-j 'alice) => '()
;;
;; 错误处理
;; ----
;; type-error 当 json 不是 JSON 对象、数组或空列表时。

(define bob-j '((bob (age . 18) (sex . male) (name . "Bob"))))

(check (json-ref bob-j 'bob 'age) => 18)
(check (json-ref bob-j 'bob 'sex) => 'male)
(check (json-ref bob-j 'alice) => '())
(check (json-ref bob-j 'alice 'age) => '())
(check (json-ref bob-j 'bob 'name) => "Bob")

(let ((j '((bob (age . 18) (sex . male)))))
  (check (json-null? (json-ref j 'alice)) => #f)
  (check (null? (json-ref j 'alice)) => #t)
  (check (json-null? (json-ref j 'bob)) => #f)
) ;let

(let ((j '((alice (age . 18) (sex . male)))))
  (check (json-null? (json-ref j 'alice)) => #f)
  (check (null? (json-ref j 'bob)) => #t)
) ;let

(check-catch 'type-error (json-ref "not-a-json" 'key))
(check-catch 'type-error (json-ref 123 'key))

;; symbol 'true/'false 在每一层都会被转换为 #t/#f（'null 保持符号不变）
(check (json-ref '((a . true)) 'a) => #t)
(check (json-ref '((a . false)) 'a) => #f)
(check (json-ref '((a . null)) 'a) => 'null)
(check (json-ref '((a . other)) 'a) => 'other)
(check (json-ref '((a (b . true))) 'a 'b) => #t)
(check (json-ref #(true false null) 0) => #t)
(check (json-ref #(true false null) 1) => #f)
(check (json-ref #(true false null) 2) => 'null)

;; 字符串键与 symbol 键互不匹配（equal? 语义）
(check (json-ref '(("age" . 18)) "age") => 18)
(check (json-ref '(("age" . 18)) 'age) => '())
(check (json-ref '((age . 18)) "age") => '())

;; 数字键按 equal? 匹配（eqv? 语义：1 与 1.0 不相等）
(check (json-ref '((1 . one) (2 . two)) 2) => 'two)
(check (json-ref '((1 . one)) 1.0) => '())

;; 空对象 '(()) 特判：任意深度都返回 '()
(check (json-ref '(()) 'a) => '())
(check (json-ref '(()) 'a 'b) => '())
(check (json-ref '() 'a) => '())
(check (json-ref '() 'a 'b) => '())

;; 数组路径
(check (json-ref #(1 2 3) 0) => 1)
(check (json-ref '((a . #(10 20 30))) 'a 1) => 20)
(check (json-ref #(((x . 1))) 0 'x) => 1)

;; 数组索引错误：越界抛 out-of-range，非整数索引抛 wrong-type-arg
(check-catch 'out-of-range (json-ref #(1 2) 5))
(check-catch 'out-of-range (json-ref #(1 2) -1))
(check-catch 'wrong-type-arg (json-ref #(1 2) 'a))
(check-catch 'wrong-type-arg (json-ref #(1 2) 1.5))

;; 中间层不是对象/数组时抛 type-error（每层都做结构校验）
(check-catch 'type-error (json-ref '((a . 1)) 'a 'b))
(check-catch 'type-error (json-ref '((a . "s")) 'a 'b))

;; 非键路径上的值不做结构校验，直接返回
(check (json-ref '((a . 1)) 'a) => 1)
(check (json-ref '((a . "s")) 'a) => "s")

;; 非 pair 列表/非真列表不是合法 JSON 结构
(check-catch 'type-error (json-ref '((a . 1) b) 'a))
(check-catch 'type-error (json-ref '(1 . 2) 'a))

;; 未找到键时返回 '()，原对象不受影响
(let ((j '((a . 1))))
  (check (json-ref j 'b) => '())
  (check j => '((a . 1)))
) ;let

(check-report)

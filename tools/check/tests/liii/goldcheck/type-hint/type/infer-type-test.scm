;; infer-type
;; 推导表达式的分析类型：字面量、局部绑定、调用返回值。
;;
;; 语法
;; ----
;; (infer-type expr env locals)
;;
;; 参数
;; ----
;; expr : 表达式
;; env : alist      类型环境
;; locals : alist   局部类型绑定
;;
;; 返回值
;; ------
;; symbol、联合类型或 #f
;; 类型符号（如 string?）或 (or #f T)；无法确定返回 #f（unknown）。

(import (scheme base) (liii check) (liii goldcheck type-hint type))

(check-set-mode! 'report-failed)

(define env
  (list (cons 'my-assoc '((any? alist? -> (or #f pair?))))
        (cons 'my-len '((string? -> integer?)))
       ) ;list
) ;define

;; 1. 字面量
(check (infer-type 42 env '()) => 'integer?)
(check (infer-type "s" env '()) => 'string?)
(check (infer-type #t env '()) => 'boolean?)
(check (infer-type #\a env '()) => 'char?)
;; #f 字面量是 unknown（不是 boolean?），避免假阳性
(check (infer-type #f env '()) => #f)

;; 2. 局部绑定
(check (infer-type 'x env '((x . string?))) => 'string?)
(check (infer-type 'x env '()) => #f)

;; 3. 调用返回值
(check (infer-type '(my-len "s") env '()) => 'integer?)
(check (infer-type '(my-assoc 'k '()) env '()) => '(or #f pair?))

;; 4. 内联 assq 取值惯用法视为存在性已保证 -> unknown
(check (infer-type '(cdr (assq 'k a)) env '()) => #f)

;; 5. quote 形式（s7 read 后为 #_quote 标记，签名数据同此形态）
(check (infer-type ''sym env '()) => 'symbol?)
(check (infer-type ''() env '()) => 'null?)
(check (infer-type ''sym2 env '()) => 'symbol?)

(check-report)

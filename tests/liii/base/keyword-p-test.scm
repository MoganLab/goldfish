(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; keyword?
;; 判断对象是否为关键字。
;;
;; 语法
;; ----
;; (keyword? obj)
;;
;; 参数
;; ----
;; obj - 任意对象
;;
;; 返回值
;; ------
;; boolean
;; 如果 obj 是关键字（如 :key），返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; keyword? 是 S7 内置函数，但不在 R7RS 标准中定义。
;; 在 S7 中，keyword 是一种特殊的 symbol。
;; 因此 (keyword? :x) 和 (symbol? :x) 都返回 #t。
;;
;; 示例
;; ----
;; (keyword? :test)     => #t
;; (keyword? 'symbol)   => #f
;; (keyword? "string")  => #f
;; (keyword? 123)       => #f


(check (keyword? :test) => #t)
(check (keyword? 'symbol) => #f)
(check (keyword? "string") => #f)
(check (keyword? 123) => #f)
(check (keyword? #t) => #f)
(check (keyword? '()) => #f)


;; keyword 同时也是 symbol
(check (and (keyword? :x) (symbol? :x)) => #t)


(check-report)

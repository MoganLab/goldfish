(import (liii check)
        (liii goldfmt-record))

;; env?
;; 判断一个值是否为 env 记录。
;;
;; 语法
;; ----
;; (env? value)
;;
;; 参数
;; ----
;; value : any?
;; 待检查的 Scheme 值。
;;
;; 返回值
;; ------
;; boolean?
;; 返回 #t 表示 value 是 env 记录；否则返回 #f。
;;
;; 说明
;; ----
;; env 用于表示带括号结构，例如 `(define x 1)`。
;; atom、字符串、数字、布尔值、vector 等都不是 env。

;; 测试 make-env 返回的对象是 env
(check (env? (make-env)) => #t)
(check (env? (make-env :tag-name "test")) => #t)
(check (env? (make-env :depth 1 :indent 2)) => #t)

;; 测试非 env 对象返回 #f
(check (env? "string") => #f)
(check (env? 123) => #f)
(check (env? '()) => #f)
(check (env? (vector)) => #f)
(check (env? #t) => #f)
(check (env? #f) => #f)

(check-report)

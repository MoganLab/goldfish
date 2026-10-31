(import (liii check) (liii json schema cache))

(check-set-mode! 'report-failed)

;; make-reference-cache
;; 创建引用解析缓存。
;;
;; 语法
;; ----
;; (make-reference-cache)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; cache
;; 引用缓存记录。
;;
;; 注意
;; ----
;; 缓存按“引用 + 根资源 + 动态作用域”三元组命中，
;; 因此不同资源的同名引用不会互相污染。
;;
;; 示例
;; ----
;; (reference-cache-lookup (make-reference-cache) "child.json" #t '()) => #f
;;
;; 错误处理
;; ----
;; 无。

(define cache (make-reference-cache))

(check-true (vector? cache))
(check (vector-length cache) => 1)
(check (reference-cache-lookup cache "child.json" #t '()) => #f)

(check-report)

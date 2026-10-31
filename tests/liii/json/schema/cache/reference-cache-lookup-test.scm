(import (liii check) (liii json schema cache))

(check-set-mode! 'report-failed)

;; reference-cache-lookup
;; 按引用、根资源与作用域查找缓存条目。
;;
;; 语法
;; ----
;; (reference-cache-lookup cache reference root scope)
;;
;; 参数
;; ----
;; cache : cache
;; 由 make-reference-cache 创建的缓存记录。
;;
;; reference : string
;; 引用字符串。
;;
;; root : any?
;; 根资源。
;;
;; scope : list
;; 动态作用域。
;;
;; 返回值
;; ----
;; list | boolean
;; 命中时返回缓存条目，未命中时返回 #f。
;;
;; 注意
;; ----
;; 缓存的值本身可能是 #f（假 Schema），此时条目仍然命中，
;; 调用方需要用条目本身而不是值来判断是否命中。
;;
;; 示例
;; ----
;; (reference-cache-lookup cache "child.json" root-a '()) => #f
;;
;; 错误处理
;; ----
;; 无。

(define root-a '(("$id" . "https://example.com/a/root")))
(define root-b '(("$id" . "https://example.com/b/root")))

(define cache (make-reference-cache))

;; 未命中
(check-false (reference-cache-lookup cache "child.json" root-a '()))

;; 写入后命中
(reference-cache-put! cache "child.json" root-a '() 'VALUE)
(define entry (reference-cache-lookup cache "child.json" root-a '()))
(check (if entry #t #f) => #t)
(check (cadddr entry) => 'VALUE)

;; 根资源不同不命中
(check-false (reference-cache-lookup cache "child.json" root-b '()))

;; 作用域不同不命中
(check-false (reference-cache-lookup cache "child.json" root-a (list root-a)))

;; 引用不同不命中
(check-false (reference-cache-lookup cache "other.json" root-a '()))

;; 缓存的假 Schema 仍然算命中
(define false-cache (make-reference-cache))
(reference-cache-put! false-cache "false.json" root-a '() #f)
(define false-entry (reference-cache-lookup false-cache "false.json" root-a '()))
(check (if false-entry #t #f) => #t)
(check (cadddr false-entry) => #f)

(check-report)

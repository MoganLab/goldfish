(import (liii check) (liii json schema cache))

(check-set-mode! 'report-failed)

;; reference-cache-key-matches?
;; 判断缓存条目是否与给定的引用、根资源与作用域匹配。
;;
;; 语法
;; ----
;; (reference-cache-key-matches? entry reference root scope)
;;
;; 参数
;; ----
;; entry : list
;; 缓存条目，形如 (引用 根资源 作用域 值)。
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
;; boolean
;; 三者都匹配返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 根资源与作用域按标识比较，不同资源不会共享同一条目；
;; 条目结构异常时返回 #f，不会中断校验。
;;
;; 示例
;; ----
;; (reference-cache-key-matches? '("a" #t '() #f) "a" #t '()) => #t
;;
;; 错误处理
;; ----
;; 无。

(define root-a '(("$id" . "https://example.com/a/root")))
(define root-b '(("$id" . "https://example.com/b/root")))

(define key-a (list "child.json" root-a '()))
(define key-b (list "child.json" root-b '()))
(define key-scope (list "child.json" root-a (list root-a)))

(check-true (reference-cache-key-matches? key-a "child.json" root-a '()))
(check-false (reference-cache-key-matches? key-a "child.json" root-b '()))
(check-false (reference-cache-key-matches? key-a "child.json" root-a (list root-a)))
(check-false (reference-cache-key-matches? key-b "child.json" root-a '()))

;; 引用不同则不匹配
(check-false (reference-cache-key-matches? key-a "other.json" root-a '()))

;; 结构异常的条目不会中断校验
(check-false (reference-cache-key-matches? '() "child.json" root-a '()))
(check-false (reference-cache-key-matches? '(1 2 3) "child.json" root-a '()))

(check-report)

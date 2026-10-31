(import (liii check) (liii json schema cache))

(check-set-mode! 'report-failed)

;; reference-cache-put!
;; 写入一条引用解析缓存。
;;
;; 语法
;; ----
;; (reference-cache-put! cache reference root scope value)
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
;; value : any?
;; 缓存的值。
;;
;; 返回值
;; ----
;; any?
;; 写入的值。
;;
;; 注意
;; ----
;; 新条目插在最前面，查找时优先命中最近写入的条目。
;;
;; 示例
;; ----
;; (reference-cache-put! cache "child.json" root-a '() 'VALUE) => 'VALUE
;;
;; 错误处理
;; ----
;; 无。

(define root-a '(("$id" . "https://example.com/a/root")))

(define cache (make-reference-cache))

(check (reference-cache-put! cache "child.json" root-a '() 'VALUE) => 'VALUE)
(check (cadddr (reference-cache-lookup cache "child.json" root-a '())) => 'VALUE)

;; 同一键重复写入时，最新的值生效。
(reference-cache-put! cache "child.json" root-a '() 'NEWER)
(check (cadddr (reference-cache-lookup cache "child.json" root-a '())) => 'NEWER)

;; 不同键互不影响。
(reference-cache-put! cache "other.json" root-a '() 'OTHER)
(check (cadddr (reference-cache-lookup cache "child.json" root-a '())) => 'NEWER)
(check (cadddr (reference-cache-lookup cache "other.json" root-a '())) => 'OTHER)

(check-report)

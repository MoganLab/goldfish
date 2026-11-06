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
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema) (liii json schema cache))

(check-set-mode! 'report-failed)

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

(define false-entry
  (reference-cache-lookup false-cache "false.json" root-a '())
) ;define
(check (if false-entry #t #f) => #t)
(check (cadddr false-entry) => #f)

;; 通过 retrieve 获得的资源不缓存，每次解析都会重新调用 retrieve

(define retrieve-calls 0)

(define remote-resource '(("type" . "integer")))

(define (counting-retrieve uri)
  (set! retrieve-calls (+ retrieve-calls 1))
  remote-resource
) ;define

(define remote-validator
  (make-json-schema-validator '(("$ref" . "urn:counted"))
    `((retrieve . ,counting-retrieve) (check-schema? . #f))
  ) ;make-json-schema-validator
) ;define
(check-true (json-schema-validator-valid? remote-validator 1))
(check-true (json-schema-validator-valid? remote-validator 1))
(check-true (json-schema-validator-valid? remote-validator 1))
(check retrieve-calls => 3)

;; registry 中的资源会被缓存，重复解析命中同一条目

(define cached-registry
  (json-schema-registry-add (make-json-schema-registry)
    "urn:cached"
    '(("type" . "integer"))
  ) ;json-schema-registry-add
) ;define

(define registry-validator
  (make-json-schema-validator '(("$ref" . "urn:cached"))
    `((registry . ,cached-registry) (check-schema? . #f))
  ) ;make-json-schema-validator
) ;define
(check-true (json-schema-validator-valid? registry-validator 1))
(check-true (json-schema-validator-valid? registry-validator 1))
(check-true (json-schema-validator-valid? registry-validator 1))

;; 不同根资源下的同名相对引用互不污染

(define base-a
  '(("$id" . "https://example.com/a/root")
    ("$defs" ("x" ("$id" . "x.json") ("const" . 1)))
    ("$ref" . "x.json"))
) ;define

(define base-b
  '(("$id" . "https://example.com/b/root")
    ("$defs" ("x" ("$id" . "x.json") ("const" . 2)))
    ("$ref" . "x.json"))
) ;define
(check-true (json-schema-valid? 1 base-a '((check-schema? . #f))))
(check-false (json-schema-valid? 2 base-a '((check-schema? . #f))))
(check-true (json-schema-valid? 2 base-b '((check-schema? . #f))))
(check-false (json-schema-valid? 1 base-b '((check-schema? . #f))))

;; 两个独立校验器不共享缓存条目

(define shared-schema '(("$defs" ("x" ("const" . 7))) ("$ref" . "#/$defs/x")))

(define validator-one
  (make-json-schema-validator shared-schema '((check-schema? . #f)))
) ;define

(define validator-two
  (make-json-schema-validator shared-schema '((check-schema? . #f)))
) ;define
(check-true (json-schema-validator-valid? validator-one 7))
(check-true (json-schema-validator-valid? validator-two 7))
(check-false (json-schema-validator-valid? validator-one 8))

(check-report)

;; (liii json schema cache) 模块介绍与用法示例
;;
;; (liii json schema cache) 实现引用解析缓存：按“引用 + 根资源 + 动态作用域”
;; 三元组命中，避免重复解析同一引用，同时保证不同资源互不污染。
;;
;; 下方第 10 组断言（valid? 短路）暂存于此，
;; 后续拆分到 keywords 库的按函数测试文件。

(import (liii json schema) (liii json schema cache) (liii check) (scheme base))

(check-set-mode! 'report-failed)

;; ==== 模块概述 ====
;;
;; - make-reference-cache            - 创建引用缓存
;; - reference-cache-key-matches?    - 条目是否与引用/根资源/作用域匹配
;; - reference-cache-lookup          - 查找缓存条目
;; - reference-cache-put!             - 写入缓存条目
;;
;; ==== 如何查看函数的文档和用例 ====
;;
;; 查看模块文档：
;;   gf doc liii/json/schema/cache
;;
;; 查看具体函数文档：
;;   gf doc "reference-cache-lookup"
;; 6. A resource obtained through retrieve is never cached, so the
;;    retrieve function keeps being called for every resolution.

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

;; 7. A registry resource is cached and resolves to the same target.

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

;; 8. The same relative reference under two different base resources
;;    stays separate, so the cached entry never crosses resources.

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

;; 9. Two independent validators do not share cached entries.

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

;; 10. valid? stops at the first failing keyword and does not evaluate
;;     the later ones.

(define probe-calls 0)

(define (counting-format value)
  (set! probe-calls (+ probe-calls 1))
  #t
) ;define

(define short-circuit-schema
  '(("type" . "string") ("format" . "probe") ("minLength" . 10))
) ;define

(define short-circuit-validator
  (make-json-schema-validator short-circuit-schema
    `((formats probe) (format-checkers . ,(list (cons 'probe counting-format))))
  ) ;make-json-schema-validator
) ;define
(check-false (json-schema-validator-valid? short-circuit-validator 1))
(check probe-calls => 0)

(check-report)

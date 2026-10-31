(import (liii check) (liii json schema meta))

(check-set-mode! 'report-failed)

;; member 返回的是匹配位置的尾部（真值但不严格等于 #t），这里归一化为布尔。
(define (truthy? value)
  (not (eq? value #f))
) ;define

;; json-schema-meta-uris
;; 返回内嵌元 Schema 资源的 URI 列表。
;;
;; 语法
;; ----
;; (json-schema-meta-uris)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; list
;; 2020-12 元 Schema 及其各词汇表的 URI 列表。
;;
;; 注意
;; ----
;; 元 Schema 与词汇表全部内嵌在库中，检查过程不做任何 I/O。
;;
;; 示例
;; ----
;; (length (json-schema-meta-uris)) => 9
;;
;; 错误处理
;; ----
;; 无。

(check (length (json-schema-meta-uris)) => 9)

(check-true
  (truthy? (member "https://json-schema.org/draft/2020-12/schema"
                    (json-schema-meta-uris)))
) ;check-true
(check-true
  (truthy? (member "https://json-schema.org/draft/2020-12/meta/core"
                    (json-schema-meta-uris)))
) ;check-true
(check-true
  (truthy? (member "https://json-schema.org/draft/2020-12/meta/applicator"
                    (json-schema-meta-uris)))
) ;check-true
(check-true
  (truthy? (member "https://json-schema.org/draft/2020-12/meta/validation"
                    (json-schema-meta-uris)))
) ;check-true
(check-true
  (truthy? (member "https://json-schema.org/draft/2020-12/meta/unevaluated"
                    (json-schema-meta-uris)))
) ;check-true
(check-true
  (truthy? (member "https://json-schema.org/draft/2020-12/meta/format-annotation"
                    (json-schema-meta-uris)))
) ;check-true
(check-true
  (truthy? (member "https://json-schema.org/draft/2020-12/meta/format-assertion"
                    (json-schema-meta-uris)))
) ;check-true
(check-true
  (truthy? (member "https://json-schema.org/draft/2020-12/meta/content"
                    (json-schema-meta-uris)))
) ;check-true
(check-true
  (truthy? (member "https://json-schema.org/draft/2020-12/meta/meta-data"
                    (json-schema-meta-uris)))
) ;check-true

(check-report)

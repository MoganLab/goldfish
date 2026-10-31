(import (liii check) (liii json schema meta) (liii json schema data))

(check-set-mode! 'report-failed)

;; json-schema-meta-resource
;; 按 URI 读取内嵌的元 Schema 资源。
;;
;; 语法
;; ----
;; (json-schema-meta-resource uri)
;;
;; 参数
;; ----
;; uri : string
;; 元 Schema 或词汇表的 URI。
;;
;; 返回值
;; ----
;; any?
;; 解析后的 JSON Schema 对象。
;;
;; 注意
;; ----
;; 资源全部内嵌在库中，读取过程不做任何 I/O。
;;
;; 示例
;; ----
;; (object-ref (json-schema-meta-resource
;;               "https://json-schema.org/draft/2020-12/schema")
;;             "$id" #f)
;; => "https://json-schema.org/draft/2020-12/schema"
;;
;; 错误处理
;; ----
;; json-schema-unresolvable 当 URI 不是内嵌资源时。

;; 每个内嵌资源都可寻址，且 $id 与 URI 一致。
(for-each
  (lambda (uri)
    (let ((resource (json-schema-meta-resource uri)))
      (check-true (pair? resource))
      (check (object-ref resource "$id" #f) => uri)
    ) ;let
  ) ;lambda
  (json-schema-meta-uris)
) ;for-each

;; 主元 Schema 声明了词汇表。
(check-true
  (pair? (object-ref (json-schema-meta-resource "https://json-schema.org/draft/2020-12/schema")
                     "$vocabulary"
                     #f
                   ) ;object-ref
            ) ;pair?
) ;check-true

(check-catch 'json-schema-unresolvable
  (json-schema-meta-resource "urn:missing")
) ;check-catch

(check-report)

(import (liii check) (liii json schema dialect))

(check-set-mode! 'report-failed)

;; draft-for
;; 返回 Schema 使用的 JSON Schema 方言。
;;
;; 语法
;; ----
;; (draft-for schema)
;;
;; 参数
;; ----
;; schema : any?
;; JSON Schema，可以是对象或布尔值。
;;
;; 返回值
;; ----
;; symbol
;; 方言符号，目前只可能是 draft2020-12。
;;
;; 注意
;; ----
;; 未声明 $schema 时默认按 2020-12 处理；
;; $schema 声明了其他草稿时抛出 json-schema-unknown-dialect。
;;
;; 示例
;; ----
;; (draft-for '(("type" . "integer"))) => 'draft2020-12
;;
;; 错误处理
;; ----
;; json-schema-unknown-dialect 当 $schema 不是 2020-12 时。
;; json-schema-schema-error 当 $schema 不是字符串时。

(check (draft-for #t) => 'draft2020-12)
(check (draft-for '(())) => 'draft2020-12)
(check (draft-for '(("type" . "integer"))) => 'draft2020-12)
(check (draft-for '(("$schema" . "https://json-schema.org/draft/2020-12/schema")))
  =>
  'draft2020-12
) ;check
(check (draft-for '(("$schema"
                     . "https://json-schema.org/draft/2020-12/schema#")))
  =>
  'draft2020-12
) ;check

;; 其他草稿被拒绝
(for-each
  (lambda (uri)
    (check-catch 'json-schema-unknown-dialect (draft-for `(("$schema" . ,uri))))
  ) ;lambda
  '("https://json-schema.org/draft/2019-09/schema"
    "http://json-schema.org/draft-07/schema#"
    "http://json-schema.org/draft-06/schema#"
    "http://json-schema.org/draft-04/schema#"
    "http://json-schema.org/draft-03/schema#" "urn:example:unknown")
) ;for-each

;; $schema 必须是字符串
(for-each
  (lambda (value)
    (check-catch 'json-schema-schema-error (draft-for `(("$schema" . ,value))))
  ) ;lambda
  '(#f 42 ())
) ;for-each

(check-report)

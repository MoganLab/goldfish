;; json-schema-registry-add
;; 向资源登记表中登记一个资源。
;;
;; 语法
;; ----
;; (json-schema-registry-add registry uri schema)
;;
;; 参数
;; ----
;; registry : registry
;; 资源登记表。
;;
;; uri : string
;; 资源的 URI。
;;
;; schema : any?
;; 资源对应的 Schema，可以是 JSON 对象或布尔值。
;;
;; 返回值
;; ----
;; registry
;; 登记了资源后的新登记表。
;;
;; 注意
;; ----
;; 登记表不可变，原登记表不会被修改；
;; 同一个 URI 不能重复登记。
;;
;; 错误处理
;; ----
;; registry 不是登记表或 uri 不是字符串时抛出类型错误；
;; uri 重复登记时抛出 json-schema-options-error；
;; schema 既不是对象也不是布尔值时抛出 json-schema-schema-error。

(import (liii check) (liii json schema registry) (liii json schema model))

(check-set-mode! 'report-failed)

(define registry (make-json-schema-registry))

;; 登记后原登记表保持不变。

(define next-registry
  (json-schema-registry-add registry "urn:integer" '(("type" . "integer")))
) ;define

(check (registry-resources registry) => '())
(check (length (registry-resources next-registry)) => 1)
(check (assoc "urn:integer" (registry-resources next-registry))
  =>
  '("urn:integer" ("type" . "integer"))
) ;check

;; 布尔 Schema 也可以登记。

(define boolean-registry (json-schema-registry-add registry "urn:false" #f))
(check (assoc "urn:false" (registry-resources boolean-registry))
  =>
  '("urn:false" . #f)
) ;check

;; 重复登记同一个 URI 会报错。

(check-catch 'json-schema-options-error
  (json-schema-registry-add next-registry "urn:integer" '(("type" . "integer")))
) ;check-catch

;; 参数类型错误。

(check-catch 'type-error
  (json-schema-registry-add '() "urn:integer" '(("type" . "integer")))
) ;check-catch
(check-catch 'type-error
  (json-schema-registry-add registry 42 '(("type" . "integer")))
) ;check-catch

;; 资源必须是对象或布尔值。

(check-catch 'json-schema-schema-error
  (json-schema-registry-add registry "urn:bad" '(1 2 3))
) ;check-catch

(check-report)

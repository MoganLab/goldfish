(import (liii check) (liii json schema) (liii json schema error) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-errors
;; 收集实例不满足 Schema 时的全部错误。
;;
;; 语法
;; ----
;; (json-schema-errors instance schema . options)
;;
;; 参数
;; ----
;; instance : any?
;; 待校验的 JSON 数据。
;;
;; schema : any?
;; JSON Schema，可以是对象或布尔值。
;;
;; options : alist?
;; 可选选项表，详见 make-json-schema-validator。
;;
;; 返回值
;; ----
;; list
;; 错误对象列表，按发现顺序排列；校验通过时返回空列表。
;;
;; 注意
;; ----
;; 错误对象是 <json-schema-error> 记录，访问器见 json-schema-error-kind。
;; max-errors 选项可以限制收集的错误数量。
;;
;; 示例
;; ----
;; (length (json-schema-errors '(("name" . 1)) schema)) => 1
;;
;; 错误处理
;; ----
;; json-schema-options-error 当选项非法时。
;; json-schema-schema-error 当 Schema 非法时。

(define schema
  '(("type" . "object")
    ("properties" ("name" ("type" . "string")))
    ("required" . #("name")))
) ;define

(check (length (json-schema-errors '(("name" . 1)) schema)) => 1)
(check (length (json-schema-errors '(("name" . "Liii")) schema)) => 0)

;; max-errors 限制错误列表长度
(define person-schema
  '(("type" . "object")
    ("properties"
     ("name" ("type" . "string"))
     ("age" ("type" . "integer") ("minimum" . 0)))
    ("required" . #("name" "age"))
    ("additionalProperties" . #f))
) ;define
(check (length (json-schema-errors '(("name" . 7)) person-schema)) => 2)
(check (length (json-schema-errors '(("name" . 7)) person-schema '((max-errors . 1))))
  =>
  1
) ;check

;; 错误对象是可识别的记录
(let ((error (car (json-schema-errors '(("name" . 1)) schema))))
  (check-true (json-schema-error? error))
  (check (json-schema-error-kind error) => 'validation)
  (check (json-schema-error-keyword error) => "type")
  (check (json-schema-error-path error) => '("name"))
  (check (json-schema-error-absolute-path error) => '("name"))
  (check (json-schema-error-json-path error) => "$.name")
) ;let

(check-report)

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


;; 错误对象携带实例路径与 Schema 路径。

(let* ((person-schema
         '(("type" . "object")
           ("properties"
            ("name" ("type" . "string"))
            ("age" ("type" . "integer") ("minimum" . 0)))
           ("required" . #("name" "age"))
           ("additionalProperties" . #f)))
       (errors (json-schema-errors '(("name" . 7)) person-schema))
      ) ;errors
  (check-true (= (length errors) 2))
  (check (json-schema-error-kind (car errors)) => 'validation)
  (check (json-schema-error-keyword (car errors)) => "type")
  (check (json-schema-error-path (car errors)) => '("name"))
) ;let*

;; 错误对象记录嵌套路径。

(define traversal-schema
  '(("properties" ("outer" ("items" ("properties" ("inner" . #f))))))
) ;define

(define traversal-instance '(("outer" . #((("inner" . 1))))))
(let ((error (car (json-schema-errors traversal-instance traversal-schema))))
  (check (json-schema-error-path error) => '("outer" 0 "inner"))
  (check (json-schema-error-schema-path error)
    =>
    '("properties" "outer" "items" "properties" "inner")
  ) ;check
  (check (json-schema-error-json-path error) => "$.outer[0].inner")
) ;let

;; 错误对象的字段稳定，包括上下文与原因。

(let* ((err (car (json-schema-errors 0 person-schema)))) ;err
  (check-true (json-schema-error? err))
  (check (json-schema-error-kind err) => 'validation)
  (check (json-schema-error-keyword err) => "type")
  (check (json-schema-error-path err) => '())
) ;let*


(check-report)

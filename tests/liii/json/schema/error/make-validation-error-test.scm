(import (liii check) (liii json schema error) (liii json schema data))

(check-set-mode! 'report-failed)

;; make-validation-error
;; 创建错误对象。
;;
;; 语法
;; ----
;; (make-validation-error kind message keyword keyword-value instance schema path schema-path context cause)
;;
;; 参数
;; ----
;; kind : symbol
;; 错误类别，例如 'validation。
;;
;; 返回值
;; ----
;; ['message : string', '人类可读的消息。']
;;
;; 注意
;; ----
;; keyword : string | boolean
;; 触发错误的关键字，没有时为 #f。
;; keyword-value : any?
;; 关键字的取值。
;; instance : any?
;; 触发位置的实例值。
;; schema : any?
;; 触发位置的 Schema。
;; path : list
;; 实例路径。
;; schema-path : list
;; Schema 路径。
;; context : list
;; 组合关键字的子错误列表。
;; cause : any?
;; 底层原因，没有时为 #f。
;; json-schema-error
;; 字段顺序固定，路径与 Schema 路径按从根到当前位置排列。
;; 记录字段可通过 json-schema-error-kind 等访问器读取。
;;
;; 示例
;; ----
;; (make-validation-error 'validation "msg" "type" "integer" 1 #t '() '() '() #f)
;;
;; 错误处理
;; ----
;; 无。

(define error
  (make-validation-error 'validation
    "child"
    "type"
    "string"
    1
    #t
    '("x")
    '("type")
    '()
    #f
  ) ;make-validation-error
) ;define

(check-true (json-schema-error? error))
(check (json-schema-error-kind error) => 'validation)
(check (json-schema-error-message error) => "child")
(check (json-schema-error-keyword error) => "type")
(check (json-schema-error-keyword-value error) => "string")
(check (json-schema-error-instance error) => 1)
(check (json-schema-error-schema error) => #t)
(check (json-schema-error-path error) => '("x"))
(check (json-schema-error-schema-path error) => '("type"))
(check (json-schema-error-context error) => '())
(check-false (json-schema-error-cause error))

(check-report)

(import (liii check) (liii json schema error) (liii json schema data))

(check-set-mode! 'report-failed)

;; validation-error
;; 创建实例校验失败错误。
;;
;; 语法
;; ----
;; (validation-error message keyword keyword-value instance schema path schema-path context cause)
;;
;; 参数
;; ----
;; message : string
;; 人类可读的消息。
;;
;; 返回值
;; ----
;; ['keyword : string | boolean', '触发错误的关键字，没有时为 #f。']
;;
;; 注意
;; ----
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
;; 错误类别固定为 'validation。
;; 关键字存在时，Schema 路径会自动追加该关键字。
;;
;; 示例
;; ----
;; (json-schema-error-kind (validation-error "msg" "type" "integer" 1 #t '() '() '() #f)) => 'validation
;;
;; 错误处理
;; ----
;; 无。

(define error
  (validation-error "msg" "type" "integer" 1 #t '("x") '("properties") '() #f)
) ;define

(check-true (json-schema-error? error))
(check (json-schema-error-kind error) => 'validation)
(check (json-schema-error-message error) => "msg")
(check (json-schema-error-keyword error) => "type")
(check (json-schema-error-keyword-value error) => "integer")
(check (json-schema-error-instance error) => 1)
(check (json-schema-error-path error) => '("x"))

;; 关键字存在时 Schema 路径追加该关键字。
(check (json-schema-error-schema-path error) => '("properties" "type"))

;; 没有关键字时 Schema 路径保持原样。
(check (json-schema-error-schema-path (validation-error "msg" #f #f 1 #t '("x") '("properties") '() #f)
       ) ;json-schema-error-schema-path
  =>
  '("properties")
) ;check

(check-report)

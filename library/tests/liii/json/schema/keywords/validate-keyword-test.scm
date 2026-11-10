;; validate-keyword
;; 按关键字类别分派校验，并返回错误列表。
;;
;; 语法
;; ----
;; (validate-keyword validator keyword value instance schema path schema-path
;;   root descend valid?)
;;
;; 参数
;; ----
;; validator : validator
;; 当前校验器。
;;
;; keyword : string
;; 关键字名称。
;;
;; value : any?
;; 关键字在 Schema 中的取值。
;;
;; instance : any?
;; 待校验的实例。
;;
;; schema : any?
;; 实例所属的 Schema 对象。
;;
;; path : list
;; 实例在根资源中的路径段列表。
;;
;; schema-path : list
;; 当前 Schema 在根资源中的路径段列表。
;;
;; root : any?
;; 根资源。
;;
;; descend : procedure
;; 下钻到子 Schema 的过程。
;;
;; valid? : procedure
;; 判断实例是否满足子 Schema 的过程。
;;
;; 返回值
;; ----
;; list
;; 校验失败时返回错误列表，通过时返回空列表。
;;
;; 注意
;; ----
;; 基础断言类关键字交给 validate-basic，
;; 对象类交给 validate-object，数组类交给 validate-array，
;; 组合类交给 validate-combo。
;;
;; $ref 与 $dynamicRef 交给引用解析处理，
;; format 交给 validate-format，
;; 未知关键字直接忽略。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema keywords)
  (liii json schema model)
) ;import

(check-set-mode! 'report-failed)

(define validator (make-validator '(("type" . "integer")) '()))

(define (descend child-instance child-schema path schema-path root)
  '()
) ;define

(define (valid? child-instance child-schema root)
  #t
) ;define

(define (count-errors keyword value instance schema)
  (length (validate-keyword validator
            keyword
            value
            instance
            schema
            '()
            '()
            #f
            descend
            valid?
          ) ;validate-keyword
  ) ;length
) ;define

;; 基础断言类关键字。

(check (count-errors "type" "integer" 1 '(("type" . "integer"))) => 0)
(check (count-errors "type" "integer" "x" '(("type" . "integer"))) => 1)
(check (count-errors "minimum" 2 1 '(("minimum" . 2))) => 1)

;; 对象类关键字。

(check (count-errors "required" #("a") '(("b" . 1)) '(("required" . #("a"))))
  =>
  1
) ;check

;; 数组类关键字。

(check (count-errors "minItems" 2 #(1) '(("minItems" . 2))) => 1)

;; 组合类关键字。

(check
  (count-errors "not" '(("type" . "integer")) 1 '(("not" ("type" . "integer"))))
  =>
  1
) ;check

;; format 关键字需要 formats 选项开启，默认只作为注解。

(check (count-errors "format" "email" "2962" '(("format" . "email"))) => 0)

;; 未知关键字直接忽略。

(check (count-errors "x-unknown" 1 1 '(("x-unknown" . 1))) => 0)

;; 组合类关键字按 if/then/else 选择分支。

(check-false (json-schema-valid? 1 '(("if" . #t) ("then" . #f) ("else" . #t))))
(check-true (json-schema-valid? 1 '(("if" . #f) ("then" . #f) ("else" . #t))))
(check-true (json-schema-valid? 1 '(("if" . #f))))
(check-true
  (json-schema-valid? 1 '(("if" ("type" . "string"))))
) ;check-true

;; 条件分支失败时错误记录携带关键字与 Schema 路径。

(define conditional-schema
  '(("if" ("type" . "integer")) ("else" ("type" . "number")))
) ;define

(define error (car (json-schema-errors "x" conditional-schema)))
(check (json-schema-error-keyword error) => "type")
(check (json-schema-error-schema-path error) => '("else" "type"))

(check-report)

(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-context
;; 读取组合关键字的子错误上下文。
;;
;; 语法
;; ----
;; (json-schema-error-context err)
;;
;; 参数
;; ----
;; err : json-schema-error
;; 错误对象。
;;
;; 返回值
;; ----
;; list
;;
;; 注意
;; ----
;; 组合关键字（如 anyOf、oneOf）失败时给出各分支的子错误列表；其他情况为空列表。
;; 上下文用于区分“分支本身失败”与“组合关键字失败”。
;;
;; 示例
;; ----
;; (json-schema-error-context (car (json-schema-errors 1 '(("type" . "integer"))))) => '()
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check (json-schema-error-context err) => '())

(let* ((errors (json-schema-errors 1 '(("anyOf"
                                        . #((("type" . "string"))
                                            (("minimum" . 2))
                                          ) ;#
                                       )))
       ) ;errors
       (parent (car errors))
       (children (json-schema-error-context parent))
      ) ;
  (check (json-schema-error-keyword parent) => "anyOf")
  (check (length children) => 2)
  (check (json-schema-error-keyword (cadr children)) => "minimum")
) ;let*

(check-report)

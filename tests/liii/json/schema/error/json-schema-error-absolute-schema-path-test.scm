(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-absolute-schema-path
;; 读取从根开始的 Schema 路径。
;;
;; 语法
;; ----
;; (json-schema-error-absolute-schema-path err)
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
;; 从根开始的完整 Schema 路径。
;; 当前实现与 json-schema-error-schema-path 相同。
;;
;; 示例
;; ----
;; (json-schema-error-absolute-schema-path (car (json-schema-errors 1 '(("type" . "integer"))))) => '("type")
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check (json-schema-error-absolute-schema-path err) => '("type"))

(define nested
  (car
    (json-schema-errors '(("rows" . #((("name" . 7)))))
      '(("properties"
         ("rows" ("items" ("properties" ("name" ("type" . "string")))))))
    ) ;json-schema-errors
  ) ;car
) ;define

(check (json-schema-error-absolute-schema-path nested)
  =>
  '("properties" "rows" "items" "properties" "name" "type")
) ;check

(check-report)

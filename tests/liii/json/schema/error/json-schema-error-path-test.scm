(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-path
;; 读取实例路径。
;;
;; 语法
;; ----
;; (json-schema-error-path err)
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
;; 实例路径的元素按从根到当前位置的顺序排列，数组下标是整数。
;; 路径用于定位实例中的具体位置。
;;
;; 示例
;; ----
;; (json-schema-error-path (car (json-schema-errors 1 '(("type" . "integer"))))) => '()
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check (json-schema-error-path err) => '())

(define nested
  (car
    (json-schema-errors '(("rows" . #((("name" . 7)))))
      '(("properties"
         ("rows" ("items" ("properties" ("name" ("type" . "string")))))))
    ) ;json-schema-errors
  ) ;car
) ;define

(check (json-schema-error-path nested) => '("rows" 0 "name"))

(check-report)

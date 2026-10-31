(import (liii check)
  (liii json schema)
  (liii json schema error)
  (liii json schema data)
) ;import

(check-set-mode! 'report-failed)

;; json-schema-error-json-path
;; 读取 JSONPath 风格的错误路径。
;;
;; 语法
;; ----
;; (json-schema-error-json-path err)
;;
;; 参数
;; ----
;; err : json-schema-error
;; 错误对象。
;;
;; 返回值
;; ----
;; string
;;
;; 注意
;; ----
;; 形如 $.rows[0].name 的路径字符串。
;; 标识符形式的键用点号连接，其他键用 ['...'] 形式并转义引号与反斜杠。
;;
;; 示例
;; ----
;; (json-schema-error-json-path (car (json-schema-errors 1 '(("type" . "integer"))))) => "$"
;;
;; 错误处理
;; ----
;; 无。

(define err (car (json-schema-errors "x" '(("type" . "integer")))))
(check (json-schema-error-json-path err) => "$")

(define nested
  (car
    (json-schema-errors '(("rows" . #((("name" . 7)))))
      '(("properties"
         ("rows" ("items" ("properties" ("name" ("type" . "string")))))))
    ) ;json-schema-errors
  ) ;car
) ;define

(check (json-schema-error-json-path nested) => "$.rows[0].name")

;; 非标识符的键使用方括号形式并转义特殊字符。
(let ((error
        (car
          (json-schema-errors '(("a'b\\c" . 1))
            '(("properties" ("a'b\\c" ("type" . "string"))))
          ) ;json-schema-errors
        ) ;car
      ) ;error
     ) ;
  (check (json-schema-error-json-path error) => "$['a\\'b\\\\c']")
) ;let

(check-report)

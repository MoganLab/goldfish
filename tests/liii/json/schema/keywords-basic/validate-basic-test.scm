;; validate-basic
;; 校验基础断言类关键字并返回错误列表。
;;
;; 语法
;; ----
;; (validate-basic keyword value instance schema path schema-path)
;;
;; 参数
;; ----
;; keyword : string
;; 关键字名称，例如 "type"、"const"、"enum"。
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
;; 返回值
;; ----
;; list
;; 校验失败时返回错误列表，通过时返回空列表。
;;
;; 注意
;; ----
;; 字符串长度按 Unicode 码点计数，而不是 UTF-8 字节数。
;;
;; 错误处理
;; ----
;; type 关键字给出未知类型名时抛出 json-schema-schema-error。

(import (liii check) (liii json schema error) (liii json schema keywords-basic))

(check-set-mode! 'report-failed)

;; type 关键字。

(check
  (length (validate-basic "type" "integer" 1 '(("type" . "integer")) '() '()))
  =>
  0
) ;check
(check
  (length (validate-basic "type" "integer" "x" '(("type" . "integer")) '() '()))
  =>
  1
) ;check
(check
  (length (validate-basic "type" #("integer" "string") 1 '(("type" . "integer")) '() '())
  ) ;length
  =>
  0
) ;check
(check-catch 'json-schema-schema-error
  (validate-basic "type" "unknown" 1 '(("type" . "unknown")) '() '())
) ;check-catch

;; const 关键字按 JSON 语义比较。

(check
  (length (validate-basic "const" 1.0 1 '(("const" . 1)) '() '()))
  =>
  0
) ;check
(check
  (length (validate-basic "const" 1 #t '(("const" . 1)) '() '()))
  =>
  1
) ;check

;; enum 关键字按 JSON 语义比较。

(check
  (length (validate-basic "enum" #(1 2) 1 '(("enum" . #(1 2))) '() '()))
  =>
  0
) ;check
(check
  (length (validate-basic "enum" #(1 2) 3 '(("enum" . #(1 2))) '() '()))
  =>
  1
) ;check

;; enum 成员按 JSON 语义比较，1 与 1.0 视为同一个值。

(check
  (length (validate-basic "enum" #(1.0) 1 '(("enum" . #(1.0))) '() '()))
  =>
  0
) ;check
(check
  (length (validate-basic "enum" #(1) #t '(("enum" . #(1))) '() '()))
  =>
  1
) ;check

;; 数值上下界关键字。

(check
  (length (validate-basic "minimum" 2 2 '(("minimum" . 2)) '() '()))
  =>
  0
) ;check
(check
  (length (validate-basic "minimum" 2 1 '(("minimum" . 2)) '() '()))
  =>
  1
) ;check
(check
  (length (validate-basic "maximum" 2 2 '(("maximum" . 2)) '() '()))
  =>
  0
) ;check
(check
  (length (validate-basic "maximum" 2 3 '(("maximum" . 2)) '() '()))
  =>
  1
) ;check
(check
  (length (validate-basic "exclusiveMinimum" 2 3 '(("exclusiveMinimum" . 2)) '() '())
  ) ;length
  =>
  0
) ;check
(check
  (length (validate-basic "exclusiveMinimum" 2 2 '(("exclusiveMinimum" . 2)) '() '())
  ) ;length
  =>
  1
) ;check
(check
  (length (validate-basic "exclusiveMaximum" 2 1 '(("exclusiveMaximum" . 2)) '() '())
  ) ;length
  =>
  0
) ;check
(check
  (length (validate-basic "exclusiveMaximum" 2 2 '(("exclusiveMaximum" . 2)) '() '())
  ) ;length
  =>
  1
) ;check

;; 非数值实例跳过数值上下界校验。

(check
  (length (validate-basic "minimum" 2 "x" '(("minimum" . 2)) '() '()))
  =>
  0
) ;check

;; multipleOf 关键字。

(check
  (length (validate-basic "multipleOf" 2 4 '(("multipleOf" . 2)) '() '()))
  =>
  0
) ;check
(check
  (length (validate-basic "multipleOf" 2 3 '(("multipleOf" . 2)) '() '()))
  =>
  1
) ;check

;; 字符串长度按码点计数。

(check
  (length (validate-basic "minLength" 1 "😀" '(("minLength" . 1)) '() '()))
  =>
  0
) ;check
(check
  (length (validate-basic "minLength" 2 "😀" '(("minLength" . 2)) '() '()))
  =>
  1
) ;check
(check
  (length (validate-basic "maxLength" 1 "😀" '(("maxLength" . 1)) '() '()))
  =>
  0
) ;check
(check
  (length (validate-basic "maxLength" 0 "😀" '(("maxLength" . 0)) '() '()))
  =>
  1
) ;check

;; 非字符串实例跳过长度校验。

(check
  (length (validate-basic "minLength" 100 42 '(("minLength" . 100)) '() '()))
  =>
  0
) ;check

;; 未知关键字不产生错误。

(check
  (length (validate-basic "unknown" 1 1 '(("unknown" . 1)) '() '()))
  =>
  0
) ;check

;; 错误对象携带关键字与路径信息。

(define error
  (car (validate-basic "minimum" 2 1 '(("minimum" . 2)) '(("items" 0)) '("root")))
) ;define
(check (json-schema-error-keyword error) => "minimum")
(check (json-schema-error-keyword-value error) => 2)
(check (json-schema-error-instance error) => 1)
(check (json-schema-error-path error) => '(("items" 0)))
(check (json-schema-error-schema-path error) => '("root" "minimum"))

(check-report)

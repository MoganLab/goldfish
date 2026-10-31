(import (liii check) (liii json schema) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-check-schema
;; 检查 Schema 本身是否符合 JSON Schema 2020-12 元 Schema。
;;
;; 语法
;; ----
;; (json-schema-check-schema schema . options)
;;
;; 参数
;; ----
;; schema : any?
;; 待检查的 JSON Schema，可以是对象或布尔值。
;;
;; options : alist?
;; 可选选项表，详见 make-json-schema-validator。
;;
;; 返回值
;; ----
;; any?
;; 检查通过时原样返回 schema。
;;
;; 注意
;; ----
;; 元 Schema 与其词汇表均内嵌在库中，检查过程不做任何 I/O。
;; 该函数不会校验实例，只校验 Schema。
;;
;; 示例
;; ----
;; (json-schema-check-schema '(("type" . "integer"))) => '(("type" . "integer"))
;;
;; 错误处理
;; ----
;; json-schema-schema-error 当 Schema 不符合元 Schema 时。
;; json-schema-unknown-dialect 当 $schema 不是 2020-12 时。
;; json-schema-options-error 当选项非法时。

;; 布尔 Schema 总是合法
(check (json-schema-check-schema #t) => #t)
(check (json-schema-check-schema #f) => #f)

;; 关键字取值非法时报告 Schema 错误
(check-catch 'json-schema-schema-error
  (json-schema-check-schema '(("type" . 1)))
) ;check-catch
(check-catch 'json-schema-schema-error
  (json-schema-check-schema '(("required" . "x")))
) ;check-catch
(check-catch 'json-schema-schema-error
  (json-schema-check-schema '(("minimum" . "0")))
) ;check-catch
(check-catch 'json-schema-schema-error
  (json-schema-check-schema '(("prefixItems" . #t)))
) ;check-catch

(check-report)

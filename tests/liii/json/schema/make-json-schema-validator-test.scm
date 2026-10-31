(import (liii check) (liii json schema) (liii json))

(check-set-mode! 'report-failed)

;; make-json-schema-validator
;; 创建可复用的 JSON Schema 校验器。
;;
;; 语法
;; ----
;; (make-json-schema-validator schema . options)
;;
;; 参数
;; ----
;; schema : any?
;; JSON Schema，可以是对象或布尔值。
;;
;; options : alist?
;; 可选选项表，支持以下键：
;;
;; check-schema? : boolean    是否先按 2020-12 元 Schema 检查 Schema，默认 #f。
;; formats : #f | #t | list    格式断言配置，默认 #f（不断言）。
;; format-checkers : alist      自定义格式检查器，默认 '()。
;; registry : registry         外部 Schema 资源表，默认空表。
;; retrieve : procedure        外部资源获取过程，默认 #f（禁止 I/O）。
;; max-errors : #f | integer   错误列表上限，默认 #f。
;; max-depth : integer         单次校验最大求值深度，默认 1000。
;; max-steps : integer         单次校验最大 Schema 节点评估数，默认 1000000。
;; max-references : integer    单次校验最大引用解析次数，默认 100000。
;;
;; 返回值
;; ----
;; validator
;; 校验器记录，可传给 json-schema-validator-valid? 等函数复用。
;;
;; 注意
;; ----
;; 复用校验器不会重复执行元 Schema 检查，引用根、元资源注册表与引用缓存会被保留。
;;
;; 示例
;; ----
;; (define v (make-json-schema-validator '(("type" . "integer"))))
;; (json-schema-validator-valid? v 1) => #t
;;
;; 错误处理
;; ----
;; json-schema-options-error 当选项非法时。
;; json-schema-schema-error 当 Schema 非法时。

(define validator (make-json-schema-validator '(("type" . "integer"))))
(check-true (json-schema-validator? validator))

;; check-schema? 打开时元 Schema 检查同样生效
(check-catch 'json-schema-schema-error
  (make-json-schema-validator '(("type" . 1)) '((check-schema? . #t)))
) ;check-catch
(check-true
  (json-schema-validator? (make-json-schema-validator '(("type" . 1))))
) ;check-true

;; 非法选项
(check-catch 'json-schema-options-error
  (make-json-schema-validator #t '((registry)))
) ;check-catch
(check-catch 'json-schema-options-error
  (make-json-schema-validator #t '((retrieve . "not-a-procedure")))
) ;check-catch
(check-catch 'json-schema-options-error (make-json-schema-validator #t '() '()))
(check-catch 'json-schema-options-error
  (make-json-schema-validator #t '((formats . "date")))
) ;check-catch
(check-catch 'json-schema-options-error
  (make-json-schema-validator #t '((max-depth . 0)))
) ;check-catch

(check-report)

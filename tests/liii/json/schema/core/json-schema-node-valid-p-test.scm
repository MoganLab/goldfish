(import (liii check) (liii json schema) (liii json schema core) (liii json))

(check-set-mode! 'report-failed)

;; json-schema-node-valid?
;; 以指定的根资源校验单个 Schema 节点，返回实例是否合法。
;;
;; 语法
;; ----
;; (json-schema-node-valid? validator instance schema root)
;;
;; 参数
;; ----
;; validator : validator
;; 由 make-json-schema-validator 创建的校验器。
;;
;; instance : any?
;; 待校验的 JSON 数据。
;;
;; schema : any?
;; 待求值的 Schema 节点，可以是对象或布尔值。
;;
;; root : any?
;; 根资源，用于解析节点内的相对引用。
;;
;; 返回值
;; ----
;; boolean
;; 节点校验通过返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 该函数只返回布尔结果，不收集错误列表；
;; 需要错误列表时请使用 json-schema-descend-errors。
;; 每次调用都会重置动态作用域，因此可以在同一次校验中重复使用。
;;
;; 示例
;; ----
;; (json-schema-node-valid? validator 1 '(("type" . "integer")) #t) => #t
;;
;; 错误处理
;; ----
;; json-schema-schema-error 当 Schema 非法时。

(define validator (make-json-schema-validator #t))

;; 对象节点
(check-true (json-schema-node-valid? validator '(("a" . 1)) '(("type"
                                                               . "object")) #t)
) ;check-true
(check-false (json-schema-node-valid? validator '("not-an-object") '(("type"
                                                                      . "object")) #t)
) ;check-false

;; 属性约束在节点求值时同样生效
(check-true
  (json-schema-node-valid? validator
    '(("a" . 1))
    '(("type" . "object") ("properties" ("a" ("type" . "integer"))))
    #t
  ) ;json-schema-node-valid?
) ;check-true
(check-false
  (json-schema-node-valid? validator
    '(("a" . "x"))
    '(("type" . "object") ("properties" ("a" ("type" . "integer"))))
    #t
  ) ;json-schema-node-valid?
) ;check-false

;; 布尔 Schema：true 接受一切，false 拒绝一切
(check-true (json-schema-node-valid? validator 1 #t #t))
(check-false (json-schema-node-valid? validator 1 #f #t))

;; 类型节点
(check-true (json-schema-node-valid? validator 1 '(("type" . "integer")) #t))
(check-false (json-schema-node-valid? validator 1.5 '(("type" . "integer")) #t))

;; 相对引用按根资源解析

(define rooted
  '(("$id" . "https://example.com/root.json")
    ("$defs" ("item" ("type" . "integer")))
    ("type" . "object")
    ("properties" ("n" ("$ref" . "#/$defs/item"))))
) ;define
(check-true (json-schema-node-valid? validator '(("n" . 1)) rooted rooted))
(check-false (json-schema-node-valid? validator '(("n" . "x")) rooted rooted))

(check-report)

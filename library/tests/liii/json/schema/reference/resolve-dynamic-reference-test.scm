;; resolve-dynamic-reference
;; 解析动态引用，返回引用指向的资源。
;;
;; 语法
;; ----
;; (resolve-dynamic-reference validator root ref)
;;
;; 参数
;; ----
;; validator : validator
;; 当前校验器，提供 retrieve、registry 等选项。
;;
;; root : any?
;; 根资源。
;;
;; ref : string
;; 动态引用字符串。
;;
;; 返回值
;; ----
;; any?
;; 引用指向的资源。
;;
;; 注意
;; ----
;; 形如 #/path 的动态引用退回静态 JSON Pointer 解析，
;; 形如 #name 的动态引用按动态锚点解析，
;; 其它引用按静态引用解析。
;;
;; 动态锚点的解析会先在当前动态作用域中查找，
;; 作用域由 call-with-dynamic-scope 建立。
;;
;; 错误处理
;; ----
;; 引用无法解析时抛出 json-schema-unresolvable。

(import (liii check)
  (liii json schema)
  (liii json schema model)
  (liii json schema reference)
) ;import

(check-set-mode! 'report-failed)

(define validator (make-validator '(("$defs" ("x" ("const" . 1)))) '()))

(define root '(("$defs" ("x" ("const" . 1)))))

;; 动态引用退回静态 JSON Pointer 解析。

(check (resolve-dynamic-reference validator root "#/$defs/x")
  =>
  '(("const" . 1))
) ;check

;; 动态锚点在当前资源中解析。

(define local-root '(("$dynamicAnchor" . "node") ("const" . 1)))

(check (resolve-dynamic-reference validator local-root "#node")
  =>
  '(("$dynamicAnchor" . "node") ("const" . 1))
) ;check

;; 动态作用域中的资源优先于当前资源。

(define scoped-root '(("$dynamicAnchor" . "node") ("const" . 2)))

(check (call-with-dynamic-scope scoped-root
         (lambda () (resolve-dynamic-reference validator local-root "#node"))
       ) ;call-with-dynamic-scope
  =>
  '(("$dynamicAnchor" . "node") ("const" . 2))
) ;check

;; 找不到动态锚点时报错。

(check-catch 'json-schema-unresolvable
  (resolve-dynamic-reference validator root "#missing")
) ;check-catch

;; 动态引用退回静态 Pointer 解析。

(check-true
  (json-schema-valid? 1
    '(("$defs" ("one" ("const" . 1))) ("$dynamicRef" . "#/$defs/one"))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? 2
    '(("$defs" ("one" ("const" . 1))) ("$dynamicRef" . "#/$defs/one"))
  ) ;json-schema-valid?
) ;check-false

;; 动态锚点可以沿递归结构逐层解析。

(define dynamic-tree
  '(("$dynamicAnchor" . "node")
    ("type" . "object")
    ("properties"
     ("value" ("type" . "number"))
     ("next" ("$dynamicRef" . "#node")))
    ("required" . #("value")))
) ;define

(check-true
  (json-schema-valid? '(("value" . 1) ("next" ("value" . 2))) dynamic-tree)
) ;check-true
(check-false
  (json-schema-valid? '(("value" . 1) ("next" ("value" . "bad"))) dynamic-tree)
) ;check-false

;; 动态锚点在不同资源之间保持一致。

(define dynamic-child
  '(("$dynamicAnchor" . "node")
    ("type" . "object")
    ("$defs"
     ("child"
      ("$id" . "child.json")
      ("$dynamicAnchor" . "node")
      ("$dynamicRef" . "#node")))
    ("properties"
     ("value" ("type" . "number"))
     ("next" ("$ref" . "child.json"))))
) ;define

(check-true
  (json-schema-valid? '(("value" . 1) ("next" ("value" . 2))) dynamic-child)
) ;check-true

;; 找不到动态锚点时报错。

(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1 '(("$dynamicRef" . "#missing")))
) ;check-catch

;; 外部 Pointer 引用保留所属资源作为 base URI。

(define nested-dynamic-resources
  '(("$id"
     . "https://test.json-schema.org/dynamic-ref-avoids-root-of-each-schema/base")
    ("$ref" . "first#/$defs/stuff")
    ("$defs"
     ("first"
      ("$id" . "first")
      ("$defs" ("stuff" ("$ref" . "second#/$defs/stuff"))))
     ("second"
      ("$id" . "second")
      ("$defs"
       ("stuff" ("$ref" . "third#/$defs/stuff"))
       ("length" ("$dynamicAnchor" . "length") ("maxLength" . 2))))
     ("third"
      ("$id" . "third")
      ("$defs"
       ("stuff" ("$dynamicRef" . "#length"))
       ("length" ("$dynamicAnchor" . "length") ("maxLength" . 3))))))
) ;define

(check-true (json-schema-valid? "hi" nested-dynamic-resources))
(check-false (json-schema-valid? "hey" nested-dynamic-resources))

(check-report)

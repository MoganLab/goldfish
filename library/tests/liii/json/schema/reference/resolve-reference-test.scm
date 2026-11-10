;; resolve-reference
;; 解析引用，返回引用指向的资源。
;;
;; 语法
;; ----
;; (resolve-reference validator ref root)
;;
;; 参数
;; ----
;; validator : validator
;; 当前校验器，提供 retrieve、registry 等选项。
;;
;; ref : string
;; 引用字符串，可以是 JSON Pointer、锚点或资源 URI。
;;
;; root : any?
;; 根资源。
;;
;; 返回值
;; ----
;; any?
;; 引用指向的资源。
;;
;; 注意
;; ----
;; 以 # 开头的引用在当前资源内解析，
;; 形如 #/path 的引用按 JSON Pointer 解析，
;; 形如 #name 的引用按锚点解析，
;; 其它引用按资源 URI 解析，可能命中内嵌资源或登记表。
;;
;; 错误处理
;; ----
;; 引用不是字符串、目标不存在或 retrieve 返回非法资源时
;; 抛出 json-schema-unresolvable。

(import (liii check)
  (liii json schema)
  (liii json schema model)
  (liii json schema reference)
  (liii json schema registry)
) ;import

(check-set-mode! 'report-failed)

(define validator (make-validator '(("$defs" ("x" ("const" . 1)))) '()))

(define root '(("$defs" ("x" ("const" . 1)))))

;; JSON Pointer 引用。

(check (resolve-reference validator "#/$defs/x" root) => '(("const" . 1)))

;; 单独的 # 指向根资源自身。

(check (resolve-reference validator "#" root) => root)

;; 锚点引用。

(define anchored '(("$defs" ("x" ("$anchor" . "x") ("const" . 7)))))

(check (resolve-reference (make-validator anchored '()) "#x" anchored)
  =>
  '(("$anchor" . "x") ("const" . 7))
) ;check

;; 非字符串引用报错。

(check-catch 'json-schema-unresolvable (resolve-reference validator 42 root))

;; $id 建立内嵌资源与相对基准作用域。

(define id-schema
  '(("$id" . "https://example.com/root.json")
    ("$defs"
     ("child"
      ("$id" . "child.json")
      ("$defs" ("value" ("const" . 1)))
      ("$ref" . "#/$defs/value"))
     ("other" ("const" . 2)))
    ("properties"
     ("x" ("$ref" . "child.json"))
     ("y" ("$ref" . "#/$defs/other"))))
) ;define

(define id-validator (make-validator id-schema '()))

(check (resolve-reference id-validator "child.json" id-schema)
  =>
  '(("$id" . "https://example.com/child.json")
    ("$defs" ("value" ("const" . 1)))
    ("$ref" . "#/$defs/value"))
) ;check

;; $id 作用域内的相对引用可以被解析。

(check-true (json-schema-valid? '(("x" . 1) ("y" . 2)) id-schema))
(check-false (json-schema-valid? '(("x" . 2) ("y" . 2)) id-schema))
(check-false (json-schema-valid? '(("x" . 1) ("y" . 1)) id-schema))

;; 非法的 $id 名称不会被当作资源标识符。

(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1
    '(("$defs" ("old" ("id" . "urn:old") ("const" . 1))) ("$ref" . "urn:old"))
  ) ;json-schema-valid?
) ;check-catch

;; JSON Pointer 引用可以包含转义字符。

(check-true
  (json-schema-valid? 1
    '(("$defs" ("a/b" ("const" . 1))) ("$ref" . "#/$defs/a~1b"))
  ) ;json-schema-valid?
) ;check-true
(check-true
  (json-schema-valid? 1
    '(("$defs" ("a/b" ("const" . 1))) ("$ref" . "#/$defs/a%7E1b"))
  ) ;json-schema-valid?
) ;check-true

;; 登记表中的资源可以被引用。

(define registry
  (json-schema-registry-add (make-json-schema-registry)
    "urn:integer"
    '(("type" . "integer"))
  ) ;json-schema-registry-add
) ;define

(define registry-validator
  (make-validator '(("$ref" . "urn:integer")) `((registry . ,registry)))
) ;define

(check (resolve-reference registry-validator "urn:integer" '(("$ref"
                                                              . "urn:integer")))
  =>
  '(("$id" . "urn:integer") ("type" . "integer"))
) ;check

;; 登记表为空时引用无法解析。

(define empty-validator (make-validator '(("$ref" . "urn:integer")) '()))

(check-catch 'json-schema-unresolvable
  (resolve-reference empty-validator "urn:integer" '(("$ref" . "urn:integer")))
) ;check-catch

;; retrieve 是显式的，并收到解析后的绝对 URI。

(define retrieve-calls 0)

(define retrieved-uri #f)

(define retrieve-one
  (lambda (uri)
    (set! retrieve-calls (+ retrieve-calls 1))
    (set! retrieved-uri uri)
    '(("const" . 1))
  ) ;lambda
) ;define

(define retrieve-validator
  (make-validator '(("$ref" . "urn:one")) `((retrieve . ,retrieve-one)))
) ;define

(check (resolve-reference retrieve-validator "urn:one" '(("$ref" . "urn:one")))
  =>
  '(("$id" . "urn:one") ("const" . 1))
) ;check
(check retrieve-calls => 1)
(check retrieved-uri => "urn:one")

;; retrieve 返回非法资源时报错。

(define retrieve-invalid (lambda (uri) 42))

(define invalid-validator
  (make-validator '(("$ref" . "urn:invalid")) `((retrieve . ,retrieve-invalid)))
) ;define

(check-catch 'json-schema-unresolvable
  (resolve-reference invalid-validator "urn:invalid" '(("$ref" . "urn:invalid")))
) ;check-catch

;; 没有 retrieve 时远程引用无法解析。

(check-catch 'json-schema-unresolvable
  (resolve-reference validator "urn:missing" '(("$ref" . "urn:missing")))
) ;check-catch

;; 锚点与 JSON Pointer 片段互不相同，且锚点只在资源内有效。

(check-true
  (json-schema-valid? 1
    '(("$defs" ("positive" ("$anchor" . "positive") ("minimum" . 1)))
      ("$ref" . "#positive"))
  ) ;json-schema-valid?
) ;check-true
(check-false
  (json-schema-valid? 0
    '(("$defs" ("positive" ("$anchor" . "positive") ("minimum" . 1)))
      ("$ref" . "#positive"))
  ) ;json-schema-valid?
) ;check-false
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1
    '(("$defs"
       ("child" ("$id" . "child.json") ("$anchor" . "hidden") ("const" . 1)))
      ("$ref" . "#hidden"))
  ) ;json-schema-valid?
) ;check-catch

;; 资源 URI 上的锚点由该资源的锚点解析。

(define anchored-resource
  '(("$id" . "https://example.com/anchored.json")
    ("$defs"
     ("child" ("$id" . "child.json") ("$anchor" . "value") ("const" . 1)))
    ("$ref" . "child.json#value"))
) ;define

(check-true (json-schema-valid? 1 anchored-resource))
(check-false (json-schema-valid? 2 anchored-resource))

;; 登记表中的资源也可以带锚点。

(define anchor-registry
  (json-schema-registry-add (make-json-schema-registry)
    "urn:anchored"
    '(("$anchor" . "answer") ("const" . 42))
  ) ;json-schema-registry-add
) ;define

(check-true
  (json-schema-valid? 42
    '(("$ref" . "urn:anchored#answer"))
    `((registry . ,anchor-registry))
  ) ;json-schema-valid?
) ;check-true

;; 静态引用与兄弟关键字同时生效。

(check-false
  (json-schema-valid? 0
    '(("$defs" ("nonnegative" ("minimum" . 0)))
      ("$ref" . "#/$defs/nonnegative")
      ("minimum" . 1))
  ) ;json-schema-valid?
) ;check-false

;; 内置元 Schema 可以被引用。

(check-true (json-schema-valid? '(("type" . "integer"))
              '(("$ref" . "https://json-schema.org/draft/2020-12/schema"))
            ) ;json-schema-valid?
) ;check-true

;; 递归引用形成有限的资源图。

(define tree-schema
  '(("$id" . "http://example.com/tree")
    ("type" . "object")
    ("properties" ("nodes" ("type" . "array") ("items" ("$ref" . "node"))))
    ("required" . #("nodes"))
    ("$defs"
     ("node"
      ("$id" . "http://example.com/node")
      ("type" . "object")
      ("properties" ("value" ("type" . "number")) ("subtree" ("$ref" . "tree")))
      ("required" . #("value")))))
) ;define

(check-true (json-schema-valid? '(("nodes" . #((("value" . 1))))) tree-schema))
(check-false (json-schema-valid? '(("nodes" . #((("value" . "bad"))))) tree-schema)
) ;check-false

;; 资源内对自身 base URI 的引用不再重复调用 retrieve。

(define lazy-calls 0)

(define recursive-resource
  '(("type" . "object")
    ("properties"
     ("value" ("type" . "number"))
     ("next" ("$ref" . "urn:recursive"))))
) ;define

(define retrieve-recursive
  (lambda (uri) (set! lazy-calls (+ lazy-calls 1)) recursive-resource)
) ;define

(check-true
  (json-schema-valid? '(("value" . 1) ("next" ("value" . 2)))
    '(("$ref" . "urn:recursive"))
    `((retrieve . ,retrieve-recursive))
  ) ;json-schema-valid?
) ;check-true
(check lazy-calls => 1)

(check-report)

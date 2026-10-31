(import (liii json schema reference)
  (liii json schema)
  (liii check)
  (scheme base)
) ;import
(check-set-mode! 'report-failed)

(check-true (absolute-uri? "https://json-schema.org/draft/2020-12/schema"))
(check-true (absolute-uri? "urn:example:resource"))
(check-false (absolute-uri? "meta/core"))
(check-false (absolute-uri? "../schema"))

(check (uri-fragment "https://example.com/schema#") => "")
(check (uri-fragment "https://example.com/schema#/$defs/x") => "/$defs/x")
(check-false (uri-fragment "https://example.com/schema"))
(check (uri-without-fragment "https://example.com/schema#abc")
  =>
  "https://example.com/schema"
) ;check
(check (uri-directory "https://example.com/root/schema.json#x")
  =>
  "https://example.com/root/"
) ;check
(check (uri-directory "urn:example:resource") => "urn:example:resource")

(check (percent-decode "%7E0%7E1") => "~0~1")
(check (percent-decode "%E4%B8%AD") => "中")
(check (percent-decode "abc") => "abc")
(check-catch 'json-schema-unresolvable (percent-decode "%"))
(check-catch 'json-schema-unresolvable (percent-decode "%XZ"))

(define root '(("$id" . "https://example.com/root/schema.json")))
(check (resolve-resource-name "meta/core" root)
  =>
  "https://example.com/root/meta/core"
) ;check
(check (resolve-resource-name "" root)
  =>
  "https://example.com/root/schema.json"
) ;check
(check (resolve-resource-name "urn:other" root) => "urn:other")

;; gf >= 18.11.47: uri-join 保留相对引用结尾斜杠（RFC 3986 5.2.3）。
(check (resolve-resource-name "baseUriChange/" root)
  =>
  "https://example.com/root/baseUriChange/"
) ;check
(check (resolve-resource-name "../up/" '(("$id" . "http://h/draft/a/b.json")))
  =>
  "http://h/draft/up/"
) ;check
;; 目录型合并结果可以作为后续合并的 base，不替换目录段。
(check (resolve-resource-name "file.json" '(("$id"
                                             . "http://h/draft/baseUriChange/")))
  =>
  "http://h/draft/baseUriChange/file.json"
) ;check
;; JSON Pointer evaluation and strict array indices.

(define pointer-root
  '(("" . "empty")
    ("a/b" . "slash")
    ("m~n" . "tilde")
    ("items" . #(10 20 30))
    ("nested" ("x" . 1)))
) ;define
(check (resolve-pointer pointer-root "") => pointer-root)
(check (resolve-pointer pointer-root "/") => "empty")
(check (resolve-pointer pointer-root "/a~1b") => "slash")
(check (resolve-pointer pointer-root "/m~0n") => "tilde")
(check (resolve-pointer pointer-root "/items/0") => 10)
(check (resolve-pointer pointer-root "/items/2") => 30)
(check (resolve-pointer pointer-root "/nested/x") => 1)
(for-each (lambda (pointer)
            (check-catch 'json-schema-unresolvable (resolve-pointer pointer-root pointer))
          ) ;lambda
  '("items" "x" "/missing" "/items/-" "/items/-1" "/items/01" "/items/+1"
    "/items/1.0" "/items/3")
) ;for-each
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

;; Registry derivation is immutable and isolated between validators.

(define registry-empty (make-json-schema-registry))

(define registry-one
  (json-schema-registry-add registry-empty "urn:integer" '(("type" . "integer")))
) ;define

(define registry-two
  (json-schema-registry-add-many registry-one
    '(("https://example.com/string" ("type" . "string")) ("urn:boolean" . #t))
  ) ;json-schema-registry-add-many
) ;define
(check-true (json-schema-registry? registry-empty))
(check-true (json-schema-registry? registry-one))
(check-true (json-schema-registry? registry-two))
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1 '(("$ref" . "urn:integer")))
) ;check-catch
(check-true
  (json-schema-valid? 1 '(("$ref" . "urn:integer")) `((registry . ,registry-one)))
) ;check-true
(check-true
  (json-schema-valid? "x"
    '(("$ref" . "https://example.com/string"))
    `((registry . ,registry-two))
  ) ;json-schema-valid?
) ;check-true
(check-true
  (json-schema-valid? 'null
    '(("$ref" . "urn:boolean"))
    `((registry . ,registry-two))
  ) ;json-schema-valid?
) ;check-true
(check-catch 'json-schema-options-error
  (json-schema-registry-add registry-two "urn:integer" #f)
) ;check-catch
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1
    '(("$ref" . "urn:integer"))
    `((registry . ,registry-empty))
  ) ;json-schema-valid?
) ;check-catch

;; $id establishes embedded resources and relative base scopes.

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
(check-true (json-schema-valid? '(("x" . 1) ("y" . 2)) id-schema))
(check-false (json-schema-valid? '(("x" . 2) ("y" . 2)) id-schema))
(check-false (json-schema-valid? '(("x" . 1) ("y" . 1)) id-schema))
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1
    '(("$defs" ("old" ("id" . "urn:old") ("const" . 1))) ("$ref" . "urn:old"))
  ) ;json-schema-valid?
) ;check-catch

;; Anchors are distinct from JSON Pointer fragments and resource-local.
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

(define anchored-resource
  '(("$id" . "https://example.com/anchored.json")
    ("$defs"
     ("child" ("$id" . "child.json") ("$anchor" . "value") ("const" . 1)))
    ("$ref" . "child.json#value"))
) ;define
(check-true (json-schema-valid? 1 anchored-resource))
(check-false (json-schema-valid? 2 anchored-resource))
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1
    '(("$defs"
       ("child" ("$id" . "child.json") ("$anchor" . "hidden") ("const" . 1)))
      ("$ref" . "#hidden"))
  ) ;json-schema-valid?
) ;check-catch

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

;; Static references compose with siblings and built-in meta resources.
(check-false
  (json-schema-valid? 0
    '(("$defs" ("nonnegative" ("minimum" . 0)))
      ("$ref" . "#/$defs/nonnegative")
      ("minimum" . 1))
  ) ;json-schema-valid?
) ;check-false
(check-true (json-schema-valid? '(("type" . "integer"))
              '(("$ref" . "https://json-schema.org/draft/2020-12/schema"))
            ) ;json-schema-valid?
) ;check-true
(check-false (json-schema-valid? '(("type" . 1))
               '(("$ref" . "https://json-schema.org/draft/2020-12/schema"))
             ) ;json-schema-valid?
) ;check-false

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
(check-true (json-schema-valid? '(("nodes"
                                   . #((("value" . 1)
                                        ("subtree" ("nodes" . #())))
                                     ) ;#
                                  ))
              tree-schema
            ) ;json-schema-valid?
) ;check-true
(check-false (json-schema-valid? '(("nodes" . #((("value" . "bad"))))) tree-schema)
) ;check-false

;; retrieve is explicit, lazy and receives the resolved absolute URI.

(define retrieve-calls 0)

(define retrieved-uri #f)

(define retrieve-one
  (lambda (uri)
    (set! retrieve-calls (+ retrieve-calls 1))
    (set! retrieved-uri uri)
    '(("const" . 1))
  ) ;lambda
) ;define
(check-true
  (json-schema-valid? 1 '(("$ref" . "urn:one")) `((retrieve . ,retrieve-one)))
) ;check-true
(check-false
  (json-schema-valid? 2 '(("$ref" . "urn:one")) `((retrieve . ,retrieve-one)))
) ;check-false
(check retrieve-calls => 2)
(check retrieved-uri => "urn:one")

(define (retrieve-invalid uri)
  42
) ;define
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1
    '(("$ref" . "urn:invalid"))
    `((retrieve . ,retrieve-invalid))
  ) ;json-schema-valid?
) ;check-catch

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

(define recursive-options `((retrieve . ,retrieve-recursive)))
(check-true
  (json-schema-valid? '(("value" . 1) ("next" ("value" . 2)))
    '(("$ref" . "urn:recursive"))
    recursive-options
  ) ;json-schema-valid?
) ;check-true
;; 外层引用通过 retrieve 获取；资源内对自身 base URI 的引用解析为
;; 已加载的资源本身，不再重复调用 retrieve。
(check lazy-calls => 1)
(check-false
  (json-schema-valid? '(("value" . 1) ("next" ("value" . "bad")))
    '(("$ref" . "urn:recursive"))
    recursive-options
  ) ;json-schema-valid?
) ;check-false

;; Dynamic references use static Pointer fallback and dynamic anchors.
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
(check-catch 'json-schema-unresolvable
  (json-schema-valid? 1 '(("$dynamicRef" . "#missing")))
) ;check-catch

;; External Pointer references retain their containing resource as the base URI.

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

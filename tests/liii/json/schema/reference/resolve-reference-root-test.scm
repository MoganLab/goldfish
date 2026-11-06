;; resolve-reference-root
;; 解析引用并返回它所属的根资源。
;;
;; 语法
;; ----
;; (resolve-reference-root validator ref root)
;;
;; 参数
;; ----
;; validator : validator
;; 当前校验器，提供 retrieve、registry 等选项。
;;
;; ref : string
;; 引用字符串。
;;
;; root : any?
;; 根资源。
;;
;; 返回值
;; ----
;; any?
;; 引用所属的根资源。
;;
;; 注意
;; ----
;; 以 # 开头的引用属于当前根资源，直接返回它；
;; 其它引用先解析出资源，再返回该资源所属的根。
;;
;; 错误处理
;; ----
;; 引用无法解析时抛出 json-schema-unresolvable。

(import (liii check) (liii json schema model) (liii json schema reference))

(check-set-mode! 'report-failed)

(define validator (make-validator '(("$defs" ("x" ("const" . 1)))) '()))

(define root '(("$defs" ("x" ("const" . 1)))))

;; 以 # 开头的引用属于当前根资源。

(check (resolve-reference-root validator "#/$defs/x" root) => root)
(check (resolve-reference-root validator "#" root) => root)

;; 锚点引用同样属于当前根资源。

(define anchored '(("$defs" ("x" ("$anchor" . "x") ("const" . 7)))))

(check (resolve-reference-root validator "#x" anchored) => anchored)

;; 资源 URI 指向的根由资源自身的 $id 决定。

(define id-schema
  '(("$id" . "https://example.com/root.json")
    ("$defs" ("child" ("$id" . "child.json") ("const" . 1))))
) ;define

(check (resolve-reference-root (make-validator id-schema '()) "child.json" id-schema)
  =>
  '(("$id" . "https://example.com/child.json") ("const" . 1))
) ;check

;; 无法解析的引用抛出 json-schema-unresolvable。

(check-catch 'json-schema-unresolvable
  (resolve-reference-root validator "urn:missing" '(("$ref" . "urn:missing")))
) ;check-catch

(check-report)

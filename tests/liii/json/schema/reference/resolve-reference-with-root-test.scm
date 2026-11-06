;; resolve-reference-with-root
;; 解析引用并同时返回资源与它所属的根资源。
;;
;; 语法
;; ----
;; (resolve-reference-with-root validator ref root)
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
;; values
;; 两个值：引用指向的资源，以及该资源所属的根资源。
;;
;; 注意
;; ----
;; 以 # 开头的引用在当前根资源内解析，根资源保持不变；
;; 其它引用会切换到目标资源所属的根资源。
;;
;; 错误处理
;; ----
;; 引用无法解析时抛出 json-schema-unresolvable。

(import (liii check) (liii json schema model) (liii json schema reference))

(check-set-mode! 'report-failed)

(define validator (make-validator '(("$defs" ("x" ("const" . 1)))) '()))

(define root '(("$defs" ("x" ("const" . 1)))))

;; 以 # 开头的引用在当前根资源内解析。

(call-with-values (lambda () (resolve-reference-with-root validator "#/$defs/x" root))
  (lambda (resource target-root)
    (check resource => '(("const" . 1)))
    (check target-root => root)
  ) ;lambda
) ;call-with-values

;; 单独的 # 指向根资源自身。

(call-with-values (lambda () (resolve-reference-with-root validator "#" root))
  (lambda (resource target-root)
    (check resource => root)
    (check target-root => root)
  ) ;lambda
) ;call-with-values

;; 资源 URI 指向的根由资源自身的 $id 决定。

(define id-schema
  '(("$id" . "https://example.com/root.json")
    ("$defs" ("child" ("$id" . "child.json") ("const" . 1))))
) ;define

(call-with-values (lambda ()
                    (resolve-reference-with-root (make-validator id-schema '())
                      "child.json"
                      id-schema
                    ) ;resolve-reference-with-root
                  ) ;lambda
  (lambda (resource target-root)
    (check resource => '(("$id" . "https://example.com/child.json")
                         ("const" . 1)))
    (check target-root
      =>
      '(("$id" . "https://example.com/child.json") ("const" . 1))
    ) ;check
  ) ;lambda
) ;call-with-values

;; 无法解析的引用抛出 json-schema-unresolvable。

(check-catch 'json-schema-unresolvable
  (resolve-reference-with-root validator
    "urn:missing"
    '(("$ref" . "urn:missing"))
  ) ;resolve-reference-with-root
) ;check-catch

(check-report)

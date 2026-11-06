;; resolve-resource-name
;; 把相对引用解析成绝对 URI。
;;
;; 语法
;; ----
;; (resolve-resource-name ref root)
;;
;; 参数
;; ----
;; ref : string
;; 待解析的引用，可以是绝对 URI，也可以是相对路径。
;;
;; root : any?
;; 根资源，提供 $id 作为解析基准。
;;
;; 返回值
;; ----
;; string
;; 解析后的绝对 URI。
;;
;; 注意
;; ----
;; 引用本身是绝对 URI 时原样返回；
;; 根资源没有 $id 时也原样返回；
;; 空引用解析为根资源的 $id 本身。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema embedded))

(check-set-mode! 'report-failed)

(define root '(("$id" . "https://example.com/root/schema.json")))

;; 相对引用基于根资源的 $id 解析。

(check (resolve-resource-name "meta/core" root)
  =>
  "https://example.com/root/meta/core"
) ;check
(check (resolve-resource-name "sub/child.json" root)
  =>
  "https://example.com/root/sub/child.json"
) ;check

;; 空引用解析为根资源自身。

(check (resolve-resource-name "" root)
  =>
  "https://example.com/root/schema.json"
) ;check

;; 绝对引用原样返回。

(check (resolve-resource-name "urn:other" root) => "urn:other")
(check (resolve-resource-name "https://other.example.com/x" root)
  =>
  "https://other.example.com/x"
) ;check

;; 根资源没有 $id 时原样返回。

(check (resolve-resource-name "meta/core" '(("type" . "object")))
  =>
  "meta/core"
) ;check

(check-report)

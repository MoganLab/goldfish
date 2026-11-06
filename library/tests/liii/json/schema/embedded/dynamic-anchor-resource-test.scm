;; dynamic-anchor-resource
;; 在根资源中查找带 $dynamicAnchor 的 Schema。
;;
;; 语法
;; ----
;; (dynamic-anchor-resource root anchor)
;;
;; 参数
;; ----
;; root : any?
;; 根资源。
;;
;; anchor : string
;; 动态锚点名。
;;
;; 返回值
;; ----
;; any?
;; 找到时返回该 Schema 对象。
;;
;; 注意
;; ----
;; 与 anchor-resource 不同，这里只查找 $dynamicAnchor。
;;
;; 错误处理
;; ----
;; 找不到动态锚点时抛出 json-schema-unresolvable。

(import (liii check) (liii json schema embedded))

(check-set-mode! 'report-failed)

(define document
  '(("$defs" ("a" ("$dynamicAnchor" . "node") ("type" . "string"))))
) ;define

(check (dynamic-anchor-resource document "node")
  =>
  '(("$dynamicAnchor" . "node") ("type" . "string"))
) ;check

;; 找不到动态锚点时报错。

(check-catch 'json-schema-unresolvable
  (dynamic-anchor-resource document "missing")
) ;check-catch

(check-report)

;; anchor-resource
;; 在根资源中查找带 $anchor 的 Schema。
;;
;; 语法
;; ----
;; (anchor-resource root anchor)
;;
;; 参数
;; ----
;; root : any?
;; 根资源。
;;
;; anchor : string
;; 锚点名。
;;
;; 返回值
;; ----
;; any?
;; 找到时返回该 Schema 对象。
;;
;; 注意
;; ----
;; 找不到静态 $anchor 时会继续查找 $dynamicAnchor。
;;
;; 错误处理
;; ----
;; 两种锚点都找不到时抛出 json-schema-unresolvable。

(import (liii check) (liii json schema embedded))

(check-set-mode! 'report-failed)

(define document
  '(("$defs"
     ("a" ("$anchor" . "x") ("type" . "string"))
     ("b" ("$anchor" . "y"))))
) ;define

(check (anchor-resource document "x")
  =>
  '(("$anchor" . "x") ("type" . "string"))
) ;check
(check (anchor-resource document "y") => '(("$anchor" . "y")))

;; 找不到锚点时报错。

(check-catch 'json-schema-unresolvable (anchor-resource document "missing"))

(check-report)

;; walk-dynamic-anchor
;; 在 Schema 树中查找带 $dynamicAnchor 的节点。
;;
;; 语法
;; ----
;; (walk-dynamic-anchor schema anchor top?)
;;
;; 参数
;; ----
;; schema : any?
;; 待查找的 Schema，可以是对象、数组或标量。
;;
;; anchor : string
;; 动态锚点名。
;;
;; top? : boolean
;; 当前节点是否为根节点。
;;
;; 返回值
;; ----
;; any? | boolean
;; 找到时返回该 Schema 对象，未找到时返回 #f。
;;
;; 注意
;; ----
;; 非根节点自带 $id 时不会继续向下查找，
;; 避免跨越资源边界匹配到其它资源的锚点。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema embedded))

(check-set-mode! 'report-failed)

(define document
  '(("$defs" ("a" ("$dynamicAnchor" . "node") ("type" . "string"))))
) ;define

;; 根节点向下查找。

(check (walk-dynamic-anchor document "node" #t)
  =>
  '(("$dynamicAnchor" . "node") ("type" . "string"))
) ;check

;; 锚点名不匹配时返回 #f。

(check-false (walk-dynamic-anchor document "other" #t))

;; 非根节点自带 $id 时不继续向下查找。

(define scoped
  '(("$defs"
     ("a"
      ("$id" . "https://example.com/other.json")
      ("$dynamicAnchor" . "node"))))
) ;define
(check-false (walk-dynamic-anchor scoped "node" #t))

(check-report)

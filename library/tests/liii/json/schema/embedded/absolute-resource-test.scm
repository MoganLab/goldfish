;; absolute-resource
;; 计算带绝对 $id 的资源。
;;
;; 语法
;; ----
;; (absolute-resource schema base)
;;
;; 参数
;; ----
;; schema : any?
;; 待处理的 Schema 对象。
;;
;; base : any?
;; 基准资源，提供 $id 作为解析基准。
;;
;; 返回值
;; ----
;; any?
;; $id 已解析为绝对 URI 的 Schema 对象。
;;
;; 注意
;; ----
;; 基准资源与当前资源是同一对象且其 $id 已是绝对 URI 时原样返回；
;; 当前资源的 $id 已经是基准 $id 的后缀时也原样返回。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema embedded))

(check-set-mode! 'report-failed)

(define base '(("$id" . "https://example.com/root/schema.json")))

;; 相对 $id 被解析为绝对 URI。

(check (absolute-resource '(("$id" . "child.json")) base)
  =>
  '(("$id" . "https://example.com/root/child.json"))
) ;check

;; 已经是绝对 $id 的资源原样返回。

(check (absolute-resource base base) => base)
(check (absolute-resource '(("$id" . "https://other.example.com/x")) base)
  =>
  '(("$id" . "https://other.example.com/x"))
) ;check

;; 没有 $id 的资源原样返回。

(check (absolute-resource '(("type" . "object")) base)
  =>
  '(("type" . "object"))
) ;check

(check-report)

;; uri-fragment
;; 取出 URI 中的 fragment 部分。
;;
;; 语法
;; ----
;; (uri-fragment uri)
;;
;; 参数
;; ----
;; uri : string
;; 待解析的 URI 字符串。
;;
;; 返回值
;; ----
;; string | boolean
;; 存在 fragment 时返回 fragment 文本，不存在时返回 #f。
;;
;; 注意
;; ----
;; 返回值不包含 # 前缀。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema pointer))

(check-set-mode! 'report-failed)

(check (uri-fragment "https://example.com/schema#") => "")
(check (uri-fragment "https://example.com/schema#/$defs/x") => "/$defs/x")
(check (uri-fragment "https://example.com/schema#anchor") => "anchor")

;; 没有 fragment 时返回 #f。

(check-false (uri-fragment "https://example.com/schema"))
(check-false (uri-fragment ""))

(check-report)

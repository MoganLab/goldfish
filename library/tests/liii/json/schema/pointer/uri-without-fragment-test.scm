;; uri-without-fragment
;; 去掉 URI 中的 fragment 部分。
;;
;; 语法
;; ----
;; (uri-without-fragment uri)
;;
;; 参数
;; ----
;; uri : string
;; 待处理的 URI 字符串。
;;
;; 返回值
;; ----
;; string
;; 去掉 fragment 之后的 URI 文本。
;;
;; 注意
;; ----
;; 原串没有 fragment 时返回原串。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema pointer))

(check-set-mode! 'report-failed)

(check (uri-without-fragment "https://example.com/schema#abc")
  =>
  "https://example.com/schema"
) ;check
(check (uri-without-fragment "https://example.com/schema#")
  =>
  "https://example.com/schema"
) ;check

;; 没有 fragment 时返回原串。

(check (uri-without-fragment "https://example.com/schema")
  =>
  "https://example.com/schema"
) ;check
(check (uri-without-fragment "") => "")

(check-report)

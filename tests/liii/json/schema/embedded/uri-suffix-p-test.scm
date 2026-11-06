;; uri-suffix?
;; 判断 URI 是否以指定后缀结尾。
;;
;; 语法
;; ----
;; (uri-suffix? uri suffix)
;;
;; 参数
;; ----
;; uri : string
;; 待检查的 URI 字符串。
;;
;; suffix : string
;; 待匹配的后缀。
;;
;; 返回值
;; ----
;; boolean
;; 以该后缀结尾返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 用于比较 $id 与 $ref 的相对关系，
;; 例如判断子资源是否位于基准资源之下。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema embedded))

(check-set-mode! 'report-failed)

(check-true (uri-suffix? "https://example.com/root/schema.json" "schema.json"))
(check-true (uri-suffix? "https://example.com/root/schema.json" "/root/schema.json")
) ;check-true
(check-true (uri-suffix? "abc" "abc"))

(check-false (uri-suffix? "https://example.com/root/schema.json" "other.json"))
(check-false (uri-suffix? "abc" "abcd"))

(check-report)

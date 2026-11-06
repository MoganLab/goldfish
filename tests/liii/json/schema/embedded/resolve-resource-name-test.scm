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

;; 相对路径中的 ../ 会被正确折叠。

(check (resolve-resource-name "../up/" '(("$id" . "http://h/draft/a/b.json")))
  =>
  "http://h/draft/up/"
) ;check

;; 目录型合并结果可以作为后续合并的 base，不替换目录段。

(check (resolve-resource-name "file.json" '(("$id" . "http://h/draft/baseUriChange/")))
  =>
  "http://h/draft/baseUriChange/file.json"
) ;check

;; 以斜杠结尾的相对引用保留结尾斜杠（RFC 3986 5.2.3）。

(check (resolve-resource-name "baseUriChange/" root)
  =>
  "https://example.com/root/baseUriChange/"
) ;check

(check-report)

;; make-uri-record
;; 使用位置参数或具名关键字参数创建 URI 记录。
;;
;; 语法
;; ----
;; (make-uri-record [scheme] [netloc] [path] [query] [fragment])
;; (make-uri-record [:scheme scheme] [:netloc netloc] [:path path] [:query query] [:fragment fragment])
;;
;; 参数
;; ----
;; scheme : string? 或 #f（默认 #f）
;;   URI 协议方案，如 "http", "https", "ftp" 等。
;; netloc : string?（默认 ""）
;;   网络位置字符串，可包含 user, password, host, port。
;; path : string?（默认 ""）
;;   路径部分。
;; query : alist?（默认 '()）
;;   查询参数列表，如 '(("a" . "1") ("b" . "2"))。
;; fragment : string? 或 #f（默认 #f）
;;   片段标识符。
;;
;; 返回值
;; ----
;; uri?
;;   新创建的 URI 记录对象。
;;
;; 说明
;; ----
;; make-uri-record 创建 URI 记录，支持全默认值无参构造、位置参数构造与具名参数构造。

(import (liii check) (liii uri record))

(check-set-mode! 'report-failed)

;; 1. 基本位置参数构造
(check-true (uri? (make-uri-record "https" "example.com" "/path" '() #f)))

;; 2. 完整组件构造

(define u
  (make-uri-record "https"
    "user@example.com:8080"
    "/path/to/file"
    '(("a" . "1") ("b" . "2"))
    "section"
  ) ;make-uri-record
) ;define
(check (uri-scheme u) => "https")
(check (uri-authority u) => "user@example.com:8080")
(check (uri-path u) => "/path/to/file")
(check (uri-query u) => '(("a" . "1") ("b" . "2")))
(check (uri-fragment u) => "section")

;; 3. 具名关键字参数构造

(define u-kw
  (make-uri-record
    :scheme   "http"
    :netloc   "localhost:8080"
    :path     "/api"
    :query    '(("page" . "1"))
    :fragment "top"
  ) ;make-uri-record
) ;define
(check (uri-scheme u-kw) => "http")
(check (uri-authority u-kw) => "localhost:8080")
(check (uri-path u-kw) => "/api")
(check (uri-query u-kw) => '(("page" . "1")))
(check (uri-fragment u-kw) => "top")

;; 4. 默认值与无参构造

(define u-empty (make-uri-record))
(check-true (uri? u-empty))
(check (uri-scheme u-empty) => #f)
(check (uri-authority u-empty) => "")
(check (uri-path u-empty) => "")
(check (uri-query u-empty) => '())
(check (uri-fragment u-empty) => #f)

;; 5. 部分具名参数构造

(define u-part (make-uri-record :scheme "https" :path "/index.html"))
(check (uri-scheme u-part) => "https")
(check (uri-authority u-part) => "")
(check (uri-path u-part) => "/index.html")

(check-report)

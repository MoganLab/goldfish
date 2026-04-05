(import (liii check)
        (liii uri-record)
        (liii uri-make)
) ;import

(check-set-mode! 'report-failed)

;; uri-build
;; 使用关键字参数从组件构建 URI。
;;
;; 语法
;; ----
;; (uri-build #:key scheme user password host port path query fragment)
;;
;; 返回值
;; ----
;; uri?
;;   返回构建的 URI 记录。

;; 简单构建
(define u1 (uri-build :scheme "https" :host "example.com"))
(check (uri-scheme u1) => "https")
(check (uri-host u1) => "example.com")
(check (uri-path u1) => "")

;; 带路径
(define u2 (uri-build :scheme "http"
                      :host "api.example.com"
                      :path "/v1/users"))
(check (uri-scheme u2) => "http")
(check (uri-host u2) => "api.example.com")
(check (uri-path u2) => "/v1/users")

;; 带端口和认证
(define u3 (uri-build :scheme "https"
                      :user "admin"
                      :password "secret"
                      :host "db.example.com"
                      :port 5432))
(check (uri-scheme u3) => "https")
(check (uri-user u3) => "admin")
(check (uri-password u3) => "secret")
(check (uri-host u3) => "db.example.com")
(check (uri-explicit-port u3) => 5432)

;; 带查询和 fragment
(define u4 (uri-build :scheme "https"
                      :host "search.com"
                      :path "/query"
                      :query '(("q" . "hello") ("page" . "1"))
                      :fragment "results"))
(check (uri-host u4) => "search.com")
(check (uri-path u4) => "/query")
(check (uri-query-ref u4 "q") => "hello")
(check (uri-query-ref u4 "page") => "1")
(check (uri-fragment u4) => "results")

;; 完整构建
(define u5 (uri-build :scheme "https"
                      :user "user"
                      :password "pass"
                      :host "api.example.com"
                      :port 8443
                      :path "/v2/resource"
                      :query '(("id" . "123") ("format" . "json"))
                      :fragment "section"))
(check (uri-scheme u5) => "https")
(check (uri-user u5) => "user")
(check (uri-password u5) => "pass")
(check (uri-host u5) => "api.example.com")
(check (uri-explicit-port u5) => 8443)
(check (uri-path u5) => "/v2/resource")
(check (uri-query-ref u5 "id") => "123")
(check (uri-query-ref u5 "format") => "json")
(check (uri-fragment u5) => "section")

;; 空值处理
(define u6 (uri-build))
(check (uri? u6) => #t)
(check (uri-scheme u6) => #f)
(check (uri-host u6) => #f)

(check-report)

(import (liii check)
        (liii uri-record)
) ;import

(check-set-mode! 'report-failed)

;; make-uri-raw
;; 从组件创建原始 URI 记录。
;;
;; 语法
;; ----
;; (make-uri-raw scheme netloc path query fragment)
;;
;; 参数
;; ----
;; scheme : string? 或 #f
;;   URI 协议方案，如 "http", "https", "ftp" 等。
;; netloc : string?
;;   网络位置字符串，可包含 user, password, host, port。
;; path : string?
;;   路径部分。
;; query : alist?
;;   查询参数列表，如 '(("a" . "1") ("b" . "2"))。
;; fragment : string? 或 #f
;;   片段标识符。
;;
;; 返回值
;; ----
;; uri?
;;   新创建的 URI 记录对象。
;;
;; 说明
;; ----
;; make-uri-raw 创建原始 URI 记录，不进行任何编码或验证。
;; 如需从字符串解析 URI，请使用 make-uri。

;; 基本构造
(check (uri? (make-uri-raw "https" "example.com" "/path" '() #f)) => #t)

;; 完整组件
(define u (make-uri-raw "https" "user@example.com:8080" "/path/to/file"
                        '(("a" . "1") ("b" . "2")) "section")
) ;define
(check (uri-scheme-raw u) => "https")
(check (uri-netloc-raw u) => "user@example.com:8080")
(check (uri-path-raw u) => "/path/to/file")
(check (uri-query-raw u) => '(("a" . "1") ("b" . "2")))
(check (uri-fragment-raw u) => "section")

;; 空值处理
(check (uri? (make-uri-raw #f "" "" '() #f)) => #t)

(check-report)

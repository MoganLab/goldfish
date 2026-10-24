(import (liii check) (liii uri) (liii uri compare) (liii uri convert)
  (liii uri transform))


(check-set-mode! 'report-failed)


;; uri-normalize
;; 按 RFC 3986 6.2.3 节做基于协议的语法归一化：
;; - 网络协议（http/https 等有默认端口）的空 path 归一化为 "/"
;; - 移除等于协议默认端口的显式端口
;;
;; 语法
;; ----
;; (uri-normalize uri-obj)
;;
;; 返回值
;; ----
;; uri?
;;   返回归一化后的新 URI，原 URI 不变。


;; 空 path 归一化为 "/"

(check (uri->string (uri-normalize (make-uri "https://api.anthropic.com")))
  => "https://api.anthropic.com/")


;; 显式默认端口 443 被移除

(check (uri->string (uri-normalize (make-uri "https://example.com:443/v1")))
  => "https://example.com/v1")


;; 显式默认端口 80 被移除

(check (uri->string (uri-normalize (make-uri "http://example.com:80/a")))
  => "http://example.com/a")


;; 非默认端口保留

(check (uri->string (uri-normalize (make-uri "https://example.com:8443/a")))
  => "https://example.com:8443/a")


;; 已归一化的 URI 保持不变

(check (uri->string (uri-normalize (make-uri "https://example.com/a")))
  => "https://example.com/a")


;; 非网络协议（mailto 等）不做 path 归一化

(check (uri->string (uri-normalize (make-uri "mailto:bob@example.com")))
  => "mailto:bob@example.com")


;; 与 uri=? 配合使用：空 path 与 "/" 视为等价

(check (uri=? (uri-normalize (make-uri "https://api.minimax.io"))
            (uri-normalize (make-uri "https://api.minimax.io/")))
  => #t)


;; 与 uri=? 配合使用：默认端口与无端口视为等价

(check (uri=? (uri-normalize (make-uri "https://api.deepseek.com"))
            (uri-normalize (make-uri "https://api.deepseek.com:443")))
  => #t)


;; path 不同则不等价

(check (uri=? (uri-normalize (make-uri "https://api.kimi.com/coding"))
            (uri-normalize (make-uri "https://api.kimi.com/coding/v1")))
  => #f)


(check-report)

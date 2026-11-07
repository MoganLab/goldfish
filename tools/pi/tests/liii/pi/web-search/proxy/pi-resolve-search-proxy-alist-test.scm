;; pi-resolve-search-proxy-alist
;; 将代理地址转换为 HTTP 客户端所需的代理 alist。
;;
;; 语法
;; ----
;; (pi-resolve-search-proxy-alist proxy-url)
;;
;; 参数
;; ----
;; proxy-url : (or string? boolean?)
;; 代理地址字符串。
;;
;; 返回值
;; ----
;; list?
;; 包含 http 与 https 协议键的代理 alist，无代理时返回空列表。

(import (liii check)
        (liii pi web-search proxy)
) ;import

(check-set-mode! 'report-failed)

;; 1. 有效代理地址同时映射 http 与 https
(check (pi-resolve-search-proxy-alist "http://127.0.0.1:7890")
  => '(("http" . "http://127.0.0.1:7890") ("https" . "http://127.0.0.1:7890"))
) ;check

;; 2. 无效或空代理返回空列表
(check (pi-resolve-search-proxy-alist #f) => '())
(check (pi-resolve-search-proxy-alist "") => '())
(check (pi-resolve-search-proxy-alist "   ") => '())

(check-report)

;; openai-build-proxy-alist
;; 根据代理 URL 构造 HTTP/HTTPS 代理配置表。
;;
;; 语法
;; ----
;; (openai-build-proxy-alist proxy-url)
;;
;; 参数
;; ----
;; proxy-url : string? | #f
;; 代理服务器 URL。
;;
;; 返回值
;; ----
;; alist
;; 包含 http 与 https 代理的 alist；无代理时返回空列表。

(import (liii check)
  (liii pi ai api openai-completions request)
) ;import

(check-set-mode! 'report-failed)

;; 正常代理 URL
(let ((proxy (openai-build-proxy-alist "http://127.0.0.1:7890")))
  (check (cdr (assoc "http" proxy)) => "http://127.0.0.1:7890")
  (check (cdr (assoc "https" proxy)) => "http://127.0.0.1:7890")
) ;let

;; 空字符串或 #f
(check (openai-build-proxy-alist "") => '())
(check (openai-build-proxy-alist #f) => '())

(check-report)

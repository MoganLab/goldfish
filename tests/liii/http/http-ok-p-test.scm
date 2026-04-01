(import (liii check)
        (liii http)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env) (exit 0))
) ;let

;; http-ok?
;; 检查 HTTP 响应是否成功（状态码 2xx）。
;;
;; 语法
;; ----
;; (http-ok? response)
;;
;; 参数
;; ----
;; response : response-object
;;   HTTP 响应对象。
;;
;; 返回值
;; ----
;; boolean
;;   成功返回 #t，否则抛出错误。
;;
;; 描述
;; ----
;; 检查响应状态码：
;; - 2xx 范围：返回 #t
;; - 4xx 范围：抛出 'http-error 错误（客户端错误）
;; - 5xx 范围：抛出 'http-error 错误（服务器错误）
;;
;; 错误处理
;; ----
;; http-error
;;   当状态码为 4xx 或 5xx 时抛出，包含状态码、原因和 URL。

(let ((r (http-head "https://httpbin.org")))
  (check-true (http-ok? r))
) ;let

(check-report)

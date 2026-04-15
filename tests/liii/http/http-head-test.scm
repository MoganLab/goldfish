(import (liii check)
  (liii http)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; 环境检查：需要设置 GOLDFISH_TEST_HTTP 环境变量才执行测试
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env)
    (exit 0)
  ) ;when
) ;let

;; http-head
;; 发送 HTTP HEAD 请求，返回响应对象。
;;
;; 语法
;; ----
;; (http-head url)
;;
;; 参数
;; ----
;; url : string
;;   请求的目标 URL。
;;
;; 返回值
;; ----
;; response-object
;;   响应对象，可通过以下字段访问：
;;   - 'status-code : 整数 HTTP 状态码
;;   - 'url         : 字符串，最终 URL（可能有重定向）
;;   - 'reason      : 字符串，状态描述（如 "OK"）
;;   - 'text        : 字符串，响应体（HEAD 请求通常为空）
;;   - 'elapsed     : 实数，请求耗时（秒）
;;   - 'headers     : hash-table，响应头
;;
;; 描述
;; ----
;; HEAD 请求与 GET 类似，但服务器不返回响应体。适合检查资源是否存在、
;; 获取元数据（如 Content-Length、Last-Modified）而不下载完整内容。

(let ((r (http-head "https://httpbin.org")))
  (check (r 'status-code) => 200)
  (check (r 'url)
    =>
    "https://httpbin.org/"
  ) ;check
  (check-true (real? (r 'elapsed)))
  ;; NOTE: httpbin.org's LB routes to different backends.
  ;;       Some return "OK", others empty string for reason.
  ;;       HTTP/2+ allows omitting reason phrases.
  (check-true (or (equal? (r 'reason) "OK")
                (equal? (r 'reason) "")
              ) ;or
  ) ;check-true
  (check (r 'text) => "")
  (check ((r 'headers) "content-type")
    =>
    "text/html; charset=utf-8"
  ) ;check
  (check ((r 'headers) "content-length")
    =>
    "9593"
  ) ;check
) ;let

(check-report)

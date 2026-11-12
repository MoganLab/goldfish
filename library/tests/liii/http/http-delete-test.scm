;; http-delete
;; 发送 HTTP DELETE 请求，返回响应对象。
;;
;; 语法
;; ----
;; (http-delete url :timeout timeout)
;;
;; 参数
;; ----
;; url : string
;;   请求的目标 URL。
;; timeout : real? | integer? | #f (可选)
;;   总时长超时（单位：秒）：整个请求的最长等待时间，到点必中止。
;;
;; 返回值
;; ----
;; response-object
;;   响应对象，可通过以下字段访问：
;;   - 'status-code : 整数 HTTP 状态码
;;   - 'url         : 字符串，最终 URL（可能有重定向）
;;   - 'reason      : 字符串，状态描述（如 "OK"）
;;   - 'text        : 字符串，响应体
;;   - 'elapsed     : 实数，请求耗时（秒）
;;   - 'headers     : hash-table，响应头
;;
;; 描述
;; ----
;; DELETE 请求用于请求服务器删除由 Request-URI 所标识的资源。
;; 返回包含状态码、响应头与响应体的哈希表。

(import (liii check) (liii http) (liii os))

(check-set-mode! 'report-failed)

;; 公开接口 http-delete 参数类型测试 (devel/0149.md)
(check-catch 'type-error (http-delete 42))
(check-catch 'type-error (http-delete #\a))
(check-catch 'type-error (http-delete 'invalid))
(check-catch 'type-error (http-delete "http://x/" :timeout "5"))
(check-catch 'type-error (http-delete "http://x/" :timeout 'fast))
(check-catch 'type-error (http-delete "http://x/" :timeout #t))
(check-catch 'value-error (http-delete "http://x/" :timeout 0))
(check-catch 'value-error (http-delete "http://x/" :timeout -1))

;; C 层入口 g_http-delete 参数测试
(check-catch 'type-error (g_http-delete 42))
(check-catch 'type-error (g_http-delete "http://x/" "5"))
(check-catch 'type-error (g_http-delete "http://x/" 'fast))
(check-catch 'value-error (g_http-delete "http://x/" 0))
(check-catch 'value-error (g_http-delete "http://x/" -1))

;; 环境检查：需要设置 GOLDFISH_TEST_HTTP 环境变量才执行网络测试
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env)
    (check-report)
    (exit 0)
  ) ;when
) ;let

(let ((r (http-delete "https://httpbin.org/delete")))
  (check (r 'status-code) => 200)
  (check (r 'url) => "https://httpbin.org/delete")
  (check-true (real? (r 'elapsed)))
  (check-true (string? (r 'text)))
  (check-true (> (string-length (r 'text)) 0))
  (check ((r 'headers) "content-type") => "application/json")
) ;let

(check-report)

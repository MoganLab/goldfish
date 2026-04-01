(import (liii check)
        (liii http)
        (liii string)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env) (exit 0))
) ;let

;; http-get
;; 发送 HTTP GET 请求，返回响应对象。
;;
;; 语法
;; ----
;; (http-get url :params params :headers headers :proxy proxy)
;;
;; 参数
;; ----
;; url     : string          - 请求的目标 URL
;; params  : alist (可选)    - 查询参数列表，如 '(("key" . "value"))
;; headers : alist (可选)    - 请求头列表
;; proxy   : alist (可选)    - 代理配置
;;
;; 返回值
;; ----
;; response-object
;;   响应对象，可通过 'status-code、'text、'headers 等字段访问。
;;
;; 描述
;; ----
;; GET 请求用于从服务器获取资源。查询参数会自动编码并附加到 URL。

;; 基本 GET 请求
(let ((r (http-get "https://httpbin.org")))
  (check (r 'status-code) => 200)
  (check-true (> (string-length (r 'text)) 0))
  (check ((r 'headers) "content-type") => "text/html; charset=utf-8")
) ;let

;; 带查询参数的 GET 请求
(let ((r (http-get "https://httpbin.org/get"
                  :params '(("key1" . "value1") ("key2" . "value2")))))
  (check-true (string-contains (r 'text) "value1"))
  (check-true (string-contains (r 'text) "value2"))
  (check (r 'url) => "https://httpbin.org/get?key1=value1&key2=value2")
) ;let

(check-report)

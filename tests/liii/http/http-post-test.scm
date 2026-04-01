(import (liii check)
        (liii http)
        (liii string)
        (liii json)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env) (exit 0))
) ;let

;; http-post
;; 发送 HTTP POST 请求，返回响应对象。
;;
;; 语法
;; ----
;; (http-post url :params params :data data :headers headers :proxy proxy)
;;
;; 参数
;; ----
;; url     : string          - 请求的目标 URL
;; params  : alist (可选)    - 查询参数列表
;; data    : string (可选)   - 请求体数据
;; headers : alist (可选)    - 请求头列表
;; proxy   : alist (可选)    - 代理配置
;;
;; 返回值
;; ----
;; response-object
;;   响应对象。
;;
;; 描述
;; ----
;; POST 请求用于向服务器提交数据。当 data 非空且未指定 headers 时，
;; 默认 Content-Type 为 text/plain。

;; 带查询参数的 POST 请求
(let ((r (http-post "https://httpbin.org/post"
                   :params '(("key1" . "value1") ("key2" . "value2")))))
  (check-true (string-contains (r 'text) "value1"))
  (check-true (string-contains (r 'text) "value2"))
  (check (r 'status-code) => 200)
  (check (r 'url) => "https://httpbin.org/post?key1=value1&key2=value2")
) ;let

;; 带原始数据的 POST 请求
(let* ((r (http-post "https://httpbin.org/post"
            :data "This is raw data"))
       (json (string->json (r 'text))))
  (check (r 'status-code) => 200)
  (check (json-ref json "data") => "This is raw data")
) ;let*

(check-report)

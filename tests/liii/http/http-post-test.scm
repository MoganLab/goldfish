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
;; (http-post url :params params :data data :headers headers :proxy proxy :files files)
;;
;; 参数
;; ----
;; url     : string          - 请求的目标 URL
;; params  : alist (可选)    - 查询参数列表
;; data    : string/alist (可选) - 原始请求体；若配合 files 使用，则表示 multipart 文本字段
;; headers : alist (可选)    - 请求头列表
;; proxy   : alist (可选)    - 代理配置
;; files   : alist (可选)    - 文件上传配置，触发 multipart/form-data
;;
;; 返回值
;; ----
;; response-object
;;   响应对象。
;;
;; files 结构
;; ----
;; 简单文件：
;;   '(("upload" . "tests/resources/http-upload.txt"))
;;
;; 完整文件描述：
;;   '(("upload" . ((file . "tests/resources/http-upload.txt")
;;                  (filename . "fixture.txt")
;;                  (content-type . "text/plain"))))
;;
;; 描述
;; ----
;; POST 请求用于向服务器提交数据。
;; 1. 当 files 为空时，data 按原有语义作为原始请求体处理
;; 2. 当 files 非空时，http-post 自动切换为 multipart/form-data
;;    - data 需为 alist，用作普通表单字段
;;    - files 用作文件字段
;; 3. 文件上传由 http-post 本身处理，不再提供单独的 multipart POST API
;; 4. 当 data 非空且未指定 headers 时，默认 Content-Type 为 text/plain。

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

;; 带文件上传的 POST 请求
(let* ((r (http-post "https://httpbin.org/post"
            :data '(("meta" . "multipart-value"))
            :files '(("upload" . ((file . "tests/resources/http-upload.txt")
                                  (filename . "fixture.txt")
                                  (content-type . "text/plain"))))
            :params '(("kind" . "multipart"))))
       (json (string->json (r 'text))))
  (check (r 'status-code) => 200)
  (check (r 'url) => "https://httpbin.org/post?kind=multipart")
  (check (json-ref json "form" "meta") => "multipart-value")
  (check (json-ref json "files" "upload") => "Goldfish multipart upload fixture.\n")
  (check-true (string-contains (json-ref json "headers" "Content-Type")
                               "multipart/form-data"))
) ;let*

(check-catch 'type-error
  (http-post "https://httpbin.org/post"
    :data '(("meta" . "invalid-header-test"))
    :files '(("upload" . "tests/resources/http-upload.txt"))
    :headers '(("token" . #f)))
) ;check-catch

(check-catch 'type-error
  (http-post "https://httpbin.org/post"
    :data "raw body is not allowed with files"
    :files '(("upload" . "tests/resources/http-upload.txt")))
) ;check-catch

(check-catch 'value-error
  (http-post "https://httpbin.org/post"
    :files '(("upload" . ((filename . "fixture.txt")))))
) ;check-catch

(check-report)

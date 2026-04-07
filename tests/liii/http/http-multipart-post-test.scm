(import (liii check)
        (liii http)
        (liii string)
        (liii json)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; http-multipart-post
;; 发送 multipart/form-data POST 请求，可同时上传普通字段和文件。
;;
;; 语法
;; ----
;; (http-multipart-post url parts :params params :headers headers :proxy proxy)
;;
;; 参数
;; ----
;; url     : string          - 请求的目标 URL
;; parts   : list            - multipart part 列表
;; params  : alist (可选)    - 查询参数列表
;; headers : alist (可选)    - 请求头列表
;; proxy   : alist (可选)    - 代理配置
;;
;; part 结构
;; ----
;; 文本字段：
;;   '((name . "meta") (value . "release"))
;;
;; 文件字段：
;;   '((name . "artifact")
;;     (file . "tests/resources/http-upload.txt")
;;     (filename . "fixture.txt")
;;     (content-type . "text/plain"))
;;
;; 返回值
;; ----
;; response-object
;;   响应对象，可通过 'status-code、'text、'headers 等字段访问。

(check-catch 'type-error
  (http-multipart-post "https://httpbin.org/post"
    '(((name . "meta") (value . "invalid-header-test")))
    :headers '(("token" . #f)))
) ;check-catch

(let* ((r (http-multipart-post "https://httpbin.org/post"
            '(((name . "meta") (value . "multipart-value"))
              ((name . "upload")
               (file . "tests/resources/http-upload.txt")
               (filename . "fixture.txt")
               (content-type . "text/plain")))
            :params '(("kind" . "multipart"))))
       (json (string->json (r 'text))))
  (check (r 'status-code) => 200)
  (check (r 'url) => "https://httpbin.org/post?kind=multipart")
  (check (json-ref json "form" "meta") => "multipart-value")
  (check (json-ref json "files" "upload") => "Goldfish multipart upload fixture.\n")
  (check-true (string-contains (json-ref json "headers" "Content-Type")
                               "multipart/form-data"))
) ;let*


(check-report)

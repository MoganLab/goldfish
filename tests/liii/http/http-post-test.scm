(import (liii check)
        (liii http)
        (liii string)
        (liii json)
        (liii os)
) ;import

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env) (exit 0))
) ;let

(check-set-mode! 'report-failed)



;; http-post
;; 发送 HTTP POST 请求，返回响应对象。
;;
;; 语法
;; ----
;; (http-post url :params params :data data :headers headers :proxy proxy
;;            :files files :output-file output-file :stream stream :callback callback)
;;
;; 参数
;; ----
;; url     : string          - 请求的目标 URL
;; params  : alist (可选)    - 查询参数列表
;; data    : string/alist (可选) - 原始请求体；若配合 files 使用，则表示 multipart 文本字段
;; headers : alist (可选)    - 请求头列表
;; proxy   : alist (可选)    - 代理配置
;; files   : alist (可选)    - 文件上传配置，触发 multipart/form-data
;; output-file: string (可选)   - stream 模式下将响应体按 chunk 写入本地文件
;; stream     : boolean (可选)  - 启用简单流式模式
;; callback   : procedure (可选)- 按 chunk 处理响应体 (lambda (chunk) ...)
;;
;; 返回值
;; ----
;; stream 为 #f 时返回 response-object；
;; stream 为 #t 时返回 #<undefined>。
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
;; 3. 当 :stream 为 #t 时，按 chunk 处理响应体
;; 4. 文件上传由 http-post 本身处理，不再提供单独的 multipart POST API
;; 5. 当 data 非空且未指定 headers 时，默认 Content-Type 为 text/plain。

(define simpletex-api-key "你的api-key")



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

;; 通过 :stream #t + :callback 流式处理 POST 响应
(let ((collected '()))
  (let ((r (http-post "https://httpbin.org/post"
                     :stream #t
                     :params '(("param1" . "value1"))
                     :data "{\"test\": \"streaming-json\"}"
                     :headers '(("Content-Type" . "application/json"))
                     :callback (lambda (chunk)
                                 (when (> (string-length chunk) 0)
                                   (set! collected (cons chunk collected))
                                 ) ;when
                                 #t
                               ) ;lambda
           )))
    (check-true (undefined? r))
    (check-true (> (length collected) 0))
    (let ((response (string-join (reverse collected) "")))
      (check-true (string-contains response "streaming-json"))
      (check-true (string-contains response "value1"))
    ) ;let
  ) ;let
) ;let

;; 通过 :stream #t + :output-file 将 POST 响应按 chunk 写入本地文件
(let* ((output-file "/tmp/goldfish-http-post-download.txt")
       (r (begin
            (when (file-exists? output-file)
              (delete-file output-file)
            ) ;when
            (http-post "https://httpbin.org/post"
                      :stream #t
                      :data "Simple streaming POST test"
                      :output-file output-file
            ) ;http-post
          ) ;begin
       )
       ) ;r
  (check-true (undefined? r))
  (check-true (file-exists? output-file))
  (let ((body (call-with-input-file output-file
                (lambda (port)
                  (read-string 4096 port)
                ) ;lambda
              ) ;call-with-input-file
        ))
    (check-true (string-contains body "Simple streaming POST test"))
  ) ;let
  (delete-file output-file)
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
                               "multipart/form-data")
  ) ;check-true
) ;let*

;; 通过 :stream #t 流式处理 multipart POST 响应
(let ((collected '()))
  (let ((r (http-post "https://httpbin.org/post"
                     :stream #t
                     :data '(("meta" . "multipart-value"))
                     :files '(("upload" . ((file . "tests/resources/http-upload.txt")
                                           (filename . "fixture.txt")
                                           (content-type . "text/plain"))))
                     :params '(("kind" . "multipart-stream"))
                     :callback (lambda (chunk)
                                 (when (> (string-length chunk) 0)
                                   (set! collected (cons chunk collected))
                                 ) ;when
                                 #t
                               ) ;lambda
           )))
    (check-true (undefined? r))
    (check-true (> (length collected) 0))
    (let ((response (string-join (reverse collected) "")))
      (check-true (string-contains response "multipart-value"))
      (check-true (string-contains response "Goldfish multipart upload fixture."))
      (check-true (string-contains response "multipart/form-data"))
    ) ;let
  ) ;let
) ;let

(check-catch 'type-error
  (http-post "https://httpbin.org/post"
    :data '(("meta" . "invalid-header-test"))
    :files '(("upload" . "tests/resources/http-upload.txt"))
    :headers '(("token" . #f))
  ) ;http-post
) ;check-catch

(check-catch 'type-error
  (http-post "https://httpbin.org/post"
    :data "raw body is not allowed with files"
    :files '(("upload" . "tests/resources/http-upload.txt"))
  ) ;http-post
) ;check-catch

(check-catch 'value-error
  (http-post "https://httpbin.org/post"
    :files '(("upload" . ((filename . "fixture.txt"))))
  ) ;http-post
) ;check-catch

;; 直接调用 SimpleTex 官方公式识别接口
(if (or (= (string-length simpletex-api-key) 0)
        (string=? simpletex-api-key "你的api-key"))
    (display "SKIP SimpleTex OCR integration: fill simpletex-api-key in this test file to enable live API assertions.\n")
    (let* ((r (http-post "https://server.simpletex.cn/api/latex_ocr_turbo"
                :headers `(("token" . ,simpletex-api-key))
                :files '(("file" . ((file . "tests/resources/simpletex-formula-a2-b2.png")
                                    (filename . "simpletex-formula-a2-b2.png")
                                    (content-type . "image/png"))))))
           (json (string->json (r 'text))))
      (check (r 'status-code) => 200)
      (check (json-ref json "status") => #t)
      (check-true (string? (json-ref json "res" "latex")))
      (check-true (> (string-length (json-ref json "res" "latex")) 0))
    ) ;let*
) ;if

(check-report)

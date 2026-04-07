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

;; http-stream-post
;; 流式发送 HTTP POST 请求，通过回调函数处理数据块。
;;
;; 语法
;; ----
;; (http-stream-post url callback :params params
;;                   :data data :headers headers :proxy proxy)
;;
;; 参数
;; ----
;; url      : string          - 请求的目标 URL
;; callback : procedure       - 回调函数 (lambda (chunk) ...)
;; params   : alist (可选)    - 查询参数列表
;; data     : string (可选)   - 请求体数据
;; headers  : alist (可选)    - 请求头列表
;; proxy    : alist (可选)    - 代理配置
;;
;; 返回值
;; ----
;; 无
;;
;; 描述
;; ----
;; 流式 POST 适合上传大文件或处理实时响应。

;; 测试流式 POST 与 JSON 数据
(let ((collected '()))
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))
                     ) ;when
                   ) ;lambda
                   :params '(("param1" . "value1"))
                   :data "{\"test\": \"streaming-json\"}"
                   :headers '(("Content-Type" . "application/json"))
  ) ;http-stream-post
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "streaming-json"))
  ) ;let
) ;let

;; 测试流式 POST 与纯文本
(let ((collected '()))
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))
                     ) ;when
                   ) ;lambda
                   :data "Simple streaming POST test"
  ) ;http-stream-post
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "Simple streaming POST test"))
  ) ;let
) ;let

;; 测试流式 POST 与 XML 数据
(let ((collected '()))
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))
                     ) ;when
                   ) ;lambda
                   :data "<root><message>stream-xml-test</message></root>"
                   :headers '(("Content-Type" . "application/xml"))
  ) ;http-stream-post
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "stream-xml-test"))
  ) ;let
) ;let

;; 测试流式 POST 与表单数据
(let ((collected '()))
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))
                     ) ;when
                   ) ;lambda
                   :data "field1=stream-test&field2=form-data"
                   :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
  ) ;http-stream-post
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "stream-test"))
  ) ;let
) ;let

(check-report)

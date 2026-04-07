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

;; http-stream-get
;; 流式发送 HTTP GET 请求，通过回调函数处理数据块。
;;
;; 语法
;; ----
;; (http-stream-get url callback :params params :proxy proxy)
;;
;; 参数
;; ----
;; url      : string          - 请求的目标 URL
;; callback : procedure       - 回调函数 (lambda (chunk) ...)
;; params   : alist (可选)    - 查询参数列表
;; proxy    : alist (可选)    - 代理配置
;;
;; 返回值
;; ----
;; 无
;;
;; 描述
;; ----
;; 流式 GET 适合处理大响应或实时数据。数据到达时立即调用 callback，
;; 而不是等待完整响应。callback 接收一个参数：数据块字符串。

;; 测试流式 GET 与简单端点
(let ((collected '()))
  (http-stream-get "https://httpbin.org/get"
                  (lambda (chunk)
                    (when (> (string-length chunk) 0)
                      (set! collected (cons chunk collected))
                    ) ;when
                  ) ;lambda
                  :params '(("query" . "test_values") ("limit" . "10"))
  ) ;http-stream-get
  (check-true (> (length collected) 0))
) ;let

;; 测试流式 GET 与 JSON 端点
(let ((collected '()))
  (http-stream-get "https://jsonplaceholder.typicode.com/posts/1"
                  (lambda (chunk)
                    (when (> (string-length chunk) 0)
                      (set! collected (cons chunk collected))
                    ) ;when
                  ) ;lambda
  ) ;http-stream-get
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "userId"))
  ) ;let
) ;let

(check-report)

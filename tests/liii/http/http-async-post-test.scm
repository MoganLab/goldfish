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

;; http-async-post
;; 异步发送 HTTP POST 请求，通过回调函数处理响应。
;;
;; 语法
;; ----
;; (http-async-post url callback :params params :data data
;;                  :headers headers :proxy proxy)
;;
;; 参数
;; ----
;; url      : string          - 请求的目标 URL
;; callback : procedure       - 回调函数 (lambda (response) ...)
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
;; 异步 POST 请求立即返回，不阻塞。请求完成后调用 callback。

;; 测试异步 POST 请求
(let ((post-completed #f)
      (post-response #f))
  (http-async-post "https://httpbin.org/post"
    (lambda (response)
      (set! post-completed #t)
      (set! post-response response)
    ) ;lambda
    :params '()
    :data "{\"test\": \"async-post\"}"
    :headers '(("Content-Type" . "application/json"))
    :proxy '()
  ) ;http-async-post
  (http-wait-all 30)
  (check-true post-completed)
  (check (post-response 'status-code) => 200)
  (check-true (string-contains (post-response 'text) "async-post"))
) ;let

(check-report)

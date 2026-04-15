(import (liii check)
  (liii http)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env)
    (exit 0)
  ) ;when
) ;let

;; http-async-head
;; 异步发送 HTTP HEAD 请求，通过回调函数处理响应。
;;
;; 语法
;; ----
;; (http-async-head url callback :params params :headers headers :proxy proxy)
;;
;; 参数
;; ----
;; url      : string          - 请求的目标 URL
;; callback : procedure       - 回调函数 (lambda (response) ...)
;; params   : alist (可选)    - 查询参数列表
;; headers  : alist (可选)    - 请求头列表
;; proxy    : alist (可选)    - 代理配置
;;
;; 返回值
;; ----
;; 无
;;
;; 描述
;; ----
;; 异步 HEAD 请求立即返回，不阻塞。请求完成后调用 callback。
;; 适合并发检查多个资源的状态。

(let ((head-completed #f) (head-response #f))
  (http-async-head "https://httpbin.org"
    (lambda (response)
      (set! head-completed #t)
      (set! head-response response)
    ) ;lambda
  ) ;http-async-head
  (http-wait-all 30)
  (check-true head-completed)
  (check (head-response 'status-code)
    =>
    200
  ) ;check
) ;let

(check-report)

(import (liii check)
  (liii http)
  (liii string)
  (liii time)
  (liii os)
) ;import

(check-set-mode! 'report-failed)

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env)
    (exit 0)
  ) ;when
) ;let

;; http-async-get
;; 异步发送 HTTP GET 请求，通过回调函数处理响应。
;;
;; 语法
;; ----
;; (http-async-get url callback :params params :headers headers :proxy proxy)
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
;; 异步 GET 请求立即返回，不阻塞。请求完成后调用 callback。
;; 使用 http-wait-all 或 http-poll 等待或检查完成状态。
;; 多个异步请求可并发执行。

;; 测试异步 GET 请求
(let ((async-completed #f)
      (async-response #f)
     ) ;
  (http-async-get "https://httpbin.org/get"
    (lambda (response)
      (set! async-completed #t)
      (set! async-response response)
    ) ;lambda
  ) ;http-async-get
  (http-wait-all 30)
  (check-true async-completed)
  (check (async-response 'status-code)
    =>
    200
  ) ;check
  (check-true (string-contains (async-response 'text)
                "httpbin.org"
              ) ;string-contains
  ) ;check-true
) ;let

;; 测试多个并发异步请求
(let ((completed-count 0)
      (start-time (current-second))
     ) ;
  (http-async-get "https://httpbin.org/delay/1"
    (lambda (r)
      (set! completed-count
        (+ completed-count 1)
      ) ;set!
    ) ;lambda
  ) ;http-async-get
  (http-async-get "https://httpbin.org/delay/1"
    (lambda (r)
      (set! completed-count
        (+ completed-count 1)
      ) ;set!
    ) ;lambda
  ) ;http-async-get
  (http-async-get "https://httpbin.org/delay/1"
    (lambda (r)
      (set! completed-count
        (+ completed-count 1)
      ) ;set!
    ) ;lambda
  ) ;http-async-get
  (http-wait-all 30)
  (let ((elapsed (- (current-second) start-time)
        ) ;elapsed
       ) ;
    ;; All 3 requests should complete in ~1s (concurrent), not ~3s (sequential)
    (check completed-count => 3)
    ;; Allow some tolerance for network latency
    (check-true (< elapsed 5.0))
  ) ;let
) ;let

(check-report)

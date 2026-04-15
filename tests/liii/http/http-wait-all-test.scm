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

;; http-wait-all
;; 等待所有异步 HTTP 请求完成。
;;
;; 语法
;; ----
;; (http-wait-all :timeout timeout)
;;
;; 参数
;; ----
;; timeout : integer (可选)   - 超时时间（秒），负数表示无限等待
;;
;; 返回值
;; ----
;; 无
;;
;; 描述
;; ----
;; 阻塞直到所有异步请求完成或超时。请求完成后会执行对应的回调函数。
;; 超时时不会抛出错误，但可能仍有未完成的请求。
;;
;; 注意
;; ----
;; 如果没有活跃的异步请求，http-wait-all 立即返回。

(let ((completed #f))
  (http-async-get "https://httpbin.org/get"
    (lambda (response) (set! completed #t))
  ) ;http-async-get
  (http-wait-all 30)
  (check-true completed)
) ;let

(check-report)

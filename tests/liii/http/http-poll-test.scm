(import (liii check)
        (liii http)
        (liii time)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; 环境检查
(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env) (exit 0))
) ;let

;; http-poll
;; 轮询并执行已完成的异步 HTTP 请求回调。
;;
;; 语法
;; ----
;; (http-poll)
;;
;; 返回值
;; ----
;; integer
;;   本次轮询执行的回调数量。0 表示没有已完成的请求。
;;
;; 描述
;; ----
;; http-poll 是非阻塞的，立即返回。适合需要精细控制异步流程的场景，
;; 或在事件循环中定期检查请求完成状态。
;;
;; 与 http-wait-all 不同，http-poll 不会等待，只处理当前已完成的请求。

(let ((poll-count 0))
  (http-async-get "https://httpbin.org/get" (lambda (r) (set! poll-count (+ poll-count 1))))
  ;; 轮询直到完成
  (let loop ((pending #t))
    (when pending
      (let ((executed (http-poll)))
        (if (> executed 0)
            (display (string-append "Poll executed " (number->string executed) " callback(s)\n"))
            (begin
              (sleep 0.05)
              (loop #t)
            ) ;begin
        ) ;if
      ) ;let
    ) ;when
  ) ;let
  (check poll-count => 1)
) ;let

(check-report)

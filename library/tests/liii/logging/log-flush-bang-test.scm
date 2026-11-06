;; log-flush!
;; 立即刷新日志文件端口。
;;
;; 语法
;; ----
;; (log-flush!)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 仅在存在文件处理器时有效；未设置文件处理器时为空操作。
;; 文件处理器本身在每条日志写入后已自动 flush。

(import (liii check) (liii logging) (liii path) (liii string) (scheme file))

(check-set-mode! 'report-failed)

;; 未设置文件处理器时为空操作
(log-flush!)

(define *test-log*
  (path->string (path-join (path-temp-dir) "goldfish-test-log-flush.log"))
) ;define

(log-set-file-handler! *test-log*)
(log-set-level! DEBUG)
(log-info "flush check")
(log-flush!)

(let ((content (call-with-input-file *test-log* (lambda (in) (read-string 200 in)))))
  (check (string-contains? content "flush check") => #t)
) ;let

;; 恢复默认回调并清理
(log-set-callback! (make-stderr-handler))
(delete-file *test-log*)

(check-report)

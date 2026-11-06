;; log-set-file-handler!
;; 把日志回调切换为写入指定文件的处理器。
;;
;; 语法
;; ----
;; (log-set-file-handler! path)
;;
;; 参数
;; ----
;; path : string
;; 日志文件路径；父目录不存在时自动创建。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 文件以追加模式打开并保持打开，每条日志写入后立即 flush。
;; 2. 切换时会关闭此前打开的文件端口。

(import (liii check) (liii logging) (liii path) (liii string) (scheme file))

(check-set-mode! 'report-failed)

(define *test-log*
  (path->string (path-join (path-temp-dir) "goldfish-test-set-file-handler.log"))
) ;define

(log-set-file-handler! *test-log*)
(log-set-level! DEBUG)
(log-warning "auto flush from log-warning")

(let ((content (call-with-input-file *test-log* (lambda (in) (read-string 200 in)))))
  (check (string-contains? content "auto flush from log-warning") => #t)
) ;let

;; 恢复默认回调并清理
(log-set-callback! (make-stderr-handler))
(delete-file *test-log*)

(check-report)

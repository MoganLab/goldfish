;; make-file-handler
;; 创建把日志追加写入文件的处理器。
;;
;; 语法
;; ----
;; (make-file-handler path)
;;
;; 参数
;; ----
;; path : string
;; 日志文件路径；父目录不存在时自动创建。
;;
;; 返回值
;; ----
;; procedure
;; 接收一条日志 alist 的回调过程，按当前格式写入文件并立即 flush。
;;
;; 说明
;; ----
;; 1. 文件以追加模式打开并保持打开，直到下一次设置回调时被关闭。
;; 2. 进程退出时通过 exit-hook 自动 flush。

(import (liii check) (liii logging) (liii path) (liii string) (scheme file))

(check-set-mode! 'report-failed)

(define *test-log*
  (path->string (path-join (path-temp-dir) "goldfish-test-make-file-handler.log"))
) ;define

(define *handler* (make-file-handler *test-log*))

;; 写入一条日志（端口保持打开，未关闭，未手动调用 log-flush!）
(*handler* '((SEVERITY . 6) (MESSAGE . "file handler flush")))

;; 立即读取验证内容已刷新到文件
(let ((content (call-with-input-file *test-log* (lambda (in) (read-string 200 in)))))
  (check (string-contains? content "file handler flush") => #t)
  (check (string-contains? content "INFO") => #t)
) ;let

;; 关闭文件端口并清理
(log-set-callback! (make-stderr-handler))
(delete-file *test-log*)

(check-report)

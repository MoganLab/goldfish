(import (liii base)
        (liii logging)
        (scheme file))

;; demo: exit-hook 与 (liii logging) 的集成
;;
;; 运行此 demo 后检查 demo/exit-hook-demo.log 是否存在且包含日志内容。
;; exit-hook 会在进程退出时自动 flush 日志文件。

;; 设置文件日志处理器
(log-set-file-handler! "demo/exit-hook-demo.log")
(log-set-level! DEBUG)

;; 记录几条日志
(log-info "Application started")
(log-debug "Debug message before exit")
(log-warning "Something might be wrong")

;; 通过 exit-hook 注册额外的清理逻辑
(set! (hook-functions *exit-hook*)
  (cons (lambda (hook)
          (display "exit-hook triggered with code: ")
          (display (hook 'code))
          (newline))
        (hook-functions *exit-hook*)))

;; 正常退出，exit-hook 会自动触发日志 flush
(display "Exiting...")
(newline)
(exit 0)

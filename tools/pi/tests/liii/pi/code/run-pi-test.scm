(import (liii check) (liii string) (liii pi code cli))

(check-set-mode! 'report-failed)

;; run-pi
;; 剥离命令行中的 gf/pi 前缀，加载扩展并执行 Pi 命令。
;;
;; 语法
;; ----
;; (run-pi raw-args) -> integer
;;
;; 参数
;; ----
;; raw-args : list
;; 原始命令行字符串列表，可包含可执行文件路径及 pi 子命令前缀。
;;
;; 返回值
;; ----
;; integer
;; 进程风格退出码：成功为 0，参数、安装或运行错误为 1。
;; 函数会捕获命令执行错误并输出简短诊断。
;;
;; 示例
;; ----
;; (run-pi '("gf" "pi" "--help")) => 0
;; (run-pi '("/usr/bin/gf" "--help")) => 0

(define (check-help-command raw-args)
  (let ((status #f))
    (let ((output (with-output-to-string (lambda () (set! status (run-pi raw-args))))))
      (check status => 0)
      (check-true (string-contains? output "Usage:"))
    ) ;let
  ) ;let
) ;define

;; gf pi 与可执行文件路径两种前缀都应正确剥离
(check-help-command '("gf" "pi" "--help"))
(check-help-command '("/usr/bin/gf" "--help"))

(check-report)

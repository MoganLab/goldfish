;; make-process-error
;; 构造用于描述子进程执行失败的错误记录。
;;
;; 语法
;; ----
;; (make-process-error code stderr stdout command)
;;
;; 参数
;; ----
;; code : integer
;; 子进程退出码。
;;
;; stderr、stdout : string
;; 子进程写入标准错误和标准输出的内容。
;;
;; command : string 或 list
;; 产生错误的原始命令。
;;
;; 返回值
;; ----
;; process-error
;; 包含退出码、输出内容和原始命令的错误记录。

(import (liii check) (liii subprocess process-error))

(check-set-mode! 'report-failed)

(let ((err (make-process-error 1 "error\n" "output\n" "false")))
  (check (process-error? err) => #t)
  (check (process-error-code err) => 1)
  (check (process-error-stderr err) => "error\n")
  (check (process-error-stdout err) => "output\n")
  (check (process-error-command err) => "false")
) ;let

(let ((err (make-process-error 2 "" "" '("sh" "-c" "exit 2"))))
  (check (process-error-command err) => '("sh" "-c" "exit 2"))
) ;let

(check-report)

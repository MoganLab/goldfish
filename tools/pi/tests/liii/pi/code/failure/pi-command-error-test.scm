(import (liii check)
  (liii subprocess)
  (liii pi code failure)
) ;import

(check-set-mode! 'report-failed)

;; pi-command-failure-detail：子进程失败的统一诊断文本

;; 1. 优先取 stderr
(check (pi-command-failure-detail (make-process-error 2 "boom\n" "" "cmd")) => "boom")

;; 2. stderr 为空时取 stdout
(check (pi-command-failure-detail (make-process-error 2 "" "out\n" "cmd")) => "out")

;; 3. 两者都为空时给出退出码
(check (pi-command-failure-detail (make-process-error 2 "" "" "cmd"))
       => "Command failed with exit code 2")

;; pi-command-error：统一的「子进程失败 → 工具错误」

;; 1. 抛出的错误标签是 'error（与其它 pi-execute-* 一致）
(check-catch 'error
  (pi-command-error "grep" (make-process-error 2 "boom\n" "" "cmd"))
) ;check-catch

;; 2. 消息格式为 "<工具名> failed: <诊断文本>"
;;    catch 的 handler 收到错误类型与错误信息两个参数
(check (catch 'error
         (lambda () (pi-command-error "grep" (make-process-error 2 "boom\n" "" "cmd")))
         (lambda (type info)
           (cond ((string? info) info)
                 ((pair? info) (if (string? (car info)) (car info) (object->string info)))
                 (else (object->string info))
           ) ;cond
         ) ;lambda
       )
       => "grep failed: boom")

(check-report)

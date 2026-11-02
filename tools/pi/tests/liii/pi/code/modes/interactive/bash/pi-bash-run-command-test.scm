;; pi-bash-run-command
;; pi-bash-run-command 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (pi-bash-run-command ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base)
  (liii check)
  (liii pi code modes interactive bash))

(check-set-mode! 'report)

;; ! 执行器对齐上游 session.executeBash：返回文本已清理
;; （剥 ANSI/OSC、删控制字符、删 \r，保留 \t\n）
(let-values (((text code) (pi-bash-run-command "printf '\\033[31mred\\033[0m\\r\\n'")))
  (check code => 0)
  (check text => "red\n"))

(check-report)

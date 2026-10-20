(import (liii check) (liii pi) (liii string))

(check-set-mode! 'report-failed)

;; pi-execute-bash
;; 执行 bash 命令并返回标准输出与是否出错。
(let-values (((out is-err) (pi-execute-bash '(("command" . "echo 'bash ok'")))))
  (check is-err => #f)
  (check-true (string-contains? out "bash ok")))

;; 执行返回非零退出码的命令
(let-values (((out is-err) (pi-execute-bash '(("command" . "exit 42")))))
  (check is-err => #t)
  (check-true (string-contains? out "Command exited with code 42")))

;; 模拟超时
(let-values (((out is-err) (pi-execute-bash '(("command" . "echo 1")) :timed-out? #t :timeout 5)))
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 5 seconds")))

(check-report)

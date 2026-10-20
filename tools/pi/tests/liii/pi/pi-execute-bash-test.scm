(import (liii check) (liii pi) (liii string))

(check-set-mode! 'report-failed)

;; pi-execute-bash
;; 执行 bash 命令并返回标准输出与是否出错。
(let-values (((out is-err) (pi-execute-bash '(("command" . "echo 'bash ok'")))))
  (check is-err => #f)
  (check-true (string-contains? out "bash ok"))
) ;let-values

;; 执行返回非零退出码的命令
(let-values (((out is-err) (pi-execute-bash '(("command" . "exit 42")))))
  (check is-err => #t)
  (check-true (string-contains? out "Command exited with code 42"))
) ;let-values

;; 模拟超时
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo 1")) :timed-out? #t :timeout 5)
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 5 seconds"))
) ;let-values

;; 传入 timeout=0 不被秒杀，正常执行
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo 'bash timeout 0'"))
                (list (cons :timeout 0))
              ) ;pi-execute-bash
             ) ;
            ) ;
  (check is-err => #f)
  (check-true (string-contains? out "bash timeout 0"))
) ;let-values

;; timeout 为 0 或负数时不被秒杀，正常执行
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo 'timeout zero'")
                                 ("timeout" . 0)))
             ) ;
            ) ;
  (check is-err => #f)
  (check (string-trim-both out) => "timeout zero")
) ;let-values

(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo 'timeout neg'")
                                 ("timeout" . -1)))
             ) ;
            ) ;
  (check is-err => #f)
  (check (string-trim-both out) => "timeout neg")
) ;let-values

;; 模拟超时配合 opts :timeout 0，安全回退到 120 秒
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo test")) :timed-out? #t :timeout 0)
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 120 seconds"))
) ;let-values

;; 模型超大 timeout 不能突破顶层用户限制
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "sleep 5") ("timeout" . 1000)) :timeout 1)
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 1 seconds"))
) ;let-values

;; 模型更小 timeout 缩短超时生效
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "sleep 5") ("timeout" . 1)) :timeout 5)
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 1 seconds"))
) ;let-values

;; opts 以 alist 格式传入 :timeout 0 正常完成
(let-values (((out is-err)
              (pi-execute-bash (list (cons "command" "echo success"))
                (list (cons :timeout 0))
              ) ;pi-execute-bash
             ) ;
            ) ;
  (check is-err => #f)
  (check (string-trim-both out) => "success")
) ;let-values

(check-report)

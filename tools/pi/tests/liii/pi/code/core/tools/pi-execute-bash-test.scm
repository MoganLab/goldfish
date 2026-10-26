(import (liii check) (liii pi agent tools) (liii pi code core tools) (liii pi agent tools) (liii string))

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

;; 模拟超时，指定 opts :timeout
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo 1")) :timed-out? #t :timeout 5)
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 5 seconds"))
) ;let-values

;; 模拟超时，未指定 opts :timeout 默认 600 秒
(let-values (((out is-err) (pi-execute-bash '(("command" . "echo test")) :timed-out? #t)))
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 600 seconds"))
) ;let-values

;; 命令执行时间超过 timeout 参数
(let-values (((out is-err) (pi-execute-bash '(("command" . "sleep 5")
                                              ("timeout" . 1)))))
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 1 seconds"))
) ;let-values

;; timeout 为 0、负数或非数值时抛出异常（对齐原生 pi）
(check-catch 'error (pi-execute-bash '(("command" . "echo 'timeout zero'")
                                       ("timeout" . 0))))
(check-catch 'error (pi-execute-bash '(("command" . "echo 'timeout neg'")
                                       ("timeout" . -1))))
(check-catch 'error (pi-execute-bash '(("command" . "echo 'timeout str'")
                                       ("timeout" . "invalid"))))

;; timeout 超过 sanity cap (2147483.647s) 抛出异常
(check-catch 'error (pi-execute-bash '(("command" . "echo 'timeout huge'")
                                       ("timeout" . 2147484))))

;; 通过 execute-tool-call 调用时非法 timeout 结构化捕获为 isError
(let ((res (execute-tool-call "bash" '(("command" . "echo test") ("timeout" . 0)))))
  (check (pi-tool-result-is-error res) => #t)
  (check-true (string-contains? (pi-extract-tool-result-text res) "Invalid timeout"))
) ;let
(let ((res (execute-tool-call "bash" '(("command" . "echo test") ("timeout" . 2147484)))))
  (check (pi-tool-result-is-error res) => #t)
  (check-true (string-contains? (pi-extract-tool-result-text res) "Invalid timeout"))
) ;let

;; 模拟超时配合 opts :timeout 0，安全回退到 600 秒
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo test")) :timed-out? #t :timeout 0)
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 600 seconds"))
) ;let-values

;; 模型显式 timeout 超越默认值生效（解除 min clamp，对齐原生 pi）
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "sleep 5") ("timeout" . 1000))
                :timed-out? #t
                :timeout 1
              ) ;pi-execute-bash
             ) ;
            ) ;
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 1000 seconds"))
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

;; 缺少 command 参数抛出异常
(check-catch 'error (pi-execute-bash '()))

(check-report)

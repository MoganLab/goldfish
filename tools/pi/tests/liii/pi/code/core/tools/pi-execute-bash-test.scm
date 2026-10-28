(import (liii check) (liii pi agent tools) (liii pi code core tools) (liii pi agent tools) (liii string) (liii go) (liii time) (scheme time))

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

;; on-update 收到累计输出快照；没有换行的数据块也应在进程结束前可见
(let ((updates '()))
  (let-values (((out is-err)
                (pi-execute-bash
                  '(("command" . "printf first; sleep 0.2; printf second"))
                  :on-update (lambda (text) (set! updates (cons text updates))))))
    (check is-err => #f)
    (check out => "firstsecond")
    (set! updates (reverse updates))
    (check-true (pair? (member "first" updates)))
    (check (car (reverse updates)) => "firstsecond")))

;; stderr 与 stdout 按 shell 写入顺序合并，并进入同一条流式回调
(let ((updates '()))
  (let-values (((out is-err)
                (pi-execute-bash
                  '(("command" . "printf out; printf err >&2"))
                  :on-update (lambda (text) (set! updates (cons text updates))))))
    (check is-err => #f)
    (check out => "outerr")
    (check-true (pair? updates))
    (check (car updates) => "outerr")))

;; 流式路径保留非零退出与超时语义
(let-values (((out is-err)
              (pi-execute-bash '(("command" . "printf boom; exit 42"))
                :on-update (lambda (text) #t))))
  (check is-err => #t)
  (check-true (string-contains? out "boom"))
  (check-true (string-contains? out "Command exited with code 42")))

(let-values (((out is-err)
              (pi-execute-bash '(("command" . "sleep 5") ("timeout" . 1))
                :on-update (lambda (text) #t))))
  (check is-err => #t)
  (check-true (string-contains? out "Command timed out after 1 seconds")))

;; ---------------------------------------------------------------------------
;; 输出清理：对齐上游 bash-executor appendText
;; sanitizeBinaryOutput(stripAnsi(rawText)).replace(/\r/g, "")
;; 即：剥 ANSI（CSI+OSC）、删控制字符、删 \r，保留 \t\n
;; ---------------------------------------------------------------------------

;; SGR 颜色序列被剥离
(let-values (((out is-err) (pi-execute-bash '(("command" . "printf '\\033[31mred\\033[0m'")))))
  (check is-err => #f)
  (check out => "red"))

;; \r 被删除（进度条、CRLF 行尾）
(let-values (((out is-err) (pi-execute-bash '(("command" . "printf 'a\\rb\\r\\n'")))))
  (check is-err => #f)
  (check out => "ab\n"))

;; TAB 保留（属于内容，由渲染层展开）
(let-values (((out is-err) (pi-execute-bash '(("command" . "printf 'a\\tb'")))))
  (check is-err => #f)
  (check out => "a\tb"))

;; 控制字符被删除（除 \t\n\r）
(let-values (((out is-err) (pi-execute-bash '(("command" . "printf 'a\\001b\\002c'")))))
  (check is-err => #f)
  (check out => "abc"))

;; OSC 序列被剥离（终端标题，BEL 终结）
(let-values (((out is-err) (pi-execute-bash '(("command" . "printf '\\033]0;title\\007hi'")))))
  (check is-err => #f)
  (check out => "hi"))

;; 流式快照同样是干净的
(let ((updates '()))
  (let-values (((out is-err)
                (pi-execute-bash
                  '(("command" . "printf '\\033[32mok\\033[0m'"))
                  :on-update (lambda (text) (set! updates (cons text updates))))))
    (check is-err => #f)
    (check out => "ok")
    (check-true (pair? updates))
    (check (car updates) => "ok")))

;; 快速大量输出时回调次数有界（100ms 节流生效，避免每个字节块都全量清理能CPU飙升）
(let ((updates '()))
  (let-values (((out is-err)
                (pi-execute-bash
                  '(("command" . "seq 1 2000"))
                  :on-update (lambda (text) (set! updates (cons text updates))))))
    (check is-err => #f)
    (check-true (string-contains? out "2000"))
    ;; 2000 行快速输出应在毫秒级完成，回调次数远小于行数
    (check-true (<= (length updates) 10))))

;; 输出修剪与非零退出输出
(let-values (((out is-err) (pi-execute-bash '(("command" . "echo 'hello bash'")))))
  (check is-err => #f)
  (check (string-trim-both out) => "hello bash"))

(let-values (((out is-err)
              (pi-execute-bash '(("command" . "echo 'fail msg'; exit 42")))))
  (check is-err => #t)
  (check-true (string-contains? out "fail msg"))
  (check-true (string-contains? out "Command exited with code 42")))

;; :check-abort 中止：延迟信号保证 echo 先完成，中止后已产生的
;; 部分输出仍保留在结果里，并追加 "Command aborted" 错误状态
;; （对齐上游 signal.aborted → "Command aborted"）
(let* ((sig (make-chan 1))
       (t (go-call (lambda (ch) (sleep 0.5) (chan-send! ch 'abort 0)) sig))
      ) ;
  (let-values (((out is-err)
                (pi-execute-bash '(("command" . "echo 'partial'; sleep 30"))
                  :check-abort (lambda () (chan-try-recv! sig #f))
                ) ;pi-execute-bash
               ) ;
              ) ;
    (check is-err => #t)
    (check-true (string-contains? out "partial"))
    (check-true (string-contains? out "Command aborted"))
  ) ;let-values
) ;let*

;; :check-abort 后台延迟中止：短延迟后中止 sleep 长命令，
;; 总耗时不等自然退出（验证真的杀掉了进程树）
(let* ((sig (make-chan 1))
       (t (go-call (lambda (ch) (sleep 0.3) (chan-send! ch 'abort 0)) sig))
       (start (current-jiffy))
      ) ;
  (let-values (((out is-err)
                (pi-execute-bash '(("command" . "sleep 30"))
                  :check-abort (lambda () (chan-try-recv! sig #f))
                ) ;pi-execute-bash
               ) ;
              ) ;
    (check is-err => #t)
    (check-true (string-contains? out "Command aborted"))
    (check-true
      (< (/ (- (current-jiffy) start) (jiffies-per-second)) 10))
  ) ;let-values
) ;let*

;; 无 :check-abort 时行为不变
(let-values (((out is-err) (pi-execute-bash '(("command" . "echo 'no-abort'")))))
  (check is-err => #f)
  (check-true (string-contains? out "no-abort")))

;; :check-abort 非过程时抛 type-error
(check-catch 'type-error
  (pi-execute-bash '(("command" . "echo x")) :check-abort "not-a-proc"))

(check-report)

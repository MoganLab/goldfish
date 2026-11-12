;; pi-bash-run-command
;; pi-bash-run-command 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (pi-bash-run-command command [:timeout seconds] [:check-abort proc])
;;
;; 参数
;; ----
;; command : string
;; 要执行的 bash 命令。
;; timeout : real，可选
;; 超时秒数，超时强杀返回退出码 -1。
;; check-abort : procedure，可选
;; ESC 中止探测谓词（对齐上游 AbortController/signal）：为真时走流式路径，
;; 轮询点探测命中即杀进程树并返回退出码 -2。
;;
;; 返回值
;; ----
;; (values 清理后的合并输出 退出码)
;; 退出码 -1 表示超时，-2 表示被中止。

(import (scheme base) (liii check) (liii go) (liii time) (scheme time)
  (liii string) (liii pi code modes interactive bash))

(check-set-mode! 'report)

;; ! 执行器对齐上游 session.executeBash：返回文本已清理
;; （剥 ANSI/OSC、删控制字符、删 \r，保留 \t\n）
(let-values (((text code) (pi-bash-run-command "printf '\\033[31mred\\033[0m\\r\\n'")))
  (check code => 0)
  (check text => "red\n")
) ;let-values

;; :check-abort 非 procedure 时报 type-error
(check-catch 'type-error
  (pi-bash-run-command "echo x" :check-abort "not-a-proc"))

;; :check-abort 恒为假时行为不变：正常返回真实退出码
(let-values (((text code)
              (pi-bash-run-command "echo 'no-abort'"
                :check-abort (lambda () #f)
              ) ;pi-bash-run-command
            ) ;
           ) ;
  (check code => 0)
  (check-true (string-contains? text "no-abort"))
) ;let-values

;; :check-abort 探测到中止：echo 先完成再 sleep 30，0.5s 后信号到达，
;; 轮询点探测即杀进程树返回退出码 -2，已产生输出保留且总耗时不等自然退出
(let* ((sig (make-chan 1))
       (t (go-call (lambda (ch) (sleep 0.5) (chan-send! ch 'abort 0)) sig))
       (t0 (current-second))
      ) ;
  (let-values (((text code)
                (pi-bash-run-command "echo 'partial'; sleep 30"
                  :check-abort (lambda () (chan-try-recv! sig #f))
                ) ;pi-bash-run-command
              ) ;
             ) ;
    (check code => -2)
    (check-true (string-contains? text "partial"))
    (check-true (< (- (current-second) t0) 10))
  ) ;let-values
) ;let*

(check-report)

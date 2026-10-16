(import (liii check) (liii signal))

(check-set-mode! 'report-failed)

;; signal-raise
;; 向当前进程主动发送指定系统信号。
;;
;; 语法
;; ----
;; (signal-raise sig)
;;
;; 参数
;; ----
;; sig : symbol?
;; 信号符号，例如 'sigusr1, 'sigusr2, 'sigint 等。
;;
;; 返回值
;; -----
;; unspecified
;;
;; 错误
;; ----
;; type-error
;; 当 sig 不是符号时抛出。
;;
;; value-error
;; 当 sig 是不受支持的信号名称时抛出。
;;
;; 说明
;; ----
;; 向当前进程发送指定系统信号。若已注册自定义处理过程，将在安全点被调度执行。

;; 1. 基本功能测试：自身触发信号并被自定义过程捕获
(let ((handled #f))
  (signal-trap 'sigusr1 (lambda (sig) (set! handled sig)))
  (signal-raise 'sigusr1)
  (check handled => 'sigusr1)
  (signal-trap 'sigusr1 'default)
) ;let

;; 2. 忽略信号的主动发送测试（不应产生中断或异常）
(let ((prev (signal-trap 'sigpipe 'ignore)))
  (signal-raise 'sigpipe)
  (check (signal-get 'sigpipe) => 'ignore)
  (signal-trap 'sigpipe prev)
) ;let

;; 3. 错误测试：类型错误与非法信号名
(check-catch 'type-error (signal-raise 123))
(check-catch 'type-error (signal-raise "sigusr1"))
(check-catch 'type-error (signal-raise #t))
(check-catch 'type-error (signal-raise '(sigusr1)))

(check-catch 'value-error (signal-raise 'unknown-sig))
(check-catch 'value-error (signal-raise 'nosuchsig))

(check-report)

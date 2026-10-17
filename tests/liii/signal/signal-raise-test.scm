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
;; 信号符号，例如 'SIGINT, 'SIGTERM, 'SIGUSR1 等。
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
;; 向当前进程发送指定信号。若已注册自定义处理过程，将在安全点被调度执行；
;; 若自定义处理过程自身抛出错误，错误将被重抛给调用者。

;; 1. 基本功能测试：自身触发信号并被自定义过程捕获
(let ((handled #f))
  (signal-trap 'SIGINT (lambda (sig) (set! handled sig)))
  (signal-raise 'SIGINT)
  (check handled => 'SIGINT)
  (signal-trap 'SIGINT 'default)
) ;let

;; 2. 忽略信号的主动发送测试（不应产生中断或异常）
(let ((prev (signal-trap 'SIGTERM 'ignore)))
  (signal-raise 'SIGTERM)
  (check (signal-get 'SIGTERM) => 'ignore)
  (signal-trap 'SIGTERM prev)
) ;let

;; 3. handler 抛出错误时，错误类型保持不变并向上传播，且派发机制不被破坏
(let ((handled #f))
  (signal-trap 'SIGTERM (lambda (sig) (error 'handler-failed "handler error")))
  (check-catch 'handler-failed (signal-raise 'SIGTERM))
  (signal-trap 'SIGTERM (lambda (sig) (set! handled sig)))
  (signal-raise 'SIGTERM)
  (check handled => 'SIGTERM)
  (signal-trap 'SIGTERM 'default)
) ;let

;; 4. 错误测试：类型错误与非法信号名
(check-catch 'type-error (signal-raise 123))
(check-catch 'type-error (signal-raise "SIGINT"))
(check-catch 'type-error (signal-raise #t))
(check-catch 'type-error (signal-raise '(SIGINT)))

(check-catch 'value-error (signal-raise 'unknown-sig))
(check-catch 'value-error (signal-raise 'nosuchsig))

(check-report)

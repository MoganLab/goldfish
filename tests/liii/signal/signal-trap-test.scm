(import (liii check) (liii signal))

(check-set-mode! 'report-failed)

;; signal-trap
;; 为指定系统信号注册处理动作。
;;
;; 语法
;; ----
;; (signal-trap sig handler)
;;
;; 参数
;; ----
;; sig : symbol?
;; 信号符号，例如 'sigint, 'sigterm, 'sigpipe, 'sigusr1 等。
;;
;; handler : procedure? | 'default | 'ignore
;; 信号处理动作：
;; - 单参数过程 (lambda (sig) ...)：自定义处理函数，进入安全点时调用
;; - 'default：恢复系统默认动作
;; - 'ignore：忽略该信号
;;
;; 返回值
;; -----
;; procedure? | 'default | 'ignore
;; 此前绑定的 handler。
;;
;; 错误
;; ----
;; type-error
;; 当 sig 不是符号，或者 handler 既不是过程也不是 'default 或 'ignore 时抛出。
;;
;; value-error
;; 当 sig 是不受支持的信号名称时抛出。
;;
;; 说明
;; ----
;; 注册的信号处理过程保证异步信号安全（Async-Signal-Safe），
;; 不在原生中断上下文中直接求值，而是在安全点受控调度执行。

;; 1. 基本功能测试：注册自定义过程、忽略与恢复默认
(let* ((orig (signal-get 'sigint))
       (dummy (lambda (s) s))
       (old1 (signal-trap 'sigint dummy))
       (curr1 (signal-get 'sigint))
       (old2 (signal-trap 'sigint 'ignore))
       (curr2 (signal-get 'sigint))
       (old3 (signal-trap 'sigint 'default))
       (curr3 (signal-get 'sigint))
      ) ;
  (check old1 => orig)
  (check (procedure? curr1) => #t)
  (check (eq? old2 dummy) => #t)
  (check curr2 => 'ignore)
  (check old3 => 'ignore)
  (check curr3 => 'default)
) ;let*

;; 2. 错误测试：类型错误与非法信号名
(check-catch 'type-error (signal-trap 123 'default))
(check-catch 'type-error (signal-trap "sigint" 'default))
(check-catch 'type-error (signal-trap '(sigint) 'default))
(check-catch 'type-error (signal-trap 'sigint 456))
(check-catch 'type-error (signal-trap 'sigint "ignore"))
(check-catch 'type-error (signal-trap 'sigint '(default)))
(check-catch 'type-error (signal-trap 'sigint #t))

(check-catch 'value-error (signal-trap 'unknown-sig 'default))
(check-catch 'value-error (signal-trap 'siginvalid 'ignore))
(check-catch 'value-error (signal-trap 'sig12345 (lambda (s) s)))

(check-report)

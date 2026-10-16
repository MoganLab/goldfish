(import (liii check) (liii signal))

(check-set-mode! 'report-failed)

;; signal-get
;; 获取指定系统信号当前绑定的处理动作。
;;
;; 语法
;; ----
;; (signal-get sig)
;;
;; 参数
;; ----
;; sig : symbol?
;; 信号符号，例如 'sigint, 'sigterm, 'sigpipe 等。
;;
;; 返回值
;; -----
;; procedure? | 'default | 'ignore
;; 当前绑定的处理动作。
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
;; 查询当前绑定的信号处理程序。若未显式注册，默认为 'default。

;; 1. 基本功能测试：默认动作与状态同步
(check (signal-get 'sigint) => 'default)
(check (signal-get 'sigterm) => 'default)

(let ((prev (signal-trap 'sigpipe 'ignore)))
  (check (signal-get 'sigpipe) => 'ignore)
  (signal-trap 'sigpipe prev)
  (check (signal-get 'sigpipe) => prev)
) ;let

;; 2. 错误测试：类型错误与非法信号名
(check-catch 'type-error (signal-get 123))
(check-catch 'type-error (signal-get "sigint"))
(check-catch 'type-error (signal-get #f))
(check-catch 'type-error (signal-get '(sigterm)))

(check-catch 'value-error (signal-get 'unknown-sig))
(check-catch 'value-error (signal-get 'nosuchsignal))

(check-report)

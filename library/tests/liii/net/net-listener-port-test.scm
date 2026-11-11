;; net-listener-port
;; 获取网络监听器绑定的本地端口号。
;;
;; 语法
;; ----
;; (net-listener-port listener)
;;
;; 参数
;; ----
;; listener : net-listener?
;; 监听器对象。
;;
;; 返回值
;; -----
;; integer?
;; 实际绑定的端口号（大于 0）。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是 net-listener 时抛出。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：获取随机绑定的端口
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let ((port (net-listener-port ln)))
    (check-true (integer? port))
    (check-true (> port 0))
    (check-true (< port 65536)))
  (net-listener-close ln))

;; 测试项2：类型错误检查
(check-catch 'type-error (net-listener-port 12345))
(check-catch 'type-error (net-listener-port "127.0.0.1:0"))

(check-report)

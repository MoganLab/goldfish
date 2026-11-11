;; net-listener-addr
;; 获取网络监听器的地址字符串表示。
;;
;; 语法
;; ----
;; (net-listener-addr listener)
;;
;; 参数
;; ----
;; listener : net-listener?
;; 监听器对象。
;;
;; 返回值
;; -----
;; string?
;; 形如 "127.0.0.1:54321" 的地址字符串。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是 net-listener 时抛出。

(import (scheme base)
        (liii check)
        (liii string)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：获取监听器地址字符串
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let ((addr (net-listener-addr ln)))
    (check-true (string? addr))
    (check-true (string-contains? addr "127.0.0.1:")))
  (net-listener-close ln))

;; 测试项2：类型错误检查
(check-catch 'type-error (net-listener-addr "127.0.0.1:0"))
(check-catch 'type-error (net-listener-addr 123))

(check-report)

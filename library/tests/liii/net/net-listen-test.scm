;; net-listen
;; 创建并绑定 TCP 网络监听器。
;;
;; 语法
;; ----
;; (net-listen network address)
;;
;; 参数
;; ----
;; network : symbol? | string?
;; 网络协议类型，支持 'tcp, "tcp" 或 "tcp4"。
;; address : string?
;; 监听地址，形如 "127.0.0.1:0"（绑定本地随机可用端口）或 "127.0.0.1:8080"。
;;
;; 返回值
;; -----
;; net-listener?
;; 返回已绑定就绪的监听器对象。
;;
;; 错误处理
;; ----
;; type-error
;; 当 network 不是 symbol 或 string，或者 address 不是 string 时抛出。
;; value-error
;; 当 network 不是支持的协议，或者 address 格式不合法时抛出。
;; net-error
;; 当底层套接字创建、端口绑定或监听失败时抛出。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：成功创建随机端口监听器
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (check-true (net-listener? ln))
  (check-true (> (net-listener-port ln) 0))
  (net-listener-close ln))

;; 测试项2：使用字符串作为 network 参数
(let ((ln (net-listen "tcp" "127.0.0.1:0")))
  (check-true (net-listener? ln))
  (net-listener-close ln))

;; 测试项3：参数校验错误
(check-catch 'type-error (net-listen 123 "127.0.0.1:0"))
(check-catch 'type-error (net-listen 'tcp 8080))
(check-catch 'value-error (net-listen 'udp "127.0.0.1:0"))
(check-catch 'value-error (net-listen 'tcp "invalid-addr"))

(check-report)

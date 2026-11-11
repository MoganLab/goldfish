;; net-dial
;; 建立到远端 TCP 地址的网络连接。
;;
;; 语法
;; ----
;; (net-dial network address)
;;
;; 参数
;; ----
;; network : symbol? | string?
;; 网络协议类型，支持 'tcp, "tcp" 或 "tcp4"。
;; address : string?
;; 远端主机与端口，形如 "127.0.0.1:8080"。
;;
;; 返回值
;; -----
;; net-conn?
;; 返回已连接的客户端网络对象。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数类型不正确时抛出。
;; value-error
;; 当 network 或 address 格式不合法时抛出。
;; net-error
;; 当连接远端服务器失败（如拒绝连接）时抛出。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：成功连接本地监听器
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let ((c (net-dial 'tcp (net-listener-addr ln))))
    (check-true (net-conn? c))
    (net-conn-close c))
  (net-listener-close ln))

;; 测试项2：使用字符串作为 network 参数
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let ((c (net-dial "tcp" (net-listener-addr ln))))
    (check-true (net-conn? c))
    (net-conn-close c))
  (net-listener-close ln))

;; 测试项3：参数校验
(check-catch 'type-error (net-dial 123 "127.0.0.1:80"))
(check-catch 'type-error (net-dial 'tcp 80))
(check-catch 'value-error (net-dial 'udp "127.0.0.1:80"))
(check-catch 'value-error (net-dial 'tcp "invalid-host"))

(check-report)

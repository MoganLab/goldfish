;; net-accept
;; 接收一个入站网络客户端连接。
;;
;; 语法
;; ----
;; (net-accept listener)
;;
;; 参数
;; ----
;; listener : net-listener?
;; 监听器对象。
;;
;; 返回值
;; -----
;; net-conn?
;; 返回接收到的已连接套接字对象。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是 net-listener 时抛出。
;; net-error
;; 当监听器已被关闭或底层系统 accept 失败时抛出。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：监听并成功接收客户端连接
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let* ((addr (net-listener-addr ln))
         (client-conn (net-dial 'tcp addr))
         (server-conn (net-accept ln)))
    (check-true (net-conn? client-conn))
    (check-true (net-conn? server-conn))
    (net-conn-close client-conn)
    (net-conn-close server-conn))
  (net-listener-close ln))

;; 测试项2：在已关闭的监听器上 accept 抛出 net-error
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (net-listener-close ln)
  (check-catch 'net-error (net-accept ln)))

;; 测试项3：类型错误检查
(check-catch 'type-error (net-accept "invalid-listener"))

(check-report)

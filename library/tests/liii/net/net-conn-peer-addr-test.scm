;; net-conn-peer-addr
;; 获取网络连接对端的地址字符串。
;;
;; 语法
;; ----
;; (net-conn-peer-addr conn)
;;
;; 参数
;; ----
;; conn : net-conn?
;; 网络连接对象。
;;
;; 返回值
;; -----
;; string?
;; 对端 IP 和端口，形如 "127.0.0.1:53210"。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是 net-conn 时抛出。

(import (scheme base)
        (liii check)
        (liii string)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：读取客户端与服务端对端地址
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let* ((client (net-dial 'tcp (net-listener-addr ln)))
         (server (net-accept ln)))
    (let ((c-peer (net-conn-peer-addr client))
          (s-peer (net-conn-peer-addr server)))
      (check-true (string? c-peer))
      (check-true (string? s-peer))
      (check-true (string-contains? c-peer "127.0.0.1:"))
      (check-true (string-contains? s-peer "127.0.0.1:")))
    (net-conn-close client)
    (net-conn-close server))
  (net-listener-close ln))

;; 测试项2：类型错误检查
(check-catch 'type-error (net-conn-peer-addr "127.0.0.1:0"))

(check-report)

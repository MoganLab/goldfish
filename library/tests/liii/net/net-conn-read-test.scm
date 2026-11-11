;; net-conn-read
;; 从网络连接读取数据（函数式分配新 bytevector）。
;;
;; 语法
;; ----
;; (net-conn-read conn [max-bytes])
;;
;; 参数
;; ----
;; conn : net-conn?
;; 网络连接对象。
;; max-bytes : integer? (可选，默认 4096)
;; 本次读取的最大字节数。
;;
;; 返回值
;; -----
;; bytevector? | (eof-object)
;; 返回接收到的数据；对端关闭连接发送 EOF 时返回 (eof-object)。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数类型不正确时抛出。
;; net-error
;; 当连接已关闭或底层系统读取失败时抛出。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：读取对端写入的数据
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let* ((client (net-dial 'tcp (net-listener-addr ln)))
         (server (net-accept ln)))
    (net-conn-write client "ping")
    (let ((data (net-conn-read server 4096)))
      (check-true (bytevector? data))
      (check (bytevector-length data) => 4))
    (net-conn-close client)
    (net-conn-close server))
  (net-listener-close ln))

;; 测试项2：对端关闭后读取返回 EOF
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let* ((client (net-dial 'tcp (net-listener-addr ln)))
         (server (net-accept ln)))
    (net-conn-close client)
    (let ((data (net-conn-read server 4096)))
      (check-true (eof-object? data))))
  (net-listener-close ln))

;; 测试项3：参数校验
(check-catch 'type-error (net-conn-read "conn" 100))
(check-catch 'type-error (net-conn-read 42))

(check-report)

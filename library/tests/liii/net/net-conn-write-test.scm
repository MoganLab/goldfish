;; net-conn-write
;; 向网络连接写入数据。
;;
;; 语法
;; ----
;; (net-conn-write conn data)
;;
;; 参数
;; ----
;; conn : net-conn?
;; 网络连接对象。
;; data : string? | bytevector?
;; 待写入的数据，字符串按 UTF-8 字节写出。
;;
;; 返回值
;; -----
;; integer?
;; 成功写入的字节数。
;;
;; 错误处理
;; ----
;; type-error
;; 当参数类型不正确时抛出。
;; net-error
;; 当连接已关闭或底层系统写入失败时抛出。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：写入字符串数据并回读确认
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let* ((client (net-dial 'tcp (net-listener-addr ln)))
         (server (net-accept ln)))
    (let ((n (net-conn-write client "hello net")))
      (check-true (integer? n))
      (check n => 9))
    (let ((data (net-conn-read server 4096)))
      (check-true (bytevector? data))
      (check (bytevector-length data) => 9))
    (net-conn-close client)
    (net-conn-close server))
  (net-listener-close ln))

;; 测试项2：写入 bytevector 数据
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let* ((client (net-dial 'tcp (net-listener-addr ln)))
         (server (net-accept ln))
         (bv (string->utf8 "Hello")))
    (let ((n (net-conn-write client bv)))
      (check n => 5))
    (net-conn-close client)
    (net-conn-close server))
  (net-listener-close ln))

;; 测试项3：参数校验
(check-catch 'type-error (net-conn-write "conn" "data"))
(check-catch 'type-error (net-conn-write 123 456))

(check-report)

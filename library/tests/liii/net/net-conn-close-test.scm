;; net-conn-close
;; 显式关闭网络连接。
;;
;; 语法
;; ----
;; (net-conn-close conn)
;;
;; 参数
;; ----
;; conn : net-conn?
;; 网络连接对象。
;;
;; 返回值
;; -----
;; unspecified
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是 net-conn 时抛出。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：正常关闭与幂等关闭
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let* ((client (net-dial 'tcp (net-listener-addr ln)))
         (server (net-accept ln)))
    (net-conn-close client)
    ;; 幂等关闭不报错
    (net-conn-close client)
    ;; 关闭后对端读取返回 EOF
    (let ((res (net-conn-read server 1024)))
      (check-true (eof-object? res)))
    (net-conn-close server))
  (net-listener-close ln))

;; 测试项2：类型错误检查
(check-catch 'type-error (net-conn-close 123))

(check-report)

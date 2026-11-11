;; net-conn?
;; 判断给定对象是否为网络连接。
;;
;; 语法
;; ----
;; (net-conn? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待测试的 Scheme 对象。
;;
;; 返回值
;; -----
;; boolean?
;; 若是 net-conn 则返回 #t，否则返回 #f。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：对连接对象返回 #t
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (let* ((client (net-dial 'tcp (net-listener-addr ln)))
         (server (net-accept ln)))
    (check-true (net-conn? client))
    (check-true (net-conn? server))
    (net-conn-close client)
    (net-conn-close server))
  (net-listener-close ln))

;; 测试项2：对非连接对象返回 #f
(check-false (net-conn? 123))
(check-false (net-conn? "conn"))
(check-false (net-conn? #f))

(check-report)

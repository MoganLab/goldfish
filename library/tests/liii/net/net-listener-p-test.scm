;; net-listener?
;; 判断给定对象是否为网络监听器。
;;
;; 语法
;; ----
;; (net-listener? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待测试的 Scheme 对象。
;;
;; 返回值
;; -----
;; boolean?
;; 若是 net-listener 则返回 #t，否则返回 #f。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：对监听器返回 #t
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (check-true (net-listener? ln))
  (net-listener-close ln)
  (check-true (net-listener? ln)))

;; 测试项2：对非监听器对象返回 #f
(check-false (net-listener? 123))
(check-false (net-listener? "not-a-listener"))
(check-false (net-listener? 'tcp))
(check-false (net-listener? #f))

(check-report)

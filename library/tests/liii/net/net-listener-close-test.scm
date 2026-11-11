;; net-listener-close
;; 显式关闭网络监听器并释放描述符。
;;
;; 语法
;; ----
;; (net-listener-close listener)
;;
;; 参数
;; ----
;; listener : net-listener?
;; 监听器对象。
;;
;; 返回值
;; -----
;; unspecified
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是 net-listener 时抛出。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：正常关闭与幂等关闭
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (net-listener-close ln)
  ;; 重复关闭不崩溃（幂等）
  (net-listener-close ln)
  (check-true #t))

;; 测试项2：关闭后 accept 报错
(let ((ln (net-listen 'tcp "127.0.0.1:0")))
  (net-listener-close ln)
  (check-catch 'net-error (net-accept ln)))

;; 测试项3：类型错误检查
(check-catch 'type-error (net-listener-close 123))

(check-report)

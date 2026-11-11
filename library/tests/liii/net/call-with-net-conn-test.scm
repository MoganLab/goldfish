;; call-with-net-conn
;; Guile 风格连接作用域组合子：离开作用域时自动关闭连接。
;;
;; 语法
;; ----
;; (call-with-net-conn conn proc)
;;
;; 参数
;; ----
;; conn : net-conn?
;; 网络连接对象。
;; proc : procedure?
;; 接收 conn 的单参过程。
;;
;; 返回值
;; -----
;; any
;; 返回 proc 调用的结果。
;;
;; 错误处理
;; ----
;; type-error
;; 当 proc 不是过程时抛出。

(import (scheme base)
        (liii check)
        (liii net))

(check-set-mode! 'report-failed)

;; 测试项1：正常离开作用域后 conn 自动关闭
(call-with-net-listener 'tcp "127.0.0.1:0"
  (lambda (ln)
    (let ((client (net-dial 'tcp (net-listener-addr ln))))
      (call-with-net-conn client
        (lambda (conn)
          (net-conn-write conn "scope-test")
          (check-true (net-conn? conn))))
      ;; 离开作用域后写入抛出 net-error 说明已关闭
      (check-catch 'net-error (net-conn-write client "after"))
      (net-conn-close (net-accept ln)))
  ) ;lambda
) ;call-with-net-listener

;; 测试项2：proc 异常中断后 conn 依然被自动关闭
(call-with-net-listener 'tcp "127.0.0.1:0"
  (lambda (ln)
    (let ((client (net-dial 'tcp (net-listener-addr ln))))
      (catch #t
        (lambda ()
          (call-with-net-conn client
            (lambda (conn)
              (net-conn-write conn "before-error")
              (error 'test-error "boom"))))
        (lambda (k e) #f))
      (check-catch 'net-error (net-conn-write client "after-error"))
      (net-conn-close (net-accept ln)))
  ) ;lambda
) ;call-with-net-listener

;; 测试项3：类型检查
(call-with-net-listener 'tcp "127.0.0.1:0"
  (lambda (ln)
    (let ((client (net-dial 'tcp (net-listener-addr ln))))
      (check-catch 'type-error (call-with-net-conn client 123))
      (net-conn-close client)
      (net-conn-close (net-accept ln)))
  ) ;lambda
) ;call-with-net-listener

(check-report)

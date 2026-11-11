;; call-with-net-listener
;; Guile 风格资源作用域组合子：创建监听器并在离开作用域时必定关闭。
;;
;; 语法
;; ----
;; (call-with-net-listener network address proc)
;;
;; 参数
;; ----
;; network : symbol? | string?
;; 网络协议类型（如 'tcp）。
;; address : string?
;; 监听地址（如 "127.0.0.1:0"）。
;; proc : procedure?
;; 接收 listener 的单参过程。
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

;; 测试项1：正常离开作用域后 listener 自动关闭
(let ((captured-ln #f))
  (let ((port (call-with-net-listener 'tcp "127.0.0.1:0"
                (lambda (ln)
                  (set! captured-ln ln)
                  (net-listener-port ln)))))
    (check-true (> port 0))
    ;; 离开后 accept 抛出 net-error 说明已关闭
    (check-catch 'net-error (net-accept captured-ln))))

;; 测试项2：proc 异常中断后 listener 依然被自动关闭
(let ((captured-ln #f))
  (catch #t
    (lambda ()
      (call-with-net-listener 'tcp "127.0.0.1:0"
        (lambda (ln)
          (set! captured-ln ln)
          (error 'test-error "boom"))))
    (lambda (k e) #f))
  (check-true (net-listener? captured-ln))
  (check-catch 'net-error (net-accept captured-ln)))

;; 测试项3：类型检查
(check-catch 'type-error (call-with-net-listener 'tcp "127.0.0.1:0" "not-a-proc"))

(check-report)

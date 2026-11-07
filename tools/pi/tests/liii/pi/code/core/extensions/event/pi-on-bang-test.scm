;; pi-on!
;; 为指定事件类型注册监听器。
;;
;; 语法
;; ----
;; (pi-on! target event-type handler)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; event-type : (or symbol string)
;; 事件类型。
;;
;; handler : procedure
;; 监听器过程。
;;
;; 返回值
;; ----
;; boolean
;; 注册成功返回 #t。

(import (scheme base)
        (liii check)
        (liii pi code core extensions event)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (check (pi-on! reg 'session_start (lambda (p c) 1)) => #t)
  (check (length (extension-registry-listeners reg)) => 1)
) ;let*

(let ((reg (make-extension-registry)))
  (pi-on! reg "session_start" (lambda (p c) 1))
  (check (length (extension-registry-listeners reg)) => 1)
  (check-catch 'type-error (pi-on! reg 'session_start "not-a-procedure"))
) ;let*

(check-report)

;; extension-registry-get-listeners
;; 读取注册表中指定事件类型的监听器列表。
;;
;; 语法
;; ----
;; (extension-registry-get-listeners target event-type)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; event-type : (or symbol string)
;; 事件类型。
;;
;; 返回值
;; ----
;; list
;; 监听器列表，未注册时返回空列表。

(import (scheme base)
        (liii check)
        (liii pi code core extensions registry base)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((reg (make-extension-registry)))
  (set-extension-registry-listeners! reg (list (list 'session_start (lambda (p c) 1))))
  (check (length (extension-registry-get-listeners reg 'session_start)) => 1)
  (check (extension-registry-get-listeners reg 'unknown) => '())
) ;let*

(check-report)

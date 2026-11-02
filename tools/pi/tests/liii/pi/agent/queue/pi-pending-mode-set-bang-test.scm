;; pi-pending-mode-set!
;; 设置待注入消息队列的投递模式，必须为 'one-at-a-time 或 'all。
;;
;; 语法
;; ----
;; (pi-pending-mode-set! q mode)
;;
;; 参数
;; ----
;; q : pi-pending-queue?，待配置的待注入消息队列。
;; mode : symbol?，待设置的模式符号（'one-at-a-time 或 'all）。
;;
;; 返回值
;; ----
;; unspecified，更新队列内部投递模式。
;;
;; 错误处理
;; ----
;; type-error 当 mode 既非 'one-at-a-time 也非 'all 时抛出。

(import (scheme base) (liii check) (liii pi agent queue))

(check-set-mode! 'report-failed)

;; 1. 正常设置模式
(let ((q (make-pi-pending-queue)))
  (pi-pending-mode-set! q 'all)
  (check (pi-pending-mode q) => 'all)
  (pi-pending-mode-set! q 'one-at-a-time)
  (check (pi-pending-mode q) => 'one-at-a-time)
) ;let

;; 2. 非法模式抛 type-error
(let ((q (make-pi-pending-queue)))
  (check-catch 'type-error (pi-pending-mode-set! q 'invalid-mode))
  (check-catch 'type-error (pi-pending-mode-set! q "all"))
  (check-catch 'type-error (pi-pending-mode-set! q 123))
) ;let

(check-report)

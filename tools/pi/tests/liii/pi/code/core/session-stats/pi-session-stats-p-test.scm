(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-stats?
;; 判断对象是否为完整会话统计 record。
;;
;; 语法
;; ----
;; (pi-session-stats? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; 仅对 pi-session-stats record 返回 #t。
;;
;; 示例
;; ----
;; (pi-session-stats? '()) => #f

(let ((stats (make-pi-session-stats #f #f #f 0 0 0 0 0
               (make-pi-session-token-stats 0 0 0 0) 0)))
  (check-true (pi-session-stats? stats)))
(check-false (pi-session-stats? #f))
(check-false (pi-session-stats? '()))
(check-false (pi-session-stats? (list (cons 'total-messages 0))))

(check-report)

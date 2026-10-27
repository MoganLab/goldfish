(import (scheme base)
  (liii check)
  (liii pi code core session-stats))

(check-set-mode! 'report-failed)

;; pi-session-token-stats?
;; 判断对象是否为会话 token 统计 record。
;;
;; 语法
;; ----
;; (pi-session-token-stats? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; 仅对 pi-session-token-stats record 返回 #t。
;;
;; 示例
;; ----
;; (pi-session-token-stats? '()) => #f

(check-true (pi-session-token-stats? (make-pi-session-token-stats 0 0 0 0)))
(check-false (pi-session-token-stats? #f))
(check-false (pi-session-token-stats? '()))
(check-false (pi-session-token-stats? (list (cons 'input 0))))

(check-report)

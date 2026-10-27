(import (scheme base)
  (liii check)
  (liii pi code core session-list))

(check-set-mode! 'report-failed)

;; pi-session-info?
;; 判断对象是否为会话摘要 record。
;;
;; 语法
;; ----
;; (pi-session-info? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ----
;; boolean
;; 仅对 pi-session-info record 返回 #t。
;;
;; 示例
;; ----
;; (pi-session-info? '()) => #f

(let ((info (make-pi-session-info "/tmp/session.jsonl" "session-1" "/tmp/project"
              #f "created" "modified" 0 "(no messages)" "")))
  (check-true (pi-session-info? info)))

(check-false (pi-session-info? #f))
(check-false (pi-session-info? '()))
(check-false (pi-session-info? (list (cons 'path "/tmp/session.jsonl"))))

(check-report)

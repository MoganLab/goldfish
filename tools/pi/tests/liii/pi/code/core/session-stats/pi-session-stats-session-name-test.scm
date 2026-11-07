;; pi-session-stats-session-name
;; 返回统计中的可选会话名称。
;;
;; 语法
;; ----
;; (pi-session-stats-session-name stats)
;;
;; 参数
;; ----
;; stats : pi-session-stats
;; 会话统计 record。
;;
;; 返回值
;; ----
;; string 或 #f
;; 会话名称，未命名时为 #f。
;;

(import (scheme base) (liii check) (liii pi code core session-stats))

(check-set-mode! 'report-failed)

(let ((named (make-pi-session-stats #f
               #f
               "Demo"
               0
               0
               0
               0
               0
               (make-pi-session-token-stats 0 0 0 0)
               0
             ) ;make-pi-session-stats
      ) ;named
      (unnamed (make-pi-session-stats #f
                 #f
                 #f
                 0
                 0
                 0
                 0
                 0
                 (make-pi-session-token-stats 0 0 0 0)
                 0
               ) ;make-pi-session-stats
      ) ;unnamed
     ) ;
  (check (pi-session-stats-session-name named) => "Demo")
  (check-false (pi-session-stats-session-name unnamed))
) ;let

(check-report)

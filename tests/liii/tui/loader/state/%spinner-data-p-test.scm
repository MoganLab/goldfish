;; %spinner-data?
;; 判断给定对象是否为内部微调器状态记录。
;;
;; 语法
;; ----
;; (%spinner-data? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 如果为微调器状态记录返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-spinner-data '("a") 0 "" "" "" 80 #t 'spinning #f #f)))
  (check-true (%spinner-data? d))
) ;let

(check-false (%spinner-data? "not-data"))
(check-false (%spinner-data? 123))

(check-report)

(import (liii check)
        (liii date-time date)
        (liii date-time moment)
        (liii date-time timezone)
        (liii date-time timestamp))

(check-set-mode! 'report-failed)

;; timestamp->moment
;; 将时间戳对象转换为时刻（moment）对象。
;;
;; 语法
;; ----
;; (timestamp->moment ts)
;;
;; 参数
;; ----
;; ts : timestamp?
;;
;; 返回值
;; ----
;; moment?
;;
;; 示例
;; ----
;; (timestamp->moment (make-timestamp 2026 2 3 3 2 3 (tz-timezone "Europe/Vilnius")))

(let* ((ts (make-timestamp 2026 2 3 3 2 3 (tz-timezone "Europe/Vilnius")))
       (m (timestamp->moment ts))
       (expected (make-moment (make-date 2026 2 3) (+ (* 1 3600) (* 2 60) 3 37))))
  (check-true (moment=? m expected)))

(check-catch 'type-error (timestamp->moment #f))

(check-report)

(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; seconds-delta
;; 创建仅包含秒数增量的 time-delta 对象。
;;
;; 语法
;; ----
;; (seconds-delta n)
;;
;; 参数
;; ----
;; n : rational? 精确有理数或整数。
;;
;; 返回值
;; ----
;; time-delta?
;;
;; 示例
;; ----
;; (time-delta-seconds (seconds-delta 45)) => 45
;; (time-delta-seconds (seconds-delta 3/4)) => 3/4
;;
;; 错误处理
;; --------
;; type-error 当参数不是精确数字时抛出错误。

(let ((dt (seconds-delta 45)))
  (check-true (time-delta? dt))
  (check (time-delta-seconds dt) => 45))

(let ((dt (seconds-delta 3/4)))
  (check (time-delta-seconds dt) => 3/4))

(check-catch 'type-error (seconds-delta 45.5))
(check-catch 'type-error (seconds-delta 'foo))

(check-report)

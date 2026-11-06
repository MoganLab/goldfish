(import (liii check)
        (liii date-time time-delta))

(check-set-mode! 'report-failed)

;; minutes-delta
;; 创建仅包含分钟增量的 time-delta 对象。
;;
;; 语法
;; ----
;; (minutes-delta n)
;;
;; 参数
;; ----
;; n : integer? 精确整数。
;;
;; 返回值
;; ----
;; time-delta?
;;
;; 示例
;; ----
;; (time-delta-minutes (minutes-delta 30)) => 30
;;
;; 错误处理
;; --------
;; type-error 当参数不是精确整数时抛出错误。

(let ((dt (minutes-delta 30)))
  (check-true (time-delta? dt))
  (check (time-delta-minutes dt) => 30))

(check-catch 'type-error (minutes-delta 30.5))

(check-report)

;; read-microsecond
;; 从 pos 起读取至多 6 位小数数字并按微秒补齐，返回值与新位置。
;;
;; 语法
;; ----
;; (read-microsecond input-str pos)
;;
;; 参数
;; ----
;; input-str : string? 输入串。
;; pos : integer? 起始位置。
;;
;; 返回值
;; ----
;; values number? 微秒数与 integer? 结束位置；无数字时抛 value-error。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (call-with-values (lambda () (read-microsecond "12" 0)) list)
  =>
  '(120000 2)
) ;check
(check (call-with-values (lambda () (read-microsecond "1234567" 0)) list)
  =>
  '(123456 6)
) ;check
(check-catch 'value-error (read-microsecond "abc" 0))

(check-report)

;; read-digits
;; 从 pos 起读取 10 进制数字，返回数值与新位置两个值。
;;
;; 语法
;; ----
;; (read-digits input-str pos min-n max-n)
;;
;; 参数
;; ----
;; input-str : string? 输入串。
;; pos : integer? 起始位置。
;; min-n : integer? 最少位数。
;; max-n : integer? 最多位数。
;;
;; 返回值
;; ----
;; values number? 读取的数值与 integer? 结束位置；不足 min-n 位抛 value-error。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (call-with-values (lambda () (read-digits "123abc" 0 1 4)) list)
  =>
  '(123 3)
) ;check
(check (call-with-values (lambda () (read-digits "12345" 0 2 3)) list)
  =>
  '(123 3)
) ;check
(check-catch 'value-error (read-digits "ab" 0 1 4))

(check-report)

;; read-fixed-digits
;; 从 pos 起读取恰好 n 位数字，返回数值与新位置两个值。
;;
;; 语法
;; ----
;; (read-fixed-digits input-str pos n)
;;
;; 参数
;; ----
;; input-str : string? 输入串。
;; pos : integer? 起始位置。
;; n : integer? 固定位数。
;;
;; 返回值
;; ----
;; values number? 读取的数值与 integer? 结束位置。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (call-with-values (lambda () (read-fixed-digits "12ab" 0 2)) list)
  =>
  '(12 2)
) ;check
(check (call-with-values (lambda () (read-fixed-digits "2021x" 0 4)) list)
  =>
  '(2021 4)
) ;check
(check-catch 'value-error (read-fixed-digits "1a" 0 2))

(check-report)

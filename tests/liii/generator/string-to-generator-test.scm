(import (liii check) (liii generator))

(check-set-mode! 'report-failed)

;; string->generator
;; 将字符串转换为依次产出其中字符的生成器。
;;
;; 语法
;; ----
;; (string->generator str)
;; (string->generator str start)
;; (string->generator str start end)
;;
;; 参数
;; ----
;; str : string
;; 待转换的字符串。
;;
;; start : exact-nonnegative-integer (可选)
;; 起始索引（包含），默认为 0。
;;
;; end : exact-nonnegative-integer (可选)
;; 结束索引（不包含），默认为 (string-length str)。
;;
;; 返回值
;; ----
;; procedure
;; 一个无参生成器过程。依次产出字符，耗尽时返回 eof-object。
;;
;; 错误处理
;; ----
;; 无

(let ((g (string->generator "ab")))
  (check (g) => #\a)
  (check (g) => #\b)
  (check-true (eof-object? (g)))
)

(let ((g (string->generator "hello" 1 3)))
  (check (g) => #\e)
  (check (g) => #\l)
  (check-true (eof-object? (g)))
)

(let ((g (string->generator "hello" 3)))
  (check (g) => #\l)
  (check (g) => #\o)
  (check-true (eof-object? (g)))
)

(check-report)

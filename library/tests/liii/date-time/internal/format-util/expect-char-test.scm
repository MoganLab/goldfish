;; expect-char
;; 校验输入串 pos 处字符是否为期望字符，返回新位置。
;;
;; 语法
;; ----
;; (expect-char input-str fmt pos ch)
;;
;; 参数
;; ----
;; input-str : string? 被解析的输入串。
;; fmt : string? 格式串（仅用于错误信息）。
;; pos : integer? 当前解析位置。
;; ch : char? 期望字符。
;;
;; 返回值
;; ----
;; integer? 匹配成功后的新位置；不匹配时抛出 value-error。

(import (liii check)
  (liii date-time time-delta)
  (liii date-time internal format-util)
) ;import

(check-set-mode! 'report-failed)

(check (expect-char "a-b" "fmt" 1 #\-) => 2)
(check-catch 'value-error (expect-char "a-b" "fmt" 0 #\-))
(check-catch 'value-error (expect-char "a" "fmt" 1 #\a))

(check-report)

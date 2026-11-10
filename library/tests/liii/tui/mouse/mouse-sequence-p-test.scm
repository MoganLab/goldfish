;; mouse-sequence?
;; 判断给定字符串是否为合法的 SGR 鼠标转义序列。
;;
;; 语法
;; ----
;; (mouse-sequence? str)
;;
;; 参数
;; ----
;; str : string?
;; 待检验的字符串。
;;
;; 返回值
;; ----
;; boolean?
;; 若符合 SGR 鼠标序列规范返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui mouse))

(check-set-mode! 'report-failed)

(check-true (mouse-sequence? "\x1b;[<0;20;5M"))
(check-true (mouse-sequence? "\x1b;[<0;20;5m"))
(check-true (mouse-sequence? "\x1b;[<64;1;1M"))
(check-false (mouse-sequence? "\x1b;[A"))
(check-false (mouse-sequence? "hello"))
(check-false (mouse-sequence? ""))

(check-report)

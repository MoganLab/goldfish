;; %bracketed-paste?
;; 判断给定字符串是否为完整的 bracketed paste 转义序列。
;;
;; 语法
;; ----
;; (%bracketed-paste? str)
;;
;; 参数
;; ----
;; str : string?
;; 待检验的字符串。
;;
;; 返回值
;; ----
;; boolean?
;; 符合 bracketed paste 规范返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(check-true (%bracketed-paste? "\x1b;[200~hello\x1b;[201~"))
(check-true (%bracketed-paste? "\x1b;[200~你好世界🎉\x1b;[201~"))
(check-false (%bracketed-paste? "hello"))
(check-false (%bracketed-paste? "\x1b;[200~hello"))

(check-report)

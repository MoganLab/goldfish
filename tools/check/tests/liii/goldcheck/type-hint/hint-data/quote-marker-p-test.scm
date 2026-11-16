;; quote-marker?
;; 判定对象是否为 quote 标记。
;;
;; 语法
;; ----
;; (quote-marker? x)
;;
;; 参数
;; ----
;; x : any   待检测的对象
;;
;; 返回值
;; ------
;; boolean
;; 是 quote 标记返回 #t，否则返回 #f。
;; [1292] 开启 symbol-quote? 后，read 对 'X 产生 (quote X)，首项即普通符号 quote。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. quote 符号判定
(check (quote-marker? 'quote) => #t)

;; 2. 非 quote 符号判定
(check (quote-marker? 'quasiquote) => #f)
(check (quote-marker? 123) => #f)

(check-report)

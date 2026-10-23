(import (liii check))
(import (liii goldfmt format reader))

(check-set-mode! 'report-failed)

;; format-inline-atom-or-quote
;; 将 atom 或 quote 形式的值内联格式化为文本。
;;
;; 语法
;; ----
;; (format-inline-atom-or-quote value)
;;
;; 参数
;; ----
;; value : any?
;;
;; 返回值
;; ------
;; string?

;; 测试：symbol 值
(check (format-inline-atom-or-quote 'abc) => "abc")

;; 测试：number 值
(check (format-inline-atom-or-quote 42) => "42")

;; 测试：quote 形式还原为 '
(check (format-inline-atom-or-quote (read (open-input-string "'(a b)"))) => "'(a b)")

;; 测试：普通 list
(check (format-inline-atom-or-quote '(a b)) => "(a b)")

(check-report)

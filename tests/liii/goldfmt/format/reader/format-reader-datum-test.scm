(import (liii check))
(import (liii goldfmt format reader) (liii goldfmt scan))

(check-set-mode! 'report-failed)

;; format-reader-datum
;; 将 datum 按 reader 风格格式化为文本（零缩进）。
;; quote 家族还原为 ' ` , ,@ 前缀写法。
;;
;; 语法
;; ----
;; (format-reader-datum datum)
;;
;; 参数
;; ----
;; datum : any?
;;
;; 返回值
;; ------
;; string?

;; 测试：普通 list
(check (format-reader-datum '(+ 1 2)) => "(+ 1 2)")

;; 测试：quote 还原为 '（经 read 读出的 quote 为 syntax 对象）
(check (format-reader-datum (read (open-input-string "'(a b)"))) => "'(a b)")

;; 测试：嵌套结构
(check (format-reader-datum '(a (b c))) => "(a (b c))")

;; 测试：vector 字面量
(check (format-reader-datum #(1 2)) => "#(1 2)")

;; 测试：dotted pair
(check (format-reader-datum '(a . b)) => "(a . b)")

(check-report)

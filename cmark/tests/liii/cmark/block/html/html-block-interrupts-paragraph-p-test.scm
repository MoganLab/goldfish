;; html-block-interrupts-paragraph?
;; 判断 HTML 块起始行是否可中断开放段落（类型 1~6）。
;;
;; 语法
;; ----
;; (html-block-interrupts-paragraph? line)
;;
;; 参数
;; ----
;; line : string
;; 输入行字符串。
;;
;; 返回值
;; ----
;; boolean
;; 可中断段落返回 #t，否则返回 #f。

(import (scheme base))
(import (liii check))
(import (liii cmark block html))

(check-set-mode! 'report-failed)

(check-true (html-block-interrupts-paragraph? "<div>"))
(check-false (html-block-interrupts-paragraph? "<span-custom>"))

(check-report)


(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark inline math))

(check-set-mode! 'report-failed)

;; math-inline-start?
;; 判断字符串某个位置是否是行内数学公式的起始。
;;
;; 语法
;; ----
;; (math-inline-start? str pos)
;;
;; 返回值
;; ----
;; boolean
;; 当 pos 处是 "$" 或 "\(" 起始时返回 #t。
;;
;; 说明
;; ----
;; \( 是数学扩展定界（0111 起解析器原生支持，替代 preprocess-latex 预处理）；
;; \\( 由转义分支先行消费，主循环不会停在被转义的 \( 上。

(check-true (math-inline-start? "$math$" 0))
(check-false (math-inline-start? "text" 0))
(check-true (math-inline-start? "a$b" 1))

;; \( 定界
(check-true (math-inline-start? "\\(x\\)" 0))
(check-true (math-inline-start? "a\\(x\\)" 1))
(check-false (math-inline-start? "\\\\(x\\)" 0))
(check-false (math-inline-start? "\\[x\\]" 0))
(check-false (math-inline-start? "\\" 0))

(check-report)

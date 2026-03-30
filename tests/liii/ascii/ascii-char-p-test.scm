(import (liii check)
        (liii ascii))

;; ascii-char?
;; 判断对象是否为 ASCII 字符。
;;
;; 语法
;; ----
;; (ascii-char? x)
;;
;; 参数
;; ----
;; x : any?
;; 要判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是 ASCII 范围内的字符则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 支持普通字符与控制字符的判断。
;;
;; 示例
;; ----
;; (ascii-char? #\A) => #t
;; (ascii-char? #\x80) => #f
;;
;; 错误处理
;; ----
;; 非字符输入返回 #f

(check-true (ascii-char? #\A))
(check-true (ascii-char? #\newline))
(check-false (ascii-char? #\x80))
(check-false (ascii-char? 65))

(check-report)

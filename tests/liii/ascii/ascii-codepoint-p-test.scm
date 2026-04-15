(import (liii check) (liii ascii))


;; ascii-codepoint?
;; 判断对象是否为 ASCII 码点。
;;
;; 语法
;; ----
;; (ascii-codepoint? x)
;;
;; 参数
;; ----
;; x : any?
;; 要判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 如果x是范围在0到#x7f之间的整数则返回#t，否则返回#f。
;;
;; 注意
;; ----
;; 该判断覆盖 ASCII 码点上下边界。
;;
;; 示例
;; ----
;; (ascii-codepoint? 0) => #t
;; (ascii-codepoint? #x80) => #f
;;
;; 错误处理
;; ----
;; 非整数输入返回 #f


(check-true (ascii-codepoint? 0))
(check-true (ascii-codepoint? 127))
(check-false (ascii-codepoint? -1))
(check-false (ascii-codepoint? 128))
(check-false (ascii-codepoint? #\A))


(check-report)

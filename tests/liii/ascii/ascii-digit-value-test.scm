(import (liii check)
        (liii ascii))

;; ascii-digit-value
;; 将 ASCII 数字字符映射为数值。
;;
;; 语法
;; ----
;; (ascii-digit-value x limit)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要转换的字符或码点。
;;
;; limit : integer?
;; 可接受的数值上界（不包含）。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 返回对应数值；若字符非法或超出limit则返回#f。
;;
;; 注意
;; ----
;; 适合做任意进制解析中的数字部分映射。
;;
;; 示例
;; ----
;; (ascii-digit-value #\0 10) => 0
;; (ascii-digit-value #\9 9) => #f
;;
;; 错误处理
;; ----
;; 非法字符或越界值返回 #f

(check (ascii-digit-value #\0 10) => 0)
(check (ascii-digit-value #\9 10) => 9)
(check (ascii-digit-value #\9 9) => #f)
(check (ascii-digit-value #\A 10) => #f)

(check-report)

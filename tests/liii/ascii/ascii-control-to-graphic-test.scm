(import (liii check)
        (liii ascii)
) ;import

;; ascii-control->graphic
;; 将 ASCII 控制字符映射为图形字符。
;;
;; 语法
;; ----
;; (ascii-control->graphic x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要转换的控制字符或码点。
;;
;; 返回值
;; ----
;; char | integer | #f
;; 返回与输入同类型的图形字符；不可转换时返回#f。
;;
;; 注意
;; ----
;; 常见映射包括 #x00 -> #x40、#x7f -> #x3f。
;;
;; 示例
;; ----
;; (ascii-control->graphic #x00) => #x40
;; (ascii-control->graphic #x20) => #f
;;
;; 错误处理
;; ----
;; 不可转换输入返回 #f

(check (ascii-control->graphic #x00) => #x40)
(check (ascii-control->graphic #x1f) => #x5f)
(check (ascii-control->graphic #x7f) => #x3f)
(check (ascii-control->graphic #\x7f) => #\?)
(check (ascii-control->graphic #x20) => #f)

(check-report)

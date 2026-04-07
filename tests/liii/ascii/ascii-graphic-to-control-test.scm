(import (liii check)
        (liii ascii)
) ;import

;; ascii-graphic->control
;; 将 ASCII 图形字符映射为控制字符。
;;
;; 语法
;; ----
;; (ascii-graphic->control x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要转换的图形字符或码点。
;;
;; 返回值
;; ----
;; char | integer | #f
;; 返回与输入同类型的控制字符；不可转换时返回#f。
;;
;; 注意
;; ----
;; 常见映射包括 #x40 -> #x00、#x3f -> #x7f。
;;
;; 示例
;; ----
;; (ascii-graphic->control #x40) => #x00
;; (ascii-graphic->control #x20) => #f
;;
;; 错误处理
;; ----
;; 不可转换输入返回 #f

(check (ascii-graphic->control #x40) => #x00)
(check (ascii-graphic->control #x5f) => #x1f)
(check (ascii-graphic->control #x3f) => #x7f)
(check (ascii-graphic->control #\@) => #\nul)
(check (ascii-graphic->control #\A) => #\x01)
(check (ascii-graphic->control #x20) => #f)

(check-report)

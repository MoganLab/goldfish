(import (liii check)
        (liii ascii)
) ;import

;; ascii-nth-upper-case
;; 将数值映射为 ASCII 大写字母。
;;
;; 语法
;; ----
;; (ascii-nth-upper-case n)
;;
;; 参数
;; ----
;; n : integer?
;; 要映射的序号。
;;
;; 返回值
;; ----
;; char
;; 返回对应的大写字母字符。
;;
;; 注意
;; ----
;; 该映射按 26 个字母循环。
;;
;; 示例
;; ----
;; (ascii-nth-upper-case 0) => #\A
;; (ascii-nth-upper-case 26) => #\A
;;
;; 错误处理
;; ----
;; 按过程定义执行映射

(check (ascii-nth-upper-case 0) => #\A)
(check (ascii-nth-upper-case 25) => #\Z)
(check (ascii-nth-upper-case 26) => #\A)

(check-report)

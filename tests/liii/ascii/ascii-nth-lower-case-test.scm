(import (liii check)
        (liii ascii)
) ;import

;; ascii-nth-lower-case
;; 将数值映射为 ASCII 小写字母。
;;
;; 语法
;; ----
;; (ascii-nth-lower-case n)
;;
;; 参数
;; ----
;; n : integer?
;; 要映射的序号。
;;
;; 返回值
;; ----
;; char
;; 返回对应的小写字母字符。
;;
;; 注意
;; ----
;; 该映射按 26 个字母循环。
;;
;; 示例
;; ----
;; (ascii-nth-lower-case 0) => #\a
;; (ascii-nth-lower-case 26) => #\a
;;
;; 错误处理
;; ----
;; 按过程定义执行映射

(check (ascii-nth-lower-case 0) => #\a)
(check (ascii-nth-lower-case 25) => #\z)
(check (ascii-nth-lower-case 26) => #\a)

(check-report)

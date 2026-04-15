(import (liii check) (liii ascii))


;; ascii-nth-digit
;; 将数值映射为 ASCII 数字字符。
;;
;; 语法
;; ----
;; (ascii-nth-digit n)
;;
;; 参数
;; ----
;; n : integer?
;; 要映射的序号。
;;
;; 返回值
;; ----
;; char 或 #f
;; 当n位于0到9之间时返回对应数字字符，否则返回#f。
;;
;; 注意
;; ----
;; 覆盖数字字符的上下边界。
;;
;; 示例
;; ----
;; (ascii-nth-digit 0) => #\0
;; (ascii-nth-digit 10) => #f
;;
;; 错误处理
;; ----
;; 越界序号返回 #f


(check (ascii-nth-digit 0) => #\0)
(check (ascii-nth-digit 9) => #\9)
(check (ascii-nth-digit -1) => #f)
(check (ascii-nth-digit 10) => #f)


(check-report)

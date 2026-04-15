(import (liii check) (liii ascii))


;; ascii-upper-case-value
;; 将 ASCII 大写字母映射为数值。
;;
;; 语法
;; ----
;; (ascii-upper-case-value x offset limit)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要转换的字符或码点。
;;
;; offset : integer?
;; 起始偏移量。
;;
;; limit : integer?
;; 可接受字母个数上界。
;;
;; 返回值
;; ----
;; integer 或 #f
;; 返回映射后的数值；非法输入返回#f。
;;
;; 注意
;; ----
;; 常用于十六进制等进制解析中的大写字母部分。
;;
;; 示例
;; ----
;; (ascii-upper-case-value #\A 10 26) => 10
;; (ascii-upper-case-value #\Q 10 16) => #f
;;
;; 错误处理
;; ----
;; 非法字符或越界值返回 #f


(check (ascii-upper-case-value #\A 10 26)
  =>
  10
) ;check
(check (ascii-upper-case-value #\F 10 16)
  =>
  15
) ;check
(check (ascii-upper-case-value #\Q 10 16)
  =>
  #f
) ;check


(check-report)

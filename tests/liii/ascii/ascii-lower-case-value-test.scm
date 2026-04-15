(import (liii check) (liii ascii))


;; ascii-lower-case-value
;; 将 ASCII 小写字母映射为数值。
;;
;; 语法
;; ----
;; (ascii-lower-case-value x offset limit)
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
;; 常用于十六进制等进制解析中的小写字母部分。
;;
;; 示例
;; ----
;; (ascii-lower-case-value #\a 10 26) => 10
;; (ascii-lower-case-value #\q 10 16) => #f
;;
;; 错误处理
;; ----
;; 非法字符或越界值返回 #f


(check (ascii-lower-case-value #\a 10 26)
  =>
  10
) ;check
(check (ascii-lower-case-value #\f 10 16)
  =>
  15
) ;check
(check (ascii-lower-case-value #\q 10 16)
  =>
  #f
) ;check


(check-report)

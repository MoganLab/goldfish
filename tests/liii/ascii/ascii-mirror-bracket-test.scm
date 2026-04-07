(import (liii check)
        (liii ascii)
) ;import

;; ascii-mirror-bracket
;; 获取括号的镜像字符。
;;
;; 语法
;; ----
;; (ascii-mirror-bracket x)
;;
;; 参数
;; ----
;; x : char? | integer?
;; 要映射的括号字符或码点。
;;
;; 返回值
;; ----
;; char | integer | #f
;; 返回配对括号；若x不是支持的括号字符则返回#f。
;;
;; 注意
;; ----
;; 支持圆括号、方括号、花括号和尖括号。
;;
;; 示例
;; ----
;; (ascii-mirror-bracket #\() => #\)
;; (ascii-mirror-bracket #\A) => #f
;;
;; 错误处理
;; ----
;; 不可转换输入返回 #f

(check (ascii-mirror-bracket #\() => #\))
(check (ascii-mirror-bracket #\]) => #\[)
(check (ascii-mirror-bracket #\>) => #\<)
(check (ascii-mirror-bracket #\A) => #f)
(check (ascii-mirror-bracket 40) => 41)

(check-report)

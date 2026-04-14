(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; number->string
;; 将数值转换为字符串表示。
;;
;; 语法
;; ----
;; (number->string num)
;; (number->string num radix)
;;
;; 参数
;; ----
;; num : number?
;; 要转换为字符串的数值，支持整数、实数、有理数、复数等各种数值类型。
;;
;; radix : exact?
;; 可选参数，指定转换的进制。必须是精确的整数，范围在2到16之间（包含2和16）。
;; 当不指定时，默认为10进制。
;;
;; 返回值
;; ------
;; string?
;; 返回给定数值的字符串表示形式。
;;
;; 说明
;; ----
;; 1. 对于整数，返回整数字符串表示
;; 2. 对于实数，返回小数格式的字符串
;; 3. 对于有理数，返回"分子/分母"格式的字符串
;; 4. 对于复数，返回"实部+虚部i"格式的字符串
;; 5. 指定进制时，返回指定进制的字符串表示（仅适用于有理的实数部分）
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是数值或进制不是精确的整数时抛出错误。
;; out-of-range
;; 当进制不在2到16范围内时抛出错误。
;; wrong-number-of-args
;; 当参数数量不为1或2时抛出错误。

;; 基本整数转换测试
(check (number->string 123) => "123")
(check (number->string 0) => "0")
(check (number->string -456) => "-456")
(check (number->string 2147483647) => "2147483647")
(check (number->string -2147483648) => "-2147483648")

;; 基本进制转换测试
(check (number->string 123 2) => "1111011")
(check (number->string 123 8) => "173")
(check (number->string 255 16) => "ff")
(check (number->string 255 10) => "255")

;; 有理数转换测试
(check (number->string 1/2) => "1/2")
(check (number->string -1/3) => "-1/3")
(check (number->string 22/7) => "22/7")
(check (number->string 0/1) => "0")

;; 有理数进制转换测试
(check (number->string 1/2 2) => "1/10")
(check (number->string 3/4 2) => "11/100")
(check (number->string 1/3 16) => "1/3")

;; 浮点数转换测试
(check (number->string 123.456) => "123.456")
(check (number->string 0.0) => "0.0")
(check (number->string -0.123) => "-0.123")
(check (number->string 1.23e10) => "1.23e+10")
(check (number->string 0.00123) => "0.00123")
(check (number->string 1.23e-3) => "0.00123")

;; 复数转换测试
(check (number->string 1+2i) => "1.0+2.0i")
(check (number->string 0+2i) => "0.0+2.0i")
(check (number->string -3+4i) => "-3.0+4.0i")
(check (number->string 1.5-2.5i) => "1.5-2.5i")
(check (number->string 0+1i) => "0.0+1.0i")
(check (number->string 0+0i) => "0.0")
(check (number->string 1.0+0.0i) => "1.0")

;; 边界测试
(check (number->string 1 2) => "1")
(check (number->string 0 16) => "0")
(check (number->string -128 16) => "-80")
(check (number->string 1023 2) => "1111111111")

;; 错误处理测试
(check-catch 'wrong-type-arg (number->string 'not-a-number))
(check-catch 'wrong-type-arg (number->string 123 'not-a-number))
(check-catch 'out-of-range (number->string 123 1)) 
(check-catch 'out-of-range (number->string 123 37))
(check-catch 'wrong-type-arg (number->string 123 3.5))
(check-catch 'wrong-number-of-args (number->string))
(check-catch 'wrong-number-of-args (number->string 123 2 3))

(check-report)

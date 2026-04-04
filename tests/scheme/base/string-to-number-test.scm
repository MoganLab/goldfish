(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; string->number
;; 将字符串解析为数值。根据R7RS规范，支持多种数值格式的解析。
;;
;; 语法
;; ----
;; (string->number str)
;; (string->number str radix)
;;
;; 参数
;; ----
;; str : string?
;; 要解析为数值的字符串。支持整数、实数、有理数、复数、浮点数科学计数法等格式。
;; radix : exact-integer?
;; 可选参数，指定解析的进制。必须是精确的整数，范围在2到16之间（包含2和16）。
;; 当不指定时，默认为10进制。
;;
;; 返回值
;; ------
;; number? | #f
;; 如果字符串可以成功解析为数值，则返回对应的数值，否则返回#f。
;;
;; 说明
;; ----
;; string->number 支持解析以下格式：
;; - 整数："123", "-456"
;; - 浮点数："123.456", "-0.123"
;; - 科学计数法："1.23e10", "1.23e-3"
;; - 有理数："1/2", "-22/7"
;; - 复数："1+2i", "3.14-2.71i"
;; - 不同进制：二进制"1010", 八进制"755", 十六进制"FF"
;;
;; 错误情况
;; -------
;; 当radix参数超出有效范围（2-16）时，行为未定义（S7中返回#f）。

;; 基本整数解析测试
(check (string->number "123") => 123)
(check (string->number "0") => 0)
(check (string->number "-456") => -456)
(check (string->number "2147483647") => 2147483647)
(check (string->number "-2147483648") => -2147483648)

;; 基本进制解析测试
(check (string->number "1111011" 2) => 123)
(check (string->number "173" 8) => 123)
(check (string->number "ff" 16) => 255)
(check (string->number "255" 10) => 255)
(check (string->number "10" 2) => 2)
(check (string->number "77" 8) => 63)

;; 浮点数解析测试
(check (string->number "123.456") => 123.456)
(check (string->number "0.0") => 0.0)
(check (string->number "-0.123") => -0.123)
(check (string->number "1.23e10") => 1.23e10)
(check-true (< (abs (- (string->number "1.23e-3") 0.00123)) 1e-10))
(check (string->number "1e5") => 100000.0)
(check (string->number "1e-5") => 0.00001)

;; 有理数解析测试
(check (string->number "1/2") => 1/2)
(check (string->number "-1/3") => -1/3)
(check (string->number "22/7") => 22/7)
(check (string->number "0/1") => 0)
(check (string->number "-22/7") => -22/7)

;; 有理数进制解析测试
(check (string->number "1/10" 2) => 1/2)
(check (string->number "11/100" 2) => 3/4)

;; 复数解析测试
(check (string->number "1+2i") => 1+2i)
(check (string->number "0+2i") => 0+2i)
(check (string->number "-3+4i") => -3+4i)
(check (string->number "3.14-2.71i") => 3.14-2.71i)
(check (string->number "0+1i") => 0+1i)
(check (string->number "0+0i") => 0.0)
(check (string->number "1.0+0.0i") => 1.0)
(check (string->number "-2.5-1.5i") => -2.5-1.5i)

;; 无效字符串解析测试（应返回#f）
(check (string->number "abc") => #f)
(check (string->number "123abc") => #f)
(check (string->number "abc123") => #f)
(check (string->number "1.2.3") => #f)
(check (string->number "1/2/3") => #f)
(check (string->number "1+i+i") => #f)
(check (string->number "") => #f)
(check (string->number "   ") => #f)
(check (string->number "1 2") => #f)

;; 边界测试
(check (string->number "1" 2) => 1)
(check (string->number "0" 16) => 0)
(check (string->number "-80" 16) => -128)
(check (string->number "1111111111" 2) => 1023)

;; 十六进制测试
(check (string->number "FF" 16) => 255)
(check (string->number "-FF" 16) => -255)
(check (string->number "A" 16) => 10)
(check (string->number "a" 16) => 10)

;; 错误处理测试（无效进制）
(check-catch 'out-of-range (string->number "123" 1))
(check-catch 'out-of-range (string->number "123" 17))
(check-catch 'out-of-range (string->number "123" 0))
(check-catch 'out-of-range (string->number "123" -1))

;; 错误参数测试
(check-catch 'wrong-type-arg (string->number 123))
(check-catch 'wrong-type-arg (string->number 'symbol))
(check-catch 'wrong-type-arg (string->number #t))
(check-catch 'wrong-type-arg (string->number "123" 'not-a-number))
(check-catch 'wrong-type-arg (string->number "123" 3.5))
(check-catch 'wrong-number-of-args (string->number))
(check-catch 'wrong-number-of-args (string->number "123" 2 3))

; R7RS Section 6.3 Booleans

(check-report)

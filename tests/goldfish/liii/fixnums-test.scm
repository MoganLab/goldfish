;
; Copyright (C) 2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (srfi srfi-143)
        (liii check))

(check-set-mode! 'report-failed)


#|
fx-greatest
fixnum 最大值常量

语法
----
fx-greatest

返回值
----
integer?

边界行为
----
等于 (*s7* 'most-positive-fixnum)。

性能边界
----
常量时间读取。

错误处理
----
无。
|#

(check fx-greatest => (*s7* 'most-positive-fixnum))


#|
fx-least
fixnum 最小值常量

语法
----
fx-least

返回值
----
integer?

边界行为
----
等于 (*s7* 'most-negative-fixnum)。

性能边界
----
常量时间读取。

错误处理
----
无。
|#

(check fx-least => (*s7* 'most-negative-fixnum))


#|
fx-width
fixnum 位宽常量

语法
----
fx-width

返回值
----
integer?

边界行为
----
等于 (fxlength fx-greatest) + 1。

性能边界
----
常量时间读取。

错误处理
----
无。
|#

(check fx-width => (+ (fxlength fx-greatest) 1))


#|
fixnum?
判断对象是否为 fixnum

语法
----
(fixnum? x)

参数
----
x : any

返回值
----
boolean?

边界行为
----
覆盖 fx-least 与 fx-greatest 边界。

性能边界
----
常量时间判断。

错误处理
----
非整数输入返回 #f。
|#

(check-true (fixnum? 0))
(check-true (fixnum? fx-greatest))
(check-true (fixnum? fx-least))
(check-false (fixnum? 1.0))
(check-false (fixnum? #\A))


#|
fx=?
fixnum 相等比较

语法
----
(fx=? i ...)

参数
----
i : fixnum?

返回值
----
boolean?

边界行为
----
支持多参数比较。

性能边界
----
与参数个数线性相关。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check-true (fx=? 1 1 1))
(check-false (fx=? 1 2))


#|
fx<?
fixnum 小于比较

语法
----
(fx<? i ...)

参数
----
i : fixnum?

返回值
----
boolean?

边界行为
----
支持多参数链式比较。

性能边界
----
与参数个数线性相关。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check-true (fx<? -1 0 1))


#|
fx>?
fixnum 大于比较

语法
----
(fx>? i ...)

参数
----
i : fixnum?

返回值
----
boolean?

边界行为
----
支持多参数链式比较。

性能边界
----
与参数个数线性相关。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check-true (fx>? 3 2 1))


#|
fx<=?
fixnum 小于等于比较

语法
----
(fx<=? i ...)

参数
----
i : fixnum?

返回值
----
boolean?

边界行为
----
支持多参数链式比较。

性能边界
----
与参数个数线性相关。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check-true (fx<=? 1 1 2))


#|
fx>=?
fixnum 大于等于比较

语法
----
(fx>=? i ...)

参数
----
i : fixnum?

返回值
----
boolean?

边界行为
----
支持多参数链式比较。

性能边界
----
与参数个数线性相关。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check-true (fx>=? 3 3 2))


#|
fxzero?
判断 fixnum 是否为 0

语法
----
(fxzero? i)

参数
----
i : fixnum?

返回值
----
boolean?

边界行为
----
仅对 fixnum 有意义。

性能边界
----
常量时间判断。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check-true (fxzero? 0))


#|
fxpositive?
判断 fixnum 是否为正数

语法
----
(fxpositive? i)

参数
----
i : fixnum?

返回值
----
boolean?

边界行为
----
与 positive? 一致。

性能边界
----
常量时间判断。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check-true (fxpositive? 2))


#|
fxnegative?
判断 fixnum 是否为负数

语法
----
(fxnegative? i)

参数
----
i : fixnum?

返回值
----
boolean?

边界行为
----
与 negative? 一致。

性能边界
----
常量时间判断。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check-true (fxnegative? -1))


#|
fxodd?
判断 fixnum 是否为奇数

语法
----
(fxodd? i)

参数
----
i : fixnum?

返回值
----
boolean?

边界行为
----
与 odd? 一致。

性能边界
----
常量时间判断。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check-true (fxodd? 3))


#|
fxeven?
判断 fixnum 是否为偶数

语法
----
(fxeven? i)

参数
----
i : fixnum?

返回值
----
boolean?

边界行为
----
与 even? 一致。

性能边界
----
常量时间判断。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check-true (fxeven? 4))


#|
fxmax
fixnum 最大值

语法
----
(fxmax i ...)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
支持多参数。

性能边界
----
与参数个数线性相关。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check (fxmax 1 5 2) => 5)
(check (fxmax fx-least 0 fx-greatest) => fx-greatest)


#|
fxmin
fixnum 最小值

语法
----
(fxmin i ...)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
支持多参数。

性能边界
----
与参数个数线性相关。

错误处理
----
非 fixnum 可能触发类型错误。
|#

(check (fxmin 1 5 2) => 1)
(check (fxmin fx-least 0 fx-greatest) => fx-least)


#|
fx+
fixnum 加法

语法
----
(fx+ i j)

参数
----
i, j : fixnum?

返回值
----
fixnum?

边界行为
----
溢出违反 fixnum 规则。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fx+ 1 2) => 3)
(check (fx+ fx-greatest 0) => fx-greatest)
(check (fx+ fx-least 0) => fx-least)
(check-catch 'type-error (fx+ 1 1.0))


#|
fx-
fixnum 减法

语法
----
(fx- i j)

参数
----
i, j : fixnum?

返回值
----
fixnum?

边界行为
----
溢出违反 fixnum 规则。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fx- 5 3) => 2)
(check (fx- fx-greatest 0) => fx-greatest)
(check (fx- fx-least 0) => fx-least)


#|
fxneg
fixnum 取负

语法
----
(fxneg i)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
对 fx-least 取负违反 fixnum 规则。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxneg 3) => -3)
(check (fxneg 0) => 0)
(check-catch 'out-of-range (fxneg fx-least))


#|
fx*
fixnum 乘法

语法
----
(fx* i j)

参数
----
i, j : fixnum?

返回值
----
fixnum?

边界行为
----
溢出违反 fixnum 规则。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fx* 6 7) => 42)
(check (fx* 0 fx-greatest) => 0)
(check (fx* -1 1) => -1)


#|
fxquotient
fixnum 商

语法
----
(fxquotient i j)

参数
----
i, j : fixnum?

返回值
----
fixnum?

边界行为
----
遵循 quotient 行为。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxquotient 7 3) => 2)
(check (fxquotient -7 3) => -2)
(check (fxquotient 7 -3) => -2)


#|
fxremainder
fixnum 余数

语法
----
(fxremainder i j)

参数
----
i, j : fixnum?

返回值
----
fixnum?

边界行为
----
遵循 remainder 行为。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxremainder 7 3) => 1)
(check (fxremainder -7 3) => -1)
(check (fxremainder 7 -3) => 1)


#|
fxabs
fixnum 绝对值

语法
----
(fxabs i)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
对 fx-least 调用违反 fixnum 规则。

性能边界
----
常量时间。

错误处理
----
fx-least 触发 out-of-range。
|#

(check (fxabs -5) => 5)
(check (fxabs 0) => 0)
(check-catch 'out-of-range (fxabs fx-least))


#|
fxsquare
fixnum 平方

语法
----
(fxsquare i)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
溢出违反 fixnum 规则。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxsquare 9) => 81)
(check (fxsquare 0) => 0)


#|
fxsqrt
fixnum 整数平方根

语法
----
(fxsqrt i)

参数
----
i : fixnum?

返回值
----
values
返回 (values s r)，其中 s 为 floor(sqrt(i))，r 为余数。

边界行为
----
与 exact-integer-sqrt 一致。

性能边界
----
与输入位宽相关。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (call-with-values (lambda () (fxsqrt 10)) list) => '(3 1))
(check (call-with-values (lambda () (fxsqrt 81)) list) => '(9 0))
(check (call-with-values (lambda () (fxsqrt 0)) list) => '(0 0))


#|
fx+/carry
fixnum 加法带进位

语法
----
(fx+/carry i j k)

参数
----
i, j, k : fixnum?

返回值
----
values
返回 (values sum carry)。

边界行为
----
carry 取值为 -1/0/1。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (call-with-values (lambda () (fx+/carry 1 2 3)) list) => '(6 0))
(check (call-with-values (lambda () (fx+/carry fx-greatest 1 0)) list) => (list fx-least 1))
(check (call-with-values (lambda () (fx+/carry fx-greatest 0 0)) list) => (list fx-greatest 0))
(check (call-with-values
         (lambda ()
           (fx+/carry fx-greatest fx-greatest fx-greatest))
         list)
       => (list (- fx-greatest 2) 1))


#|
fx-/carry
fixnum 减法带借位

语法
----
(fx-/carry i j k)

参数
----
i, j, k : fixnum?

返回值
----
values
返回 (values diff carry)。

边界行为
----
carry 取值为 -1/0/1。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (call-with-values
         (lambda ()
           (fx-/carry fx-least fx-greatest 0))
         list)
       => '(1 -1))
(check (call-with-values (lambda () (fx-/carry fx-least 1 0)) list) => (list fx-greatest -1))
(check (call-with-values (lambda () (fx-/carry fx-least 0 0)) list) => (list fx-least 0))


#|
fx*/carry
fixnum 乘法带进位

语法
----
(fx*/carry i j k)

参数
----
i, j, k : fixnum?

返回值
----
values
返回 (values prod carry)。

边界行为
----
carry 取值为 -1/0/1。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (call-with-values (lambda () (fx*/carry 2 3 4)) list) => '(10 0))
(check (call-with-values (lambda () (fx*/carry -2 3 0)) list) => '(-6 0))
(check (call-with-values (lambda () (fx*/carry fx-greatest 1 0)) list) => (list fx-greatest 0))


#|
fxnot
fixnum 按位取反

语法
----
(fxnot i)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
等价于 bitwise-not。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxnot 0) => -1)
(check (fxnot fx-greatest) => fx-least)
(check (fxand) => -1)
(check (fxior) => 0)
(check (fxxor) => 0)


#|
fxand
fixnum 按位与

语法
----
(fxand i ...)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
空参数返回 -1。

性能边界
----
与参数个数线性相关。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxand #b1100 #b1010) => #b1000)
(check (fxand -1 #b1010) => #b1010)


#|
fxior
fixnum 按位或

语法
----
(fxior i ...)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
空参数返回 0。

性能边界
----
与参数个数线性相关。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxior #b1100 #b1010) => #b1110)
(check (fxior 0 fx-least) => fx-least)


#|
fxxor
fixnum 按位异或

语法
----
(fxxor i ...)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
空参数返回 0。

性能边界
----
与参数个数线性相关。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxxor #b1100 #b1010) => #b0110)
(check (fxxor -1 -1) => 0)


#|
fxarithmetic-shift
fixnum 算术移位

语法
----
(fxarithmetic-shift i count)

参数
----
i : fixnum?
count : fixnum?

返回值
----
fixnum?

边界行为
----
|count| 不能超过 fx-width - 1。

性能边界
----
与位移量相关。

错误处理
----
位移越界触发 out-of-range。
|#

(check (fxarithmetic-shift 1 3) => 8)
(check (fxarithmetic-shift 8 -1) => 4)
(check (fxarithmetic-shift 1 (- fx-width 1)) => fx-least)
(check (fxarithmetic-shift fx-least (- 1 fx-width)) => -1)
(check-catch 'out-of-range (fxarithmetic-shift 1 fx-width))


#|
fxarithmetic-shift-left
fixnum 左移

语法
----
(fxarithmetic-shift-left i count)

参数
----
i : fixnum?
count : fixnum?

返回值
----
fixnum?

边界行为
----
count 必须为非负且小于 fx-width。

性能边界
----
与位移量相关。

错误处理
----
位移越界触发 out-of-range。
|#

(check (fxarithmetic-shift-left 1 4) => 16)
(check-catch 'out-of-range (fxarithmetic-shift-left 1 -1))
(check (fxarithmetic-shift-left 1 (- fx-width 1)) => fx-least)


#|
fxarithmetic-shift-right
fixnum 右移

语法
----
(fxarithmetic-shift-right i count)

参数
----
i : fixnum?
count : fixnum?

返回值
----
fixnum?

边界行为
----
count 必须为非负且小于 fx-width。

性能边界
----
与位移量相关。

错误处理
----
位移越界触发 out-of-range。
|#

(check (fxarithmetic-shift-right 16 2) => 4)
(check-catch 'out-of-range (fxarithmetic-shift-right 1 -1))
(check (fxarithmetic-shift-right fx-least (- fx-width 1)) => -1)


#|
fxbit-count
fixnum 1 位计数

语法
----
(fxbit-count i)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
对负数使用补码语义。

性能边界
----
与位宽相关。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxbit-count #b101101) => 4)
(check (fxbit-count 0) => 0)
(check (fxbit-count -1) => 0)


#|
fxlength
fixnum 位长度

语法
----
(fxlength i)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
等价于 integer-length。

性能边界
----
与位宽相关。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxlength 0) => 0)
(check (fxlength 1) => 1)
(check (fxlength -1) => 1)
(check (fxlength fx-greatest) => (- fx-width 1))


#|
fxif
fixnum 按位条件选择

语法
----
(fxif mask i j)

参数
----
mask : fixnum?
i, j : fixnum?

返回值
----
fixnum?

边界行为
----
等价于 (fxior (fxand mask i) (fxand (fxnot mask) j))。

性能边界
----
常量时间。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxif 1 1 2) => 3)
(check (fxif 0 1 2) => 2)
(check (fxif -1 1 2) => 1)


#|
fxbit-set?
判断指定 bit 是否为 1

语法
----
(fxbit-set? index i)

参数
----
index : fixnum?
i : fixnum?

返回值
----
boolean?

边界行为
----
index 需满足 0 <= index < fx-width。

性能边界
----
常量时间。

错误处理
----
越界触发 out-of-range。
|#

(check-true (fxbit-set? 0 1))
(check-false (fxbit-set? 1 1))
(check-true (fxbit-set? 63 -1))
(check-catch 'out-of-range (fxbit-set? -1 0))
(check-catch 'type-error (fxbit-set? 1.0 0))
(check-false (fxbit-set? 0 fx-least))
(check-true (fxbit-set? (- fx-width 1) fx-least))
(check-catch 'out-of-range (fxbit-set? fx-width 0))


#|
fxcopy-bit
设置指定 bit

语法
----
(fxcopy-bit index i boolean)

参数
----
index : fixnum?
i : fixnum?
boolean : boolean?

返回值
----
fixnum?

边界行为
----
index 需满足 0 <= index < fx-width。

性能边界
----
常量时间。

错误处理
----
越界触发 out-of-range。
|#

(check (fxcopy-bit 1 0 #t) => 2)
(check (fxcopy-bit 1 2 #f) => 0)
(check (fxcopy-bit 63 0 #t) => fx-least)
(check-catch 'out-of-range (fxcopy-bit -1 0 #t))
(check-catch 'type-error (fxcopy-bit 0 0 1))
(check (fxcopy-bit (- fx-width 1) -1 #f) => fx-greatest)
(check-catch 'out-of-range (fxcopy-bit fx-width 0 #t))


#|
fxfirst-set-bit
返回最低位 1 的位置

语法
----
(fxfirst-set-bit i)

参数
----
i : fixnum?

返回值
----
fixnum?

边界行为
----
若 i 为 0，返回 -1。

性能边界
----
与位宽相关。

错误处理
----
非 fixnum 触发类型错误。
|#

(check (fxfirst-set-bit 0) => -1)
(check (fxfirst-set-bit 18) => 1)


#|
fxbit-field
提取位域

语法
----
(fxbit-field i start end)

参数
----
i : fixnum?
start, end : fixnum?

返回值
----
fixnum?

边界行为
----
start 与 end 满足 0 <= start <= end <= fx-width。

性能边界
----
与位宽相关。

错误处理
----
越界触发 out-of-range。
|#

(check (fxbit-field #b110110 1 4) => 3)
(check (fxbit-field #b110110 4 4) => 0)
(check-catch 'out-of-range (fxbit-field 1 0 (+ fx-width 1)))
(check-catch 'out-of-range (fxbit-field 1 -1 1))
(check (fxbit-field fx-greatest 0 fx-width) => fx-greatest)
(check-catch 'out-of-range (fxbit-field 1 5 4))


#|
fxbit-field-rotate
位域旋转

语法
----
(fxbit-field-rotate i count start end)

参数
----
i : fixnum?
count : fixnum?
start, end : fixnum?

返回值
----
fixnum?

边界行为
----
对 [start, end) 位域旋转。

性能边界
----
与位宽相关。

错误处理
----
越界触发 out-of-range。
|#

(check (fxbit-field-rotate #b110110 1 1 5) => 46)
(check (fxbit-field-rotate #b110110 -1 1 5) => 58)
(check (fxbit-field-rotate #b110110 0 1 5) => #b110110)
(check (fxbit-field-rotate #b110110 4 1 5) => #b110110)


#|
fxbit-field-reverse
位域反转

语法
----
(fxbit-field-reverse i start end)

参数
----
i : fixnum?
start, end : fixnum?

返回值
----
fixnum?

边界行为
----
对 [start, end) 位域反转。

性能边界
----
与位宽相关。

错误处理
----
越界触发 out-of-range。
|#

(check (fxbit-field-reverse #b110110 1 5) => 58)
(check-catch 'out-of-range (fxbit-field-reverse 1 5 4))
(check (fxbit-field-reverse #b110110 2 2) => #b110110)
(check (fxbit-field-reverse #b110110 2 3) => #b110110)


(check-report)
(if (check-failed?) (exit -1))

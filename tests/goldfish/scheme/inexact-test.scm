;
; Copyright (C) 2024 The Goldfish Scheme Authors
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

(import (liii check)
        (scheme inexact)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

#|
nan?
判断一个数值是否为 NaN（Not a Number）。

语法
----
(nan? obj)

参数
----
obj : any
任意类型的对象。

返回值
-----
boolean?
- 若 obj 是数值，且其实部或虚部中存在 NaN，返回 #t。
- 否则返回 #f。

说明
----
1. NaN 表示"非数字"值，通常由无效的数学运算产生
2. 支持检测各种数值类型中的 NaN
3. 非数值类型将返回 #f
4. 复数中只要实部或虚部任一为 NaN，就返回 #t

错误
----
无错误情况，非数值将返回 #f。
|#

;; nan? 基本测试
(check (nan? +nan.0) => #t)
(check (nan? -nan.0) => #t)
(check (nan? +nan.0+5.0i) => #t)
(check (nan? 5.0+nan.0i) => #t)
(check (nan? +nan.0+5i) => #t)
(check (nan? 5+nan.0i) => #t)
(check (nan? +nan.0+2/5i) => #t)
(check (nan? 2/5+nan.0i) => #t)

;; nan? 非 NaN 数值测试
(check (nan? 32) => #f)
(check (nan? 3.14) => #f)
(check (nan? 1+2i) => #f)
(check (nan? +inf.0) => #f)
(check (nan? -inf.0) => #f)
(check (nan? 0) => #f)
(check (nan? 0.0) => #f)
(check (nan? 1/2) => #f)
(check (nan? 1/2+i) => #f)
(check (nan? 1+1/2i) => #f)
(check (nan? 1.0+2.0i) => #f)

;; nan? 运算产生的 NaN 测试
(check (nan? (* +nan.0 2.0)) => #t)
(check (nan? (* 0.0 +nan.0)) => #t)
(check (nan? (+ +nan.0 1)) => #t)
(check (nan? (- +nan.0 0.5)) => #t)
(check (nan? (sqrt -1.0)) => #f)  ; sqrt(-1) = 0+1i，不是 NaN

;; nan? 非数值类型测试
(check (nan? #t) => #f)
(check (nan? #f) => #f)
(check (nan? "hello") => #f)
(check (nan? 'symbol) => #f)
(check (nan? '(+nan.0)) => #f)
(check (nan? '#(+nan.0)) => #f)
(check (nan? '()) => #f)
(check (nan? '(1 2 3)) => #f)
(check (nan? #\a) => #f)

#|
sqrt
计算给定数值的平方根。

语法
----
(sqrt z)

参数
----
z : number?
被开方数，可以是整数、有理数、浮点数或复数。

返回值
------
number?
返回z的平方根。当z为负数时，返回复数值。

说明
----
1. 计算平方根函数√z
2. 支持整数、有理数、浮点数、复数等各种数值类型
3. 当z为负数时，返回复数形式的平方根（如√-1 = 0+1i）
4. 返回值精确度与输入值类型保持一致：
   - 如果输入为精确值且结果可表示为精确值，则返回精确值
   - 如果输入为不精确值，则返回不精确值
   - 如果输入为负数，由于结果为复数，总是返回不精确值

示例
----
(sqrt 9) => 3
(sqrt 25.0) => 5.0
(sqrt 2) => 1.4142135623730951 (近似值)
(sqrt -1) => 0.0+1.0i
(sqrt -4) => 0.0+2.0i
(sqrt 0) => 0
(sqrt 0.0) => 0.0
(sqrt 1/4) => 1/2

错误处理
--------
wrong-type-arg
当参数不是数值时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; sqrt 基本测试
(check (sqrt 9) => 3)              
(check (sqrt 25.0) => 5.0)
(check (sqrt 9/4) => 3/2) 
(check (< (abs (- (sqrt 2.0) 1.4142135623730951)) 1e-10) => #t)
       
;; sqrt 负数测试
(check (sqrt -1.0) => 0.0+1.0i)
(check (sqrt -1) => 0.0+1.0i)
(check (sqrt -4.0) => 0.0+2.0i)
(check (sqrt -4) => 0.0+2.0i)
(check (sqrt -2.25) => 0.0+1.5i)
       
;; sqrt 边界测试
(check (sqrt 0) => 0)
(check (sqrt 0.0) => 0.0)
(check (sqrt 1) => 1)
(check (sqrt 1.0) => 1.0)
       
;; sqrt 精度测试  
(check (exact? (sqrt 4)) => #t)
(check (exact? (sqrt 4.0)) => #f) 
(check (exact? (sqrt -1)) => #f) 
(check (exact? (sqrt -1.0)) => #f)

;; sqrt 大型数值测试
(check (sqrt 10000) => 100)
(check (sqrt 1000000.0) => 1000.0)

;; 错误处理测试
(check-catch 'wrong-type-arg  (sqrt "hello"))
(check-catch 'wrong-type-arg  (sqrt 'symbol))
(check-catch 'wrong-number-of-args (sqrt))
(check-catch 'wrong-number-of-args (sqrt 1 2))

#|
infinite?

判断一个数值是否无限。

语法
----
(infinite? obj)

参数
----
obj : number?
要判断的数值。支持整数、浮点数、有理数、复数。

返回值
-----
boolean?
- 若 obj 是数值，且其实部或虚部中存在 +inf.0 或 -inf.0，返回 #t。
- 否则返回 #f。

错误
----
无错误情况，非数值将返回 #f。
|#

(check (infinite? 0) => #f)
(check (infinite? 0.0) => #f)
(check (infinite? 1/2) => #f)
(check (infinite? 1/2+i) => #f)
(check (infinite? 1+1/2i) => #f)
(check (infinite? 1+2i) => #f)
(check (infinite? 1.0+2.0i) => #f)
(check (infinite? +inf.0) => #t)
(check (infinite? -inf.0) => #t)
(check (infinite? +inf.0+2.0i) => #t)
(check (infinite? +inf.0+2i) => #t)
(check (infinite? +inf.0+1/2i) => #t)
(check (infinite? 2.0-inf.0i) => #t)
(check (infinite? 2-inf.0i) => #t)
(check (infinite? 1/2-inf.0i) => #t)
(check (infinite? +inf.0-inf.0i) => #t)
(check (infinite? -inf.0+inf.0i) => #t)
(check (infinite? +nan.0) => #f)
(check (infinite? -nan.0) => #f)
(check (infinite? (* +nan.0 2.0)) => #f)
(check (infinite? (* 0.0 +nan.0)) => #f)
(check (infinite? +nan.0+5.0i) => #f)
(check (infinite? 5.0+nan.0i) => #f)
(check (infinite? +nan.0+5i) => #f)
(check (infinite? 5+nan.0i) => #f)
(check (infinite? +nan.0+2/5i) => #f)
(check (infinite? 2/5+nan.0i) => #f)
(check (infinite? #t) => #f)
(check (infinite? "hello") => #f)
(check (infinite? 'symbol) => #f)
(check (infinite? '(+inf.0)) => #f)
(check (infinite? '#(+inf.0)) => #f)

#|
finite?

判断一个数值是否有限。

语法
----
(finite? obj)

参数
----
obj : any
任意类型的对象。

返回值
-----
boolean?
- 若 obj 是数值，且实部与虚部都为有限数，返回 #t。
- 若是非数值、包含 inf.0、nan.0 的实部或虚部，返回 #f。

错误
----
无错误情况，非数值将返回 #f。
|#

(check (finite? 0) => #t)
(check (finite? 0.0) => #t)
(check (finite? 1/2) => #t)
(check (finite? 1/2+i) => #t)
(check (finite? 1+1/2i) => #t)
(check (finite? 1+2i) => #t)
(check (finite? 1.0+2.0i) => #t)
(check (finite? +inf.0) => #f)
(check (finite? -inf.0) => #f)
(check (finite? +inf.0+2.0i) => #f)
(check (finite? +inf.0+2i) => #f)
(check (finite? +inf.0+1/2i) => #f)
(check (finite? 2.0-inf.0i) => #f)
(check (finite? 2-inf.0i) => #f)
(check (finite? 1/2-inf.0i) => #f)
(check (finite? +inf.0-inf.0i) => #f)
(check (finite? -inf.0+inf.0i) => #f)
(check (finite? +nan.0) => #f)
(check (finite? -nan.0) => #f)
(check (finite? (* +nan.0 2.0)) => #f)
(check (finite? (* 0.0 +nan.0)) => #f)
(check (finite? +nan.0+5.0i) => #f)
(check (finite? 5.0+nan.0i) => #f)
(check (finite? +nan.0+5i) => #f)
(check (finite? 5+nan.0i) => #f)
(check (finite? +nan.0+2/5i) => #f)
(check (finite? 2/5+nan.0i) => #f)
(check (finite? #t) => #f)
(check (finite? "hello") => #f)
(check (finite? 'symbol) => #f)
(check (finite? '(+inf.0)) => #f)
(check (finite? '#(+inf.0)) => #f)

#|
exp
计算指数函数 e^n。

语法
----
(exp n)

参数
----
n : number?
可选的数值参数，指数值。

返回值
------
number?
e的n次幂值。

说明
----
1. 计算自然指数函数
2. e ≈ 2.718281828459045
3. 支持整数、有理数、浮点数、复数等各种数值类型

错误处理
--------
wrong-type-arg
当参数不是数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; exp 基本测试
(check (exp 0) => 1)
(check (exp 1) => 2.718281828459045)
(check (exp -1) => 0.36787944117144233)
(check (exp 2) => 7.38905609893065)

;; exp 边界测试
(check (exp 10) => 22026.465794806718)
(check (exp -10) => 4.5399929762484854e-05)
(check (exp 0.5) => 1.6487212707001282)
(check (exp -0.5) => 0.6065306597126334)

;; 错误处理测试
(when (not (os-windows?))
  (check (exp 1+2i) => -1.1312043837568135+2.4717266720048188i)
) ;when

(check-catch 'wrong-type-arg (exp "hello"))
(check-catch 'wrong-number-of-args (exp))

#|
log
计算对数函数。单个参数时计算自然对数(log base e)，两个参数时计算以第二个参数为底的对数。

语法
----
(log z [base])

参数
----
z : number?
必须为数，计算对数值

base : number? 可选
必须为数，表示对数底

返回值
------
number?
对应的对数值

说明
----
1. 单个参数：计算自然对数ln(z) = log_e(z)
2. 两个参数：计算log_base(z) = log(z)/log(base)
3. 支持各种数值类型
4. 注意参数必须为正数且不等于1

错误处理
--------
out-of-range
当z <= 0或base <= 0时抛出错误。
wrong-type-arg
当参数类型错误时抛出错误。
wrong-number-of-args
当参数数量不为1或2个时抛出错误。
|#

;; log 基本自然对数测试
(check (log 1) => 0.0)
(check (log (exp 1)) => 1.0)
(check (log 2) => 0.6931471805599453)

;; log 双参数对数测试
(check (log 100 10) => 2)
(check (log 8 2) => 3)
(check (log 16 2) => 4)

;; log 通用对数测试
(check (log 10 10) => 1)
(check (log 100 10) => 2)
(check (log 1 10) => 0)

;; log 有理数测试
(check (log 2 4) => 1/2)
(check (log 1/2 2) => -1.0)
(check (log 9 3) => 2)

;; log 浮点数对数测试
(check (log 2.718281828459045) => 1.0)
(check (log 0.1 10) =>  -0.9999999999999998) ;返回值是个不精确数

;; 相互验证测试
(check (log (exp 3)) => 3.0)
(check (exp (log 5)) => 4.999999999999999)

;; 错误处理测试
(check (log 0) => -inf.0+3.141592653589793i) ; log(0) = -∞ + πi
(check (log -1) => 0+3.141592653589793i) ; log(-1) = πi
(check (log 3 1) => +inf.0) ; log(3, 1) = 0
(check-catch 'out-of-range (log 10 0))
(check-catch 'wrong-type-arg (log "a"))
(check-catch 'wrong-number-of-args (log))
(check-catch 'wrong-number-of-args (log 12 4 5))

#|
sin
计算给定角度的正弦值。

语法
----
(sin radians)

参数
----
radians : number?
以弧度为单位的角度值。

返回值
------
real?
返回弧度角度的正弦值，当输入值为实数时值域为[-1, 1]。

说明
----
1. 计算正弦函数sin(x)
2. 角度必须以弧度为单位
3. 支持整数、有理数、浮点数、复数等各种数值类型
4. 返回值精确度与输入值类型保持一致

错误处理
--------
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; sin 基本测试
(check (sin 0) => 0)
(check (sin (/ pi 2)) => 1.0)
(check (sin pi) => 1.2246467991473532e-16)
(check (sin (* 2 pi)) => -2.4492935982947064e-16)
(check-float (sin (/ pi 4)) 0.7071067811865475)

;; 特殊角度测试
(check (sin (/ pi 6)) => 0.49999999999999994)
(check (sin (* -1 (/ pi 2))) => -1.0)
(check (sin (* 3 (/ pi 2))) => -1.0)

;; 边界测试
(check-float (sin 1000) 0.8268795405320025)
(check (sin 0.001) => 9.999998333333417e-4)
(check (sin -0.001) => -9.999998333333417e-4)

;; 复数测试
(when (not (os-windows?))
  (check (sin 1+2i) => 3.165778513216168+1.9596010414216063i)
) ;when

;; 错误处理测试
(check-catch 'wrong-type-arg (sin "hello"))
(check-catch 'wrong-number-of-args (sin))
(check-catch 'wrong-number-of-args (sin 1 2))

#|
cos
计算给定角度的余弦值。

语法
----
(cos radians)

参数
----
radians : number?
以弧度为单位的角度值。

返回值
------
real?
返回弧度角度的余弦值，当输入值为实数时值域为[-1, 1]。

说明
----
1. 计算余弦函数cos(x)
2. 角度必须以弧度为单位
3. 支持整数、有理数、浮点数、复数等各种数值类型
4. 返回值精确度与输入值类型保持一致


错误处理
--------
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; cos 基本测试
(check (cos 0) => 1)
(check (cos (/ pi 2)) => 6.123233995736766e-17)
(check (cos pi) => -1.0)
(check (cos (* 2 pi)) => 1.0)
(check (cos (/ pi 4)) => 0.7071067811865476)

;; 特殊角度测试
(check (cos (/ pi 3)) => 0.5000000000000001)
(check (cos (/ pi 6)) => 0.8660254037844387)
(check (cos (* -1 (/ pi 3))) => 0.5000000000000001)

;; 边界测试
(check (cos 100) => 0.8623188722876839)


;; 有理数测试
(check (cos 3/4) => 0.7316888688738209)

;； 复数测试
(when (not (os-windows?))
  (check (cos 1+2i) => 2.0327230070196656-3.0518977991518i)
) ;when

;; 错误处理测试
(check-catch 'wrong-type-arg (cos "hello"))
(check-catch 'wrong-number-of-args (cos))
(check-catch 'wrong-number-of-args (cos 1 2))

#|
tan
计算给定角度的正切值。

语法
----
(tan radians)

参数
----
radians : number?
以弧度为单位的角度值。

返回值
------
real?
返回弧度角度的正切值。

说明
----
1. 计算正切函数tan(x) = sin(x)/cos(x)
2. 角度必须以弧度为单位
3. 支持整数、有理数、浮点、复数数等各种数值类型
4. 需要注意tan在π/2 + kπ处的奇点（无定义）


错误处理
--------
wrong-type-arg
当参数不是实数时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; tan 基本测试
(check (tan 0) => 0)

;; 特殊角度测试
(check (tan (/ pi 3)) => 1.7320508075688767)


;; 有理数测试
(check (tan 1/2) => 0.5463024898437905)

;; 错误处理测试
(check-catch 'wrong-type-arg (tan "hello"))
(check-catch 'wrong-number-of-args (tan))
(check-catch 'wrong-number-of-args (tan 1 2))

#|
asin
计算给定值的反正弦值。

语法
----
(asin x)

参数
----
x : real?
必须在区间[-1, 1]内的实数，表示sin函数的值。

返回值
------
real?
返回x的反正弦值（arcsin），范围在[-π/2, π/2]内。

说明
----
1. 计算反正弦函数arcsin(x)
2. 支持整数、有理数、浮点数等各种数值类型
3. 当|x| > 1时，返回复数值
4. 返回值精确度与输入值类型保持一致

示例
----
(asin 0) => 0.0
(asin 1) => 1.5707963267948966 (π/2)
(asin -1) => -1.5707963267948966 (-π/2)

错误处理
--------
wrong-type-arg
当参数不是实数或超出范围时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; asin 基本测试
(check (asin 0) => 0)
(check (asin 1) => 1.5707963267948966)
(check (asin -1) => -1.5707963267948966)

;; 特殊值测试
(check (asin (/ (sqrt 2) 2)) => 0.7853981633974484)
(check (asin (/ (sqrt 3) 2)) => 1.0471975511965976)

;; 边界测试
(check (asin 0.000001) => 1.0000000000001666e-6)

;; 有理数测试
(check (asin 2/3) => 0.7297276562269664)


;; 错误处理测试
(check-catch 'wrong-type-arg (asin "hello"))
(check-catch 'wrong-number-of-args (asin))
(check-catch 'wrong-number-of-args (asin 1 2))

#|
acos
计算给定值的反余弦值。

语法
----
(acos x)

参数
----
x : real?
必须在区间[-1, 1]内的实数，表示cos函数的值。

返回值
------
real?
返回x的反余弦值（arccos），范围在[0, π]内。

说明
----
1. 计算反余弦函数arccos(x)
2. 支持整数、有理数、浮点数等各种数值类型
3. 当|x| > 1时，返回复数值
4. 返回值精确度与输入值类型保持一致

示例
----
(acos 0) => 1.5707963267948966 (π/2)
(acos 1) => 0.0
(acos -1) => 3.141592653589793 (π)

错误处理
--------
wrong-type-arg
当参数不是实数或超出范围时抛出错误。
wrong-number-of-args
当参数数量不为1时抛出错误。
|#

;; acos 基本测试
(check (acos 0) => 1.5707963267948966)
(check (acos 1) => 0)
(check (acos -1) => 3.141592653589793)
(check (acos -0.5) => 2.0943951023931957)

;; 特殊值测试
(check (acos (/ (sqrt 2) 2)) => 0.7853981633974483)
(check (acos (/ (sqrt 3) 2)) => 0.5235987755982989)

;; 边界测试
(check (acos 0.999999) => 0.0014142136802445852)
(check (acos 0.000001) => 1.5707953267948966)
(check (acos -0.999999) => 3.1401784399095485)

;; 有理数测试
(check (acos 3/4) => 0.7227342478134157)
(check (acos 2/3) => 0.8410686705679303)

;; 错误处理测试
(check-catch 'wrong-type-arg (acos "hello"))
(check-catch 'wrong-number-of-args (acos))
(check-catch 'wrong-number-of-args (acos 1 2))

#|
atan
计算给定值的反正切值，或计算两个值之比值的反正切值。

语法
----
(atan x [y])

参数
----
x : number?
当y未提供时，必须为实数，表示tan函数的值
当y提供时，必须为实数，表示纵坐标

y : real? 可选
表示横坐标的实数

返回值
------
real?
当只有x参数时，返回x的反正切值，范围在(-π/2, π/2)内
当提供x和y参数时，返回y/x的反正切值，范围在(-π, π]内

说明
----
1. 计算反正切函数arctan(x)或arctan(y/x)
2. 角度以弧度为单位返回
3. 双参数形式可以处理所有象限的角度
4. 支持各种数值类型

错误处理
--------
wrong-type-arg
当参数类型错误时抛出错误。
wrong-number-of-args
当参数数量不为1或2个时抛出错误。
|#

;; atan 基本单参数测试
(check (atan 0) => 0)
(check (atan 1) => 0.7853981633974483)
(check (atan -1) => -0.7853981633974483)

;; atan 双参数测试
(check (atan 1 1) => 0.7853981633974483)
(check (atan -1 1) => -0.7853981633974483)
(check (atan 1 -1) => 2.356194490192345)
(check (atan -1 -1) => -2.356194490192345)
(check (atan 0 1) => 0.0)
(check (atan 1 0) => 1.5707963267948966)
(check (atan -1 0) => -1.5707963267948966)

;; 特殊角度测试
(check (atan (/ 1 (sqrt 3))) => 0.5235987755982989)
(check (atan 2 3) => 0.5880026035475675)
(check (atan 3 2) => 0.982793723247329)

;; 有理数测试(atan)
(check (atan 2/3) => 0.5880026035475675)
(check (atan 3/4) => 0.6435011087932844)
(check (atan 4 3) => 0.9272952180016122)

;; 边界测试
(check (atan 1000) => 1.5697963271282298)
(check (atan 0.000001) => 9.999999999996666e-7)
(check (atan -0.000001) => -9.999999999996666e-7)

;; 复数测试
(when (not (os-windows?))
  (check (atan 1+2i) => 1.3389725222944935+0.40235947810852507i)
) ;when

;; 错误处理测试
(check-catch 'wrong-type-arg (atan "hello"))
(check-catch 'wrong-number-of-args (atan))
(check-catch 'wrong-number-of-args (atan 1 2 3))
(check-catch 'wrong-type-arg (atan 1 "hello"))

(check-report)

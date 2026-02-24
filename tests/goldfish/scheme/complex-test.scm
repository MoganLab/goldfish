;
; Copyright (C) 2025 The Goldfish Scheme Authors
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

;;; Test cases for (scheme complex) library - real-part function

(import (liii check)
        (scheme complex))

(check-set-mode! 'report-failed)

#|
real-part
返回复数的实部

函数签名
----
(real-part z) → real

参数
----
z : number
复数或实数

返回值
----
real
复数 z 的实部

描述
----
`real-part` 用于返回复数或实数的实部。对于实数，返回该实数本身；
对于复数，返回其实部。

行为特征
------
- 对于实数，返回该实数本身
- 对于复数，返回其实部
- 支持精确数和近似数
- 遵循 R7RS 标准规范

数学定义
------
如果 z = x + yi，其中 x 和 y 是实数，i 是虚数单位，则：
real-part(z) = x

特殊情况
------
- (real-part 5) → 5
- (real-part 3.14) → 3.14
- (real-part (make-rectangular 3 4)) → 3
- (real-part (make-rectangular -3 4)) → -3

错误处理
------
- 参数必须是数值类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme complex) 库中提供
- 底层由 S7 scheme 引擎实现

相关函数
--------
- `imag-part` : 返回复数的虚部
- `make-rectangular` : 根据实部和虚部构造复数
- `magnitude` : 返回复数的模
- `angle` : 返回复数的辐角
|#

;; Test real-part with complex numbers
(check (real-part (make-rectangular 3 4)) => 3.0)
(check (real-part (make-rectangular -3 4)) => -3.0)
(check (real-part (make-rectangular 3 -4)) => 3.0)
(check (real-part (make-rectangular -3 -4)) => -3.0)

;; Test real-part with real numbers
(check (real-part 5) => 5)
(check (real-part -5) => -5)
(check (real-part 0) => 0)

;; Test real-part with floating point numbers
(check (real-part 3.14) => 3.14)
(check (real-part -2.71) => -2.71)

;; Test real-part with complex number literals
(check (real-part 1+2i) => 1.0)
(check (real-part 3-4i) => 3.0)
(check (real-part -5+6i) => -5.0)
(check (real-part -7-8i) => -7.0)
(check (real-part 0+9i) => 0.0)
(check (real-part 10+0i) => 10.0)

#|
imag-part
返回复数的虚部

函数签名
----
(imag-part z) → real

参数
----
z : number
复数或实数

返回值
----
real
复数 z 的虚部

描述
----
`imag-part` 用于返回复数或实数的虚部。对于实数，返回 0；
对于复数，返回其虚部。

行为特征
------
- 对于实数，返回 0
- 对于复数，返回其虚部
- 支持精确数和近似数
- 遵循 R7RS 标准规范

数学定义
------
如果 z = x + yi，其中 x 和 y 是实数，i 是虚数单位，则：
imag-part(z) = y

特殊情况
------
- (imag-part 5) → 0
- (imag-part 3.14) → 0.0
- (imag-part (make-rectangular 3 4)) → 4
- (imag-part (make-rectangular -3 4)) → 4

错误处理
------
- 参数必须是数值类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme complex) 库中提供
- imag-part 是内置函数，由 S7 scheme 引擎实现
- 不需要额外的实现代码

相关函数
--------
- `real-part` : 返回复数的实部
- `make-rectangular` : 根据实部和虚部构造复数
- `magnitude` : 返回复数的模
- `angle` : 返回复数的辐角
|#

;; Test imag-part with complex numbers
(check (imag-part (make-rectangular 3 4)) => 4.0)
(check (imag-part (make-rectangular -3 4)) => 4.0)
(check (imag-part (make-rectangular 3 -4)) => -4.0)
(check (imag-part (make-rectangular -3 -4)) => -4.0)

;; Test imag-part with real numbers
(check (imag-part 5) => 0)
(check (imag-part -5) => 0)
(check (imag-part 0) => 0)

;; Test imag-part with floating point numbers
(check (imag-part 3.14) => 0.0)
(check (imag-part -2.71) => 0.0)

;; Test imag-part with complex number literals
(check (imag-part 1+2i) => 2.0)
(check (imag-part 3-4i) => -4.0)
(check (imag-part -5+6i) => 6.0)
(check (imag-part -7-8i) => -8.0)
(check (imag-part 0+9i) => 9.0)
(check (imag-part 10+0i) => 0.0)

;; Test complex constructor
(check (complex 3 0) => 3)
(check (real-part (complex 3 4)) => 3.0)
(check (imag-part (complex 3 4)) => 4.0)
(check (complex 2.5 0.0) => 2.5)

;; Test magnitude
(check (magnitude 3+4i) => 5.0)
(check (magnitude -3) => 3)
(check (magnitude -3.5) => 3.5)
(check (magnitude 0) => 0)

;; Test angle
(check (angle 1) => 0)
(check (angle -1) => 3.141592653589793)
(check (> (angle 1+1i) 0.78) => #t)
(check (< (angle 1+1i) 0.79) => #t)

;; Test make-polar
(check (real-part (make-polar 2 0)) => 2.0)
(check (imag-part (make-polar 2 0)) => 0.0)
(check (> (real-part (make-polar 1 1.5707963267948966)) -0.001) => #t)
(check (< (real-part (make-polar 1 1.5707963267948966)) 0.001) => #t)
(check (> (imag-part (make-polar 1 1.5707963267948966)) 0.999) => #t)

;; Error handling
(check-catch 'wrong-type-arg (complex "a" 1))
(check-catch 'wrong-type-arg (complex 1 "b"))
(check-catch 'wrong-type-arg (magnitude "x"))
(check-catch 'wrong-type-arg (angle "x"))
(check-catch 'wrong-type-arg (make-polar "x" 1))
(check-catch 'wrong-type-arg (make-polar 1 "x"))
(check-catch 'wrong-number-of-args (complex 1))
(check-catch 'wrong-number-of-args (make-polar 1))

(check-report)

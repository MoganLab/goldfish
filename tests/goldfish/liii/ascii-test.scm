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

(import (liii check)
        (srfi srfi-175))

(check-set-mode! 'report-failed)


#|
ascii-codepoint? 
判断对象是否为 ASCII 码点

语法
----
(ascii-codepoint? x)

参数
----
x : any

返回值
----
boolean?

边界行为
----
支持 0 与 #x7f 边界，以及 -1/#x80 等越界值。

性能边界
----
单次判断为常量时间。

错误处理
----
非整数输入返回 #f。
|#

(check-true (ascii-codepoint? 0))
(check-true (ascii-codepoint? #x7f))
(check-false (ascii-codepoint? -1))
(check-false (ascii-codepoint? #x80))
(check-false (ascii-codepoint? #\A))

#|
ascii-char? 
判断对象是否为 ASCII 字符

语法
----
(ascii-char? x)

参数
----
x : any

返回值
----
boolean?

边界行为
----
支持 ASCII 字符与非 ASCII 字符边界。

性能边界
----
单次判断为常量时间。

错误处理
----
非字符输入返回 #f。
|#

(check-true (ascii-char? #\A))
(check-true (ascii-char? #\newline))
(check-false (ascii-char? #\x80))
(check-false (ascii-char? 65))

#|
ascii-bytevector? 
判断字节向量是否全部为 ASCII

语法
----
(ascii-bytevector? x)

参数
----
x : any

返回值
----
boolean?

边界行为
----
覆盖空字节向量与包含非 ASCII 字节的场景。

性能边界
----
按字节向量长度线性检查。

错误处理
----
非字节向量输入返回 #f。
|#

(check-true (ascii-bytevector? #u8()))
(check-true (ascii-bytevector? #u8(0 65 127)))
(check-false (ascii-bytevector? #u8(0 128)))
(check-false (ascii-bytevector? '(65 66)))

#|
ascii-string? 
判断字符串是否全部为 ASCII

语法
----
(ascii-string? x)

参数
----
x : any

返回值
----
boolean?

边界行为
----
覆盖空字符串、ASCII 字符串与包含非 ASCII 字符的场景。

性能边界
----
按字符串长度线性检查。

错误处理
----
非字符串输入返回 #f。
|#

(check-true (ascii-string? "Goldfish"))
(check-true (ascii-string? "A\tB\nC"))
(check-false (ascii-string? "G中"))
(check-false (ascii-string? #\A))

#|
ascii-control? 
判断是否为 ASCII 控制字符

语法
----
(ascii-control? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖 #x1f/#x20/#x7f 控制区间边界。

性能边界
----
单次判断为常量时间。

错误处理
----
类型或范围不匹配时返回 #f。
|#

(check-true (ascii-control? 0))
(check-true (ascii-control? #x1f))
(check-true (ascii-control? #x7f))
(check-false (ascii-control? #x20))

#|
ascii-non-control? 
判断是否为 ASCII 非控制字符

语法
----
(ascii-non-control? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖 #x1f/#x20/#x7e/#x7f 边界。

性能边界
----
单次判断为常量时间。

错误处理
----
类型或范围不匹配时返回 #f。
|#

(check-true (ascii-non-control? #x20))
(check-true (ascii-non-control? #x7e))
(check-false (ascii-non-control? #x1f))
(check-false (ascii-non-control? #x7f))

#|
ascii-whitespace? 
判断是否为 ASCII 空白字符

语法
----
(ascii-whitespace? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖 tab/newline/space 与普通字母场景。

性能边界
----
单次判断为常量时间。

错误处理
----
类型或范围不匹配时返回 #f。
|#

(check-true (ascii-whitespace? #\tab))
(check-true (ascii-whitespace? #\newline))
(check-true (ascii-whitespace? #\space))
(check-false (ascii-whitespace? #\A))

#|
ascii-space-or-tab? 
判断是否为空格或制表符

语法
----
(ascii-space-or-tab? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
仅识别空格与制表符，不包含换行符。

性能边界
----
单次判断为常量时间。

错误处理
----
类型或范围不匹配时返回 #f。
|#

(check-true (ascii-space-or-tab? #\space))
(check-true (ascii-space-or-tab? #\tab))
(check-false (ascii-space-or-tab? #\newline))

#|
ascii-other-graphic? 
判断是否为可见非字母数字字符

语法
----
(ascii-other-graphic? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖标点与字母数字分界。

性能边界
----
单次判断为常量时间。

错误处理
----
类型或范围不匹配时返回 #f。
|#

(check-true (ascii-other-graphic? #\!))
(check-true (ascii-other-graphic? #\{))
(check-false (ascii-other-graphic? #\A))
(check-false (ascii-other-graphic? #\0))

#|
ascii-upper-case? 
判断是否为 ASCII 大写字母

语法
----
(ascii-upper-case? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖大写字母边界与小写字母对照。

性能边界
----
单次判断为常量时间。

错误处理
----
类型或范围不匹配时返回 #f。
|#

(check-true (ascii-upper-case? #\A))
(check-false (ascii-upper-case? #\a))

#|
ascii-lower-case? 
判断是否为 ASCII 小写字母

语法
----
(ascii-lower-case? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖小写字母边界与大写字母对照。

性能边界
----
单次判断为常量时间。

错误处理
----
类型或范围不匹配时返回 #f。
|#

(check-true (ascii-lower-case? #\z))
(check-false (ascii-lower-case? #\Z))

#|
ascii-alphabetic? 
判断是否为 ASCII 字母

语法
----
(ascii-alphabetic? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖字母与数字分界。

性能边界
----
单次判断为常量时间。

错误处理
----
类型或范围不匹配时返回 #f。
|#

(check-true (ascii-alphabetic? #\A))
(check-true (ascii-alphabetic? #\z))
(check-false (ascii-alphabetic? #\0))

#|
ascii-alphanumeric? 
判断是否为 ASCII 字母或数字

语法
----
(ascii-alphanumeric? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖字母数字与符号分界。

性能边界
----
单次判断为常量时间。

错误处理
----
类型或范围不匹配时返回 #f。
|#

(check-true (ascii-alphanumeric? #\0))
(check-true (ascii-alphanumeric? #\G))
(check-false (ascii-alphanumeric? #\-))

#|
ascii-numeric? 
判断是否为 ASCII 数字

语法
----
(ascii-numeric? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖数字字符边界与字母对照。

性能边界
----
单次判断为常量时间。

错误处理
----
类型或范围不匹配时返回 #f。
|#

(check-true (ascii-numeric? #\0))
(check-true (ascii-numeric? #\9))
(check-false (ascii-numeric? #\a))

#|
ascii-digit-value 
将数字字符映射为数值

语法
----
(ascii-digit-value x limit)

参数
----
x : char? | integer?
limit : integer

返回值
----
integer? | #f

边界行为
----
覆盖有效数字边界与超出 limit 的输入。

性能边界
----
单次映射为常量时间。

错误处理
----
非法字符或越界值返回 #f。
|#

(check (ascii-digit-value #\0 10) => 0)
(check (ascii-digit-value #\9 10) => 9)
(check (ascii-digit-value #\9 9) => #f)
(check (ascii-digit-value #\A 10) => #f)

#|
ascii-upper-case-value 
将大写字母映射为数值

语法
----
(ascii-upper-case-value x offset limit)

参数
----
x : char? | integer?
offset : integer
limit : integer

返回值
----
integer? | #f

边界行为
----
覆盖 A-Z 有效边界及超界字符。

性能边界
----
单次映射为常量时间。

错误处理
----
非法字符或越界值返回 #f。
|#

(check (ascii-upper-case-value #\A 10 26) => 10)
(check (ascii-upper-case-value #\F 10 16) => 15)
(check (ascii-upper-case-value #\Q 10 16) => #f)

#|
ascii-lower-case-value 
将小写字母映射为数值

语法
----
(ascii-lower-case-value x offset limit)

参数
----
x : char? | integer?
offset : integer
limit : integer

返回值
----
integer? | #f

边界行为
----
覆盖 a-z 有效边界及超界字符。

性能边界
----
单次映射为常量时间。

错误处理
----
非法字符或越界值返回 #f。
|#

(check (ascii-lower-case-value #\a 10 26) => 10)
(check (ascii-lower-case-value #\f 10 16) => 15)
(check (ascii-lower-case-value #\q 10 16) => #f)

#|
ascii-nth-digit 
将数值映射为数字字符

语法
----
(ascii-nth-digit n)

参数
----
n : integer

返回值
----
char? | #f

边界行为
----
覆盖 0/9 边界与 -1/10 越界值。

性能边界
----
单次映射为常量时间。

错误处理
----
越界序号返回 #f。
|#

(check (ascii-nth-digit 0) => #\0)
(check (ascii-nth-digit 9) => #\9)
(check (ascii-nth-digit -1) => #f)
(check (ascii-nth-digit 10) => #f)

#|
ascii-nth-upper-case 
将数值映射为大写字母

语法
----
(ascii-nth-upper-case n)

参数
----
n : integer

返回值
----
char?

边界行为
----
覆盖 0/25/26 环绕边界。

性能边界
----
单次映射为常量时间。

错误处理
----
按过程定义执行映射。
|#

(check (ascii-nth-upper-case 0) => #\A)
(check (ascii-nth-upper-case 25) => #\Z)
(check (ascii-nth-upper-case 26) => #\A)

#|
ascii-nth-lower-case 
将数值映射为小写字母

语法
----
(ascii-nth-lower-case n)

参数
----
n : integer

返回值
----
char?

边界行为
----
覆盖 0/25/26 环绕边界。

性能边界
----
单次映射为常量时间。

错误处理
----
按过程定义执行映射。
|#

(check (ascii-nth-lower-case 0) => #\a)
(check (ascii-nth-lower-case 25) => #\z)
(check (ascii-nth-lower-case 26) => #\a)


#|
ascii-upcase 
将 ASCII 字母转换为大写

语法
----
(ascii-upcase x)

参数
----
x : char? | integer?

返回值
----
与输入同类型

边界行为
----
覆盖小写转大写、已大写与非字母输入。

性能边界
----
单次转换为常量时间。

错误处理
----
不需要转换时返回原值。
|#

(check (ascii-upcase #\a) => #\A)
(check (ascii-upcase #\A) => #\A)
(check (ascii-upcase #\?) => #\?)
(check (ascii-upcase 97) => 65)

#|
ascii-downcase 
将 ASCII 字母转换为小写

语法
----
(ascii-downcase x)

参数
----
x : char? | integer?

返回值
----
与输入同类型

边界行为
----
覆盖大写转小写、已小写与非字母输入。

性能边界
----
单次转换为常量时间。

错误处理
----
不需要转换时返回原值。
|#

(check (ascii-downcase #\A) => #\a)
(check (ascii-downcase #\a) => #\a)
(check (ascii-downcase #\?) => #\?)
(check (ascii-downcase 65) => 97)

#|
ascii-control->graphic 
将控制字符映射为图形字符

语法
----
(ascii-control->graphic x)

参数
----
x : char? | integer?

返回值
----
与输入同类型或 #f

边界行为
----
覆盖 #x00/#x1f/#x7f 边界和不可转换值。

性能边界
----
单次转换为常量时间。

错误处理
----
不可转换输入返回 #f。
|#

(check (ascii-control->graphic #x00) => #x40)
(check (ascii-control->graphic #x1f) => #x5f)
(check (ascii-control->graphic #x7f) => #x3f)
(check (ascii-control->graphic #\x7f) => #\?)
(check (ascii-control->graphic #x20) => #f)

#|
将图形字符映射为控制字符

语法
----
(ascii-graphic->control x)

参数
----
x : char? | integer?

返回值
----
与输入同类型或 #f

边界行为
----
覆盖 @/_/? 边界和不可转换值。

性能边界
----
单次转换为常量时间。

错误处理
----
不可转换输入返回 #f。
|#

(check (ascii-graphic->control #x40) => #x00)
(check (ascii-graphic->control #x5f) => #x1f)
(check (ascii-graphic->control #x3f) => #x7f)
(check (ascii-graphic->control #\@) => #\nul)
(check (ascii-graphic->control #\A) => #\x01)
(check (ascii-graphic->control #x20) => #f)

#|
ascii-mirror-bracket 
获取括号的镜像字符

语法
----
(ascii-mirror-bracket x)

参数
----
x : char? | integer?

返回值
----
与输入同类型或 #f

边界行为
----
覆盖括号配对边界与非括号输入。

性能边界
----
单次转换为常量时间。

错误处理
----
不可转换输入返回 #f。
|#

(check (ascii-mirror-bracket #\() => #\))
(check (ascii-mirror-bracket #\]) => #\[)
(check (ascii-mirror-bracket #\>) => #\<)
(check (ascii-mirror-bracket #\A) => #f)
(check (ascii-mirror-bracket 40) => 41)

#|
ascii-ci=? 
ASCII 大小写无关相等比较

语法
----
(ascii-ci=? char1 char2)

参数
----
char1, char2 : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖大小写折叠后相等与不等场景。

性能边界
----
单次比较为常量时间。

错误处理
----
参数类型不匹配时按过程约定报错。
|#

(check-true (ascii-ci=? #\a #\A))
(check-false (ascii-ci=? #\a #\b))

#|
ascii-ci<? 
ASCII 大小写无关小于比较

语法
----
(ascii-ci<? char1 char2)

参数
----
char1, char2 : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖大小写折叠后的小于关系。

性能边界
----
单次比较为常量时间。

错误处理
----
参数类型不匹配时按过程约定报错。
|#

(check-true (ascii-ci<? #\a #\B))

#|
ascii-ci>? 
ASCII 大小写无关大于比较

语法
----
(ascii-ci>? char1 char2)

参数
----
char1, char2 : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖大小写折叠后的大于关系。

性能边界
----
单次比较为常量时间。

错误处理
----
参数类型不匹配时按过程约定报错。
|#

(check-true (ascii-ci>? #\Z #\y))

#|
ascii-ci<=? 
ASCII 大小写无关小于等于比较

语法
----
(ascii-ci<=? char1 char2)

参数
----
char1, char2 : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖相等与小于边界。

性能边界
----
单次比较为常量时间。

错误处理
----
参数类型不匹配时按过程约定报错。
|#

(check-true (ascii-ci<=? #\A #\a))

#|
ascii-ci>=? 
ASCII 大小写无关大于等于比较

语法
----
(ascii-ci>=? char1 char2)

参数
----
char1, char2 : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖相等与大于边界。

性能边界
----
单次比较为常量时间。

错误处理
----
参数类型不匹配时按过程约定报错。
|#

(check-true (ascii-ci>=? #\z #\Y))

#|
ascii-string-ci=? 
ASCII 字符串大小写无关相等比较

语法
----
(ascii-string-ci=? string1 string2)

参数
----
string1, string2 : string?

返回值
----
boolean?

边界行为
----
覆盖大小写折叠后相等与不等字符串。

性能边界
----
与最短可判定前缀长度线性相关。

错误处理
----
参数类型不匹配时按过程约定报错。
|#

(check-true (ascii-string-ci=? "GoldFish" "goldfish"))
(check-false (ascii-string-ci=? "goldfish" "gold-fish"))

#|
ascii-string-ci<? 
ASCII 字符串大小写无关小于比较

语法
----
(ascii-string-ci<? string1 string2)

参数
----
string1, string2 : string?

返回值
----
boolean?

边界行为
----
覆盖大小写折叠后字符串小于关系。

性能边界
----
与最短可判定前缀长度线性相关。

错误处理
----
参数类型不匹配时按过程约定报错。
|#

(check-true (ascii-string-ci<? "abc" "ABD"))

#|
ascii-string-ci>? 
ASCII 字符串大小写无关大于比较

语法
----
(ascii-string-ci>? string1 string2)

参数
----
string1, string2 : string?

返回值
----
boolean?

边界行为
----
覆盖大小写折叠后字符串大于关系。

性能边界
----
与最短可判定前缀长度线性相关。

错误处理
----
参数类型不匹配时按过程约定报错。
|#

(check-true (ascii-string-ci>? "ABD" "abc"))

#|
ascii-string-ci<=? 
ASCII 字符串大小写无关小于等于比较

语法
----
(ascii-string-ci<=? string1 string2)

参数
----
string1, string2 : string?

返回值
----
boolean?

边界行为
----
覆盖相等与小于边界。

性能边界
----
与最短可判定前缀长度线性相关。

错误处理
----
参数类型不匹配时按过程约定报错。
|#

(check-true (ascii-string-ci<=? "abc" "ABC"))
(check-true (ascii-string-ci<=? "abc" "abd"))

#|
ascii-string-ci>=? 
ASCII 字符串大小写无关大于等于比较

语法
----
(ascii-string-ci>=? string1 string2)

参数
----
string1, string2 : string?

返回值
----
boolean?

边界行为
----
覆盖相等与大于边界。

性能边界
----
与最短可判定前缀长度线性相关。

错误处理
----
参数类型不匹配时按过程约定报错。
|#

(check-true (ascii-string-ci>=? "ABD" "abc"))
(check-true (ascii-string-ci>=? "abc" "ABC"))

(check-report)

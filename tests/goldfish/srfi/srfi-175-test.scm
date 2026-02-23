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
名称
----
ascii-codepoint? / ascii-char? / ascii-bytevector? / ascii-string?
判断整数、字符、字节向量、字符串是否属于 ASCII。

语法
----
(ascii-codepoint? x)
(ascii-char? x)
(ascii-bytevector? x)
(ascii-string? x)

参数
----
x : any

返回值
----
boolean?

边界行为
----
支持 ASCII 上下边界（0 与 #x7f）和非 ASCII 越界值（-1、#x80）。

性能边界
----
按输入长度线性检查；空字节向量与空字符串应快速返回 #t。

错误处理
----
非预期类型输入返回 #f（例如将字符传给 ascii-codepoint?）。
|#

(check-true (ascii-codepoint? 0))
(check-true (ascii-codepoint? #x7f))
(check-false (ascii-codepoint? -1))
(check-false (ascii-codepoint? #x80))
(check-false (ascii-codepoint? #\A))

;; 字符输入边界测试
(check-true (ascii-char? #\A))
(check-true (ascii-char? #\newline))
(check-false (ascii-char? #\x80))
(check-false (ascii-char? 65))

;; 字节向量边界测试
(check-true (ascii-bytevector? #u8()))
(check-true (ascii-bytevector? #u8(0 65 127)))
(check-false (ascii-bytevector? #u8(0 128)))
(check-false (ascii-bytevector? '(65 66)))

;; 字符串边界测试
(check-true (ascii-string? "Goldfish"))
(check-true (ascii-string? "A\tB\nC"))
(check-false (ascii-string? "G中"))
(check-false (ascii-string? #\A))

#|
名称
----
ascii-control? / ascii-non-control? / ascii-whitespace? / ascii-space-or-tab?
ascii-other-graphic? / ascii-upper-case? / ascii-lower-case?
ascii-alphabetic? / ascii-alphanumeric? / ascii-numeric?
按 ASCII 子类判断字符或码点。

语法
----
(ascii-control? x)
(ascii-non-control? x)
(ascii-whitespace? x)
(ascii-space-or-tab? x)
(ascii-other-graphic? x)
(ascii-upper-case? x)
(ascii-lower-case? x)
(ascii-alphabetic? x)
(ascii-alphanumeric? x)
(ascii-numeric? x)

参数
----
x : char? | integer?

返回值
----
boolean?

边界行为
----
覆盖控制字符边界（#x1f/#x20/#x7f）和字母数字分界。

性能边界
----
单次判断为常量时间，不随输入规模增长。

错误处理
----
输入不在期望类型或范围时返回 #f。
|#

(check-true (ascii-control? 0))
(check-true (ascii-control? #x1f))
(check-true (ascii-control? #x7f))
(check-false (ascii-control? #x20))

;; 非控制字符边界测试
(check-true (ascii-non-control? #x20))
(check-true (ascii-non-control? #x7e))
(check-false (ascii-non-control? #x1f))
(check-false (ascii-non-control? #x7f))

;; 空白字符边界测试
(check-true (ascii-whitespace? #\tab))
(check-true (ascii-whitespace? #\newline))
(check-true (ascii-whitespace? #\space))
(check-false (ascii-whitespace? #\A))

;; 空格与制表符边界测试
(check-true (ascii-space-or-tab? #\space))
(check-true (ascii-space-or-tab? #\tab))
(check-false (ascii-space-or-tab? #\newline))

;; 可见非字母数字字符边界测试
(check-true (ascii-other-graphic? #\!))
(check-true (ascii-other-graphic? #\{))
(check-false (ascii-other-graphic? #\A))
(check-false (ascii-other-graphic? #\0))

;; 大小写字母边界测试
(check-true (ascii-upper-case? #\A))
(check-false (ascii-upper-case? #\a))
(check-true (ascii-lower-case? #\z))
(check-false (ascii-lower-case? #\Z))

;; 字母与数字分类边界测试
(check-true (ascii-alphabetic? #\A))
(check-true (ascii-alphabetic? #\z))
(check-false (ascii-alphabetic? #\0))

(check-true (ascii-alphanumeric? #\0))
(check-true (ascii-alphanumeric? #\G))
(check-false (ascii-alphanumeric? #\-))

(check-true (ascii-numeric? #\0))
(check-true (ascii-numeric? #\9))
(check-false (ascii-numeric? #\a))

#|
名称
----
ascii-digit-value / ascii-upper-case-value / ascii-lower-case-value
ascii-nth-digit / ascii-nth-upper-case / ascii-nth-lower-case
在字符与数值语义之间做双向转换。

语法
----
(ascii-digit-value x limit)
(ascii-upper-case-value x offset limit)
(ascii-lower-case-value x offset limit)
(ascii-nth-digit n)
(ascii-nth-upper-case n)
(ascii-nth-lower-case n)

参数
----
x : char? | integer?
limit : integer
offset : integer
n : integer

返回值
----
ascii-digit-value / ascii-upper-case-value / ascii-lower-case-value:
    integer? | #f
ascii-nth-digit / ascii-nth-upper-case / ascii-nth-lower-case:
    char? | #f

边界行为
----
覆盖最小/最大有效序号与超界序号（如 -1、10、26）。

性能边界
----
单次映射为常量时间，适合高频调用。

错误处理
----
非法字符、超出 limit 的值或越界序号返回 #f。
|#

(check (ascii-digit-value #\0 10) => 0)
(check (ascii-digit-value #\9 10) => 9)
(check (ascii-digit-value #\9 9) => #f)
(check (ascii-digit-value #\A 10) => #f)

;; 大写字母转数值边界测试
(check (ascii-upper-case-value #\A 10 26) => 10)
(check (ascii-upper-case-value #\F 10 16) => 15)
(check (ascii-upper-case-value #\Q 10 16) => #f)

;; 小写字母转数值边界测试
(check (ascii-lower-case-value #\a 10 26) => 10)
(check (ascii-lower-case-value #\f 10 16) => 15)
(check (ascii-lower-case-value #\q 10 16) => #f)

;; 数值转数字字符边界测试
(check (ascii-nth-digit 0) => #\0)
(check (ascii-nth-digit 9) => #\9)
(check (ascii-nth-digit -1) => #f)
(check (ascii-nth-digit 10) => #f)

;; 数值转大写字母边界测试
(check (ascii-nth-upper-case 0) => #\A)
(check (ascii-nth-upper-case 25) => #\Z)
(check (ascii-nth-upper-case 26) => #\A)

;; 数值转小写字母边界测试
(check (ascii-nth-lower-case 0) => #\a)
(check (ascii-nth-lower-case 25) => #\z)
(check (ascii-nth-lower-case 26) => #\a)

#|
名称
----
ascii-upcase / ascii-downcase / ascii-control->graphic
ascii-graphic->control / ascii-mirror-bracket
处理 ASCII 规则下的大小写、控制字符与括号镜像转换。

语法
----
(ascii-upcase x)
(ascii-downcase x)
(ascii-control->graphic x)
(ascii-graphic->control x)
(ascii-mirror-bracket x)

参数
----
x : char? | integer?

返回值
----
ascii-upcase / ascii-downcase:
    与输入同类型（char 或 integer）
ascii-control->graphic / ascii-graphic->control / ascii-mirror-bracket:
    与输入同类型或 #f

边界行为
----
覆盖可转换区间边界与不可转换输入（如普通字母传给镜像函数）。

性能边界
----
所有转换均为常量时间。

错误处理
----
对不可转换输入返回 #f，并保持可转换输入的原始类型。
|#

(check (ascii-upcase #\a) => #\A)
(check (ascii-upcase #\A) => #\A)
(check (ascii-upcase #\?) => #\?)
(check (ascii-upcase 97) => 65)

;; 下折叠边界测试
(check (ascii-downcase #\A) => #\a)
(check (ascii-downcase #\a) => #\a)
(check (ascii-downcase #\?) => #\?)
(check (ascii-downcase 65) => 97)

;; 控制字符转图形字符边界测试
(check (ascii-control->graphic #x00) => #x40)
(check (ascii-control->graphic #x1f) => #x5f)
(check (ascii-control->graphic #x7f) => #x3f)
(check (ascii-control->graphic #\x7f) => #\?)
(check (ascii-control->graphic #x20) => #f)

;; 图形字符转控制字符边界测试
(check (ascii-graphic->control #x40) => #x00)
(check (ascii-graphic->control #x5f) => #x1f)
(check (ascii-graphic->control #x3f) => #x7f)
(check (ascii-graphic->control #\@) => #\nul)
(check (ascii-graphic->control #\A) => #\x01)
(check (ascii-graphic->control #x20) => #f)

;; 括号镜像边界测试
(check (ascii-mirror-bracket #\() => #\))
(check (ascii-mirror-bracket #\]) => #\[)
(check (ascii-mirror-bracket #\>) => #\<)
(check (ascii-mirror-bracket #\A) => #f)
(check (ascii-mirror-bracket 40) => 41)

#|
名称
----
ascii-ci=? / ascii-ci<? / ascii-ci>? / ascii-ci<=? / ascii-ci>=?
ascii-string-ci=? / ascii-string-ci<? / ascii-string-ci>?
ascii-string-ci<=? / ascii-string-ci>=?
提供 ASCII 范围内大小写无关的字符与字符串比较。

语法
----
(ascii-ci=? char1 char2)
(ascii-ci<? char1 char2)
(ascii-ci>? char1 char2)
(ascii-ci<=? char1 char2)
(ascii-ci>=? char1 char2)
(ascii-string-ci=? string1 string2)
(ascii-string-ci<? string1 string2)
(ascii-string-ci>? string1 string2)
(ascii-string-ci<=? string1 string2)
(ascii-string-ci>=? string1 string2)

参数
----
char1, char2 : char? | integer?
string1, string2 : string?

返回值
----
boolean?

边界行为
----
覆盖大小写折叠后的相等、小于、大于与边界相邻比较。

性能边界
----
字符比较为常量时间；字符串比较与最短可判定前缀长度线性相关。

错误处理
----
参数类型不匹配时按过程约定触发类型错误。
|#

(check-true (ascii-ci=? #\a #\A))
(check-false (ascii-ci=? #\a #\b))
(check-true (ascii-ci<? #\a #\B))
(check-true (ascii-ci>? #\Z #\y))
(check-true (ascii-ci<=? #\A #\a))
(check-true (ascii-ci>=? #\z #\Y))

;; 字符串大小写无关比较边界测试
(check-true (ascii-string-ci=? "GoldFish" "goldfish"))
(check-false (ascii-string-ci=? "goldfish" "gold-fish"))
(check-true (ascii-string-ci<? "abc" "ABD"))
(check-true (ascii-string-ci>? "ABD" "abc"))
(check-true (ascii-string-ci<=? "abc" "ABC"))
(check-true (ascii-string-ci<=? "abc" "abd"))
(check-true (ascii-string-ci>=? "ABD" "abc"))
(check-true (ascii-string-ci>=? "abc" "ABC"))

(check-report)

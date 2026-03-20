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
        (scheme char)
) ;import

(check-set-mode! 'report-failed)

#|
char-upcase
将字符转换为大写形式

函数签名
----
(char-upcase char) → char

参数
----
char : character
要转换的字符

返回值
----
character
转换后的大写字符

描述
----
`char-upcase` 用于将字符转换为大写形式。如果字符已经是大写或不是字母，则返回原字符。

行为特征
------
- 对于小写字母，返回对应的大写字母
- 对于大写字母，返回原字符
- 对于非字母字符，返回原字符
- 遵循 R7RS 标准规范


错误处理
------
- 参数必须是字符类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- char-upcase 是内置函数，由 S7 scheme 引擎实现
- 不需要额外的实现代码

相关函数
--------
- `char-downcase` : 将字符转换为小写形式
- `char-upper-case?` : 判断字符是否为大写字母
- `char-lower-case?` : 判断字符是否为小写字母
- `char-foldcase` : 执行大小写折叠
|#

(check (char-upcase #\z) => #\Z)
(check (char-upcase #\a) => #\A)

(check (char-upcase #\A) => #\A)
(check (char-upcase #\?) => #\?)
(check (char-upcase #\$) => #\$)
(check (char-upcase #\.) => #\.)
(check (char-upcase #\\) => #\\)
(check (char-upcase #\5) => #\5)
(check (char-upcase #\)) => #\))
(check (char-upcase #\%) => #\%)
(check (char-upcase #\0) => #\0)
(check (char-upcase #\_) => #\_)
(check (char-upcase #\?) => #\?)
(check (char-upcase #\space) => #\space)
(check (char-upcase #\newline) => #\newline)
(check (char-upcase #\null) => #\null)

;; Test char-upcase error handling
(check-catch 'type-error (char-upcase "a"))
(check-catch 'type-error (char-upcase 65))
(check-catch 'type-error (char-upcase 'a))

#|
char-downcase
将字符转换为小写形式

函数签名
----
(char-downcase char) → char

参数
----
char : character
要转换的字符

返回值
----
character
转换后的小写字符

描述
----
`char-downcase` 用于将字符转换为小写形式。如果字符已经是小写或不是字母，则返回原字符。

行为特征
------
- 对于大写字母，返回对应的小写字母
- 对于小写字母，返回原字符
- 对于非字母字符，返回原字符
- 遵循 R7RS 标准规范

错误处理
------
- 参数必须是字符类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- char-downcase 是内置函数，由 S7 scheme 引擎实现
- 不需要额外的实现代码

相关函数
--------
- `char-upcase` : 将字符转换为大写形式
- `char-upper-case?` : 判断字符是否为大写字母
- `char-lower-case?` : 判断字符是否为小写字母
- `char-foldcase` : 执行大小写折叠
|#

#|
char-foldcase
执行字符的大小写折叠

函数签名
----
(char-foldcase char) → char

参数
----
char : character
要转换的字符

返回值
----
character
转换后的字符

描述
----
`char-foldcase` 用于执行字符的大小写折叠。大小写折叠是一种 Unicode 规范化过程，
用于将字符转换为一种形式，使得大小写不敏感的比较能够正确工作。

行为特征
------
- 对于 ASCII 字母字符，当前实现与 `char-downcase` 相同
- 对于非字母字符，返回原字符
- 遵循 R7RS 标准规范

错误处理
------
- 参数必须是字符类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- 当前实现暂时定义为 `char-downcase`，因为 S7 目前不支持希腊字母等 Unicode 字符字面量
- 对于完整的 Unicode 大小写折叠支持，需要底层引擎的增强

相关函数
--------
- `char-upcase` : 将字符转换为大写形式
- `char-downcase` : 将字符转换为小写形式
- `char-upper-case?` : 判断字符是否为大写字母
- `char-lower-case?` : 判断字符是否为小写字母
|#

(check (char-downcase #\A) => #\a)
(check (char-downcase #\Z) => #\z)

(check (char-downcase #\a) => #\a)

;; Test char-downcase error handling
(check-catch 'type-error (char-downcase "A"))
(check-catch 'type-error (char-downcase 65))
(check-catch 'type-error (char-downcase 'A))

(check-true (char-upper-case? #\A))
(check-true (char-upper-case? #\Z))

(check-false (char-upper-case? #\a))
(check-false (char-upper-case? #\z))

(check-true (char-lower-case? #\a))
(check-true (char-lower-case? #\z))

(check-false (char-lower-case? #\A))
(check-false (char-lower-case? #\Z))

#|
char-numeric?
判断字符是否为数字。

函数签名
----
(char-numeric? char) → boolean?

参数
----
char : character
要判断的字符

返回值
----
boolean?
如果字符是数字则返回 #t，否则返回 #f

描述
----
`char-numeric?` 用于判断字符是否为数字字符。该函数只处理基本的 ASCII 数字字符（0-9）。

行为特征
------
- 对于数字字符 #\0 到 #\9，返回 #t
- 对于非数字字符，返回 #f
- 遵循 R7RS 标准规范

错误处理
------
- 参数必须是字符类型，否则会抛出异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- char-numeric? 是内置函数，由 S7 scheme 引擎实现
- 不需要额外的实现代码

相关函数
--------
- `digit-value` : 获取数字字符对应的整数值
- `char-alphabetic?` : 判断字符是否为字母
- `char-whitespace?` : 判断字符是否为空白字符
|#

;; 数字范围测试
(check (char-numeric? #\0) => #t)
(check (char-numeric? #\1) => #t)
(check (char-numeric? #\2) => #t)
(check (char-numeric? #\3) => #t)
(check (char-numeric? #\4) => #t)
(check (char-numeric? #\5) => #t)
(check (char-numeric? #\6) => #t)
(check (char-numeric? #\7) => #t)
(check (char-numeric? #\8) => #t)
(check (char-numeric? #\9) => #t)

;; 非数字字符测试
(check (char-numeric? #\a) => #f)
(check (char-numeric? #\A) => #f)
(check (char-numeric? #\z) => #f)
(check (char-numeric? #\Z) => #f)
(check (char-numeric? #\!) => #f)
(check (char-numeric? #\@) => #f)
(check (char-numeric? #\#) => #f)

;; 特殊字符测试
(check (char-numeric? #\space) => #f)
(check (char-numeric? #\newline) => #f)
(check (char-numeric? #\tab) => #f)
(check (char-numeric? #\.) => #f)
(check (char-numeric? #\-) => #f)

;; 字母与数字边界测试
(check (char-numeric? #\/) => #f)
(check (char-numeric? #\:) => #f)

;; 错误处理测试
(check-catch 'type-error (char-numeric? 1))
(check-catch 'type-error (char-numeric? "1"))
(check-catch 'wrong-number-of-args (char-numeric?))
(check-catch 'wrong-number-of-args (char-numeric? #\1 #\2))

#|
char-alphabetic?
判断字符是否为字母。

函数签名
----
(char-alphabetic? char) → boolean?

参数
----
char : character
要判断的字符

返回值
----
boolean?
如果字符是字母则返回 #t，否则返回 #f

描述
----
`char-alphabetic?` 用于判断字符是否为字母字符。该函数正确处理大小写字母。

行为特征
------
- 对于字母字符（A-Z, a-z），返回 #t
- 对于非字母字符，返回 #f
- 遵循 R7RS 标准规范

错误处理
------
- 参数必须是字符类型，否则会抛出异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- char-alphabetic? 是内置函数，由 S7 scheme 引擎实现
- 不需要额外的实现代码

相关函数
--------
- `char-upper-case?` : 判断字符是否为大写字母
- `char-lower-case?` : 判断字符是否为小写字母
- `char-numeric?` : 判断字符是否为数字
|#

;; 小写字母测试
(check (char-alphabetic? #\a) => #t)
(check (char-alphabetic? #\b) => #t)
(check (char-alphabetic? #\z) => #t)

;; 大写字母测试
(check (char-alphabetic? #\A) => #t)
(check (char-alphabetic? #\B) => #t)
(check (char-alphabetic? #\Z) => #t)

;; 非字母字符测试
(check (char-alphabetic? #\0) => #f)
(check (char-alphabetic? #\1) => #f)
(check (char-alphabetic? #\9) => #f)
(check (char-alphabetic? #\!) => #f)
(check (char-alphabetic? #\@) => #f)
(check (char-alphabetic? #\#) => #f)

;; 特殊字符测试
(check (char-alphabetic? #\space) => #f)
(check (char-alphabetic? #\newline) => #f)
(check (char-alphabetic? #\tab) => #f)
(check (char-alphabetic? #\return) => #f)

;; 边界字符测试
(check (char-alphabetic? #\[) => #f)
(check (char-alphabetic? #\\) => #f)
(check (char-alphabetic? #\`) => #f)
(check (char-alphabetic? #\{) => #f)

;; 错误处理测试
(check-catch 'type-error (char-alphabetic? 1))
(check-catch 'type-error (char-alphabetic? "a"))
(check-catch 'wrong-number-of-args (char-alphabetic?))
(check-catch 'wrong-number-of-args (char-alphabetic? #\a #\b))

#|
digit-value
获取数字字符的数值

函数签名
----
(digit-value char) → integer | #f

参数
----
char : character
要获取数值的字符

返回值
----
integer | #f
如果字符是数字字符，返回对应的整数值（0-9）；否则返回 #f

描述
----
`digit-value` 用于获取数字字符对应的整数值。该函数只处理基本的 ASCII 数字字符（0-9）。

行为特征
------
- 对于数字字符 #\0 到 #\9，返回对应的整数值 0 到 9
- 对于非数字字符，返回 #f
- 遵循 R7RS 标准规范

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- 使用 char-numeric? 判断字符是否为数字
- 通过字符编码的差值计算数值

相关函数
--------
- `char-numeric?` : 判断字符是否为数字字符
- `char->integer` : 获取字符的整数编码
- `integer->char` : 将整数转换为字符
|#

;; Test digit-value with numeric characters
(check (digit-value #\0) => 0)
(check (digit-value #\1) => 1)
(check (digit-value #\2) => 2)
(check (digit-value #\3) => 3)
(check (digit-value #\4) => 4)
(check (digit-value #\5) => 5)
(check (digit-value #\6) => 6)
(check (digit-value #\7) => 7)
(check (digit-value #\8) => 8)
(check (digit-value #\9) => 9)

;; Test digit-value with non-numeric characters
(check (digit-value #\a) => #f)
(check (digit-value #\c) => #f)
(check (digit-value #\A) => #f)
(check (digit-value #\Z) => #f)
(check (digit-value #\space) => #f)
(check (digit-value #\newline) => #f)
(check (digit-value #\null) => #f)
(check (digit-value #\.) => #f)
(check (digit-value #\,) => #f)
(check (digit-value #\!) => #f)
(check (digit-value #\@) => #f)
(check (digit-value #\$) => #f)
(check (digit-value #\%) => #f)
(check (digit-value #\^) => #f)
(check (digit-value #\&) => #f)
(check (digit-value #\*) => #f)
(check (digit-value #\( ) => #f)
(check (digit-value #\)) => #f)
(check (digit-value #\_) => #f)
(check (digit-value #\+) => #f)
(check (digit-value #\-) => #f)
(check (digit-value #\=) => #f)
(check (digit-value #\[) => #f)
(check (digit-value #\]) => #f)
(check (digit-value #\{) => #f)
(check (digit-value #\}) => #f)
(check (digit-value #\|) => #f)
(check (digit-value #\\) => #f)
(check (digit-value #\:) => #f)
(check (digit-value #\;) => #f)
(check (digit-value #\") => #f)
(check (digit-value #\') => #f)
(check (digit-value #\<) => #f)
(check (digit-value #\>) => #f)
(check (digit-value #\?) => #f)
(check (digit-value #\/) => #f)

#|
char-whitespace?
判断字符是否为空白字符。

函数签名
----
(char-whitespace? char) → boolean?

参数
----
char : character
要判断的字符

返回值
----
boolean?
如果字符是空白字符则返回 #t，否则返回 #f

描述
----
`char-whitespace?` 用于判断字符是否为空白字符。该函数正确处理各种空白字符。

行为特征
------
- 对于空白字符（空格、换行符、制表符等），返回 #t
- 对于非空白字符，返回 #f
- 遵循 R7RS 标准规范

错误处理
------
- 参数必须是字符类型，否则会抛出异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- char-whitespace? 是内置函数，由 S7 scheme 引擎实现
- 不需要额外的实现代码

相关函数
--------
- `char-alphabetic?` : 判断字符是否为字母
- `char-numeric?` : 判断字符是否为数字
- `char-upper-case?` : 判断字符是否为大写字母
- `char-lower-case?` : 判断字符是否为小写字母
|#

;; 标准空白测试
(check (char-whitespace? #\space) => #t)
(check (char-whitespace? #\newline) => #t)
(check (char-whitespace? #\tab) => #t)

;; 控制字符测试
(check (char-whitespace? #\return) => #t)
(check (char-whitespace? #\backspace) => #f)

;; 非空白字符测试
(check (char-whitespace? #\a) => #f)
(check (char-whitespace? #\A) => #f)
(check (char-whitespace? #\0) => #f)
(check (char-whitespace? #\9) => #f)
(check (char-whitespace? #\!) => #f)
(check (char-whitespace? #\@) => #f)

;; 特殊边界测试
(check (char-whitespace? #\0) => #f)
(check (char-whitespace? #\a) => #f)

;; 符号
(check (char-whitespace? #\.) => #f)
(check (char-whitespace? #\,) => #f)
(check (char-whitespace? #\;) => #f)

;; 错误处理测试
(check-catch 'type-error (char-whitespace? 1))
(check-catch 'type-error (char-whitespace? " "))
(check-catch 'wrong-number-of-args (char-whitespace?))
(check-catch 'wrong-number-of-args (char-whitespace? #\space #\a))

#|
char-upper-case?
判断字符是否为大写字母字符。

函数签名
----
(char-upper-case? char) → boolean?

参数
----
char : character
要判断的字符

返回值
----
boolean?
如果字符是大写字母则返回 #t，否则返回 #f

描述
----
`char-upper-case?` 用于判断字符是否为大写字母。该函数只处理基本的 ASCII 大写字母字符（A-Z）。

行为特征
------
- 对于大写字母字符（A-Z），返回 #t
- 对于小写字母、数字、符号和空白字符，返回 #f
- 遵循 R7RS 标准规范

错误处理
------
- 参数必须是字符类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- char-upper-case? 是内置函数，由 S7 scheme 引擎实现
- 不需要额外的实现代码

相关函数
--------
- `char-lower-case?` : 判断字符是否为小写字母
- `char-alphabetic?` : 判断字符是否为字母
- `char-numeric?` : 判断字符是否为数字
- `char-whitespace?` : 判断字符是否为空白字符
|#

;; char-upper-case? 大写字母测试
(check (char-upper-case? #\A) => #t)
(check (char-upper-case? #\B) => #t)
(check (char-upper-case? #\Z) => #t)

;; 小写字母测试
(check (char-upper-case? #\a) => #f)
(check (char-upper-case? #\z) => #f)
(check (char-upper-case? #\b) => #f)

;; 特殊测试
(check (char-upper-case? #\@) => #f)
(check (char-upper-case? #\[) => #f)
(check (char-upper-case? #\`) => #f)

;; 非字母字符测试
(check (char-upper-case? #\0) => #f)
(check (char-upper-case? #\9) => #f)
(check (char-upper-case? #\!) => #f)
(check (char-upper-case? #\space) => #f)
(check (char-upper-case? #\newline) => #f)

;; 混合测试
(check (char-upper-case? #\@) => #f)
(check (char-upper-case? #\_) => #f)
(check (char-upper-case? #\`) => #f)

;; 字母测试
(check (char-upper-case? #\M) => #t)
(check (char-upper-case? #\m) => #f)

;; 错误处理测试
(check-catch 'type-error (char-upper-case? 1))
(check-catch 'type-error (char-upper-case? "A"))
(check-catch 'wrong-number-of-args (char-upper-case?))
(check-catch 'wrong-number-of-args (char-upper-case? #\A #\B))

#|
char-lower-case?
判断字符是否为小写字母字符。

函数签名
----
(char-lower-case? char) → boolean?

参数
----
char : character
要判断的字符

返回值
----
boolean?
如果字符是小写字母则返回 #t，否则返回 #f

描述
----
`char-lower-case?` 用于判断字符是否为小写字母。该函数只处理基本的 ASCII 小写字母字符（a-z）。

行为特征
------
- 对于小写字母字符（a-z），返回 #t
- 对于大写字母、数字、符号和空白字符，返回 #f
- 遵循 R7RS 标准规范

错误处理
------
- 参数必须是字符类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- char-lower-case? 是内置函数，由 S7 scheme 引擎实现
- 不需要额外的实现代码

相关函数
--------
- `char-upper-case?` : 判断字符是否为大写字母
- `char-alphabetic?` : 判断字符是否为字母
- `char-numeric?` : 判断字符是否为数字
- `char-whitespace?` : 判断字符是否为空白字符
|#

;; char-lower-case? 小写字母测试
(check (char-lower-case? #\a) => #t)
(check (char-lower-case? #\b) => #t)
(check (char-lower-case? #\z) => #t)

;; 大写字母测试
(check (char-lower-case? #\A) => #f)
(check (char-lower-case? #\B) => #f)
(check (char-lower-case? #\Z) => #f)

;; 特殊测试
(check (char-lower-case? #\`) => #f)
(check (char-lower-case? #\{) => #f)

;; 非字母字符测试
(check (char-lower-case? #\0) => #f)
(check (char-lower-case? #\9) => #f)
(check (char-lower-case? #\!) => #f)
(check (char-lower-case? #\space) => #f)
(check (char-lower-case? #\newline) => #f)

;; 混合测试
(check (char-lower-case? #\a) => #t)
(check (char-lower-case? #\z) => #t)
(check (char-lower-case? #\_) => #f)
(check (char-lower-case? #\`) => #f)
(check (char-lower-case? #\{) => #f)

;; 字母测试
(check (char-lower-case? #\m) => #t)
(check (char-lower-case? #\M) => #f)

;; 错误处理测试
(check-catch 'type-error (char-lower-case? 1))
(check-catch 'type-error (char-lower-case? "a"))
(check-catch 'wrong-number-of-args (char-lower-case?))
(check-catch 'wrong-number-of-args (char-lower-case? #\a #\b))

#|
char-ci=?
按大小写不敏感的方式比较字符是否相等。

函数签名
-----
(char-ci=? char1 char2 char3 ...) → boolean?

参数
----
char1, char2, char3, ... : character
要比较的字符

返回值
----
boolean?
如果所有字符在大小写不敏感的情况下都相等，则返回 #t，否则返回 #f

描述
----
`char-ci=?` 用于按大小写不敏感的方式比较字符是否相等。该函数忽略字符的大小写差异，
将大写字母和小写字母视为相等。

行为特征
------
- 对于相同字母的大小写不同形式（如 #\a 和 #\A），返回 #t
- 对于相同的字符（无论大小写），返回 #t
- 对于不同的字符，返回 #f
- 支持多个参数，只有当所有字符在大小写不敏感的情况下都相等时才返回 #t
- 遵循 R7RS 标准规范

错误处理
------
- 所有参数必须是字符类型，否则会抛出 `type-error` 异常
- 至少需要两个参数，否则会抛出 `wrong-number-of-args` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- 使用底层 S7 的 char-ci=? 函数实现核心功能
- 添加了参数类型检查，确保所有参数都是字符类型

相关函数
--------
- `char=?` : 按大小写敏感的方式比较字符是否相等
- `char-ci<?` : 按大小写不敏感的方式比较字符是否小于
- `char-ci>?` : 按大小写不敏感的方式比较字符是否大于
- `char-ci<=?` : 按大小写不敏感的方式比较字符是否小于等于
- `char-ci>=?` : 按大小写不敏感的方式比较字符是否大于等于
|#

;; char-ci=? 基本功能测试
(check (char-ci=? #\a #\A) => #t)
(check (char-ci=? #\A #\a) => #t)
(check (char-ci=? #\z #\Z) => #t)
(check (char-ci=? #\Z #\z) => #t)
(check (char-ci=? #\b #\B) => #t)
(check (char-ci=? #\B #\b) => #t)

;; 大小写一致测试
(check (char-ci=? #\a #\a) => #t)
(check (char-ci=? #\A #\A) => #t)
(check (char-ci=? #\1 #\1) => #t)

;; 不同大小写混合测试
(check (char-ci=? #\a #\b) => #f)
(check (char-ci=? #\a #\B) => #f)
(check (char-ci=? #\A #\b) => #f)
(check (char-ci=? #\A #\z) => #f)
(check (char-ci=? #\Z #\a) => #f)

;; 多参数测试
(check (char-ci=? #\a #\a #\A) => #t)
(check (char-ci=? #\A #\a #\a) => #t)
(check (char-ci=? #\z #\Z #\z #\Z) => #t)
(check (char-ci=? #\a #\b #\A) => #f)
(check (char-ci=? #\A #\B #\a) => #f)

;; 数字字符测试（char-ci不影响数字）
(check (char-ci=? #\0 #\0) => #t)
(check (char-ci=? #\1 #\1) => #t)
(check (char-ci=? #\1 #\2) => #f)

;; 特殊字符测试
(check (char-ci=? #\space #\space) => #t)
(check (char-ci=? #\newline #\newline) => #t)
(check (char-ci=? #\! #\!) => #t)
(check (char-ci=? #\! #\@) => #f)

;; 大小写转换边界测试
(check (char-ci=? #\a #\A #\b #\B) => #f)
(check (char-ci=? #\A #\a #\A) => #t)
(check (char-ci=? #\m #\M #\m) => #t)
(check (char-ci=? #\M #\m #\M) => #t)

;; 边界字符测试
(check (char-ci=? #\0 #\a) => #f)
(check (char-ci=? #\A #\z) => #f)
(check (char-ci=? #\Z #\a) => #f)

;; 错误处理测试 - 更新为 type-error
(check-catch 'type-error (char-ci=? 1 #\A))
(check-catch 'type-error (char-ci=? #\A 'symbol))
(check-catch 'wrong-number-of-args (char-ci=?))
(check-catch 'wrong-number-of-args (char-ci=? #\A))

#|
char-ci<?
按大小写不敏感的方式比较字符是否小于。

函数签名
-----
(char-ci<? char1 char2 char3 ...) → boolean?

参数
----
char1, char2, char3, ... : character
要比较的字符

返回值
----
boolean?
如果字符序列在大小写不敏感的情况下严格升序，则返回 #t，否则返回 #f

描述
----
`char-ci<?` 用于按大小写不敏感的方式比较字符是否小于。该函数忽略字符的大小写差异，
将大写字母和小写字母视为相等，然后比较它们的顺序。

行为特征
------
- 对于相同字母的大小写不同形式（如 #\a 和 #\A），返回 #f（相等）
- 对于不同的字符，按大小写不敏感的方式比较顺序
- 支持多个参数，只有当所有字符在大小写不敏感的情况下严格升序时才返回 #t
- 遵循 R7RS 标准规范

错误处理
------
- 所有参数必须是字符类型，否则会抛出 `type-error` 异常
- 至少需要两个参数，否则会抛出 `wrong-number-of-args` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- 使用底层 S7 的 char-ci<? 函数实现核心功能
- 添加了参数类型检查，确保所有参数都是字符类型

相关函数
--------
- `char<?` : 按大小写敏感的方式比较字符是否小于
- `char-ci=?` : 按大小写不敏感的方式比较字符是否相等
- `char-ci>?` : 按大小写不敏感的方式比较字符是否大于
- `char-ci<=?` : 按大小写不敏感的方式比较字符是否小于等于
- `char-ci>=?` : 按大小写不敏感的方式比较字符是否大于等于
|#

;; char-ci<? 基本功能测试
(check (char-ci<? #\a #\B) => #t)
(check (char-ci<? #\A #\b) => #t)
(check (char-ci<? #\A #\a) => #f)  ; 等大写和小写不严格升序
(check (char-ci<? #\a #\A) => #f)
(check (char-ci<? #\Z #\a) => #f)  ; A-Z在a-z之前大写
(check (char-ci<? #\z #\A) => #f)

;; 大小写一致测试
(check (char-ci<? #\a #\b) => #t)
(check (char-ci<? #\A #\B) => #t)
(check (char-ci<? #\B #\a) => #f)  ; B > a小写
(check (char-ci<? #\z #\A) => #f)

;; 相等字符测试
(check (char-ci<? #\a #\A) => #f)
(check (char-ci<? #\A #\A) => #f)
(check (char-ci<? #\a #\a) => #f)

;; 特殊字符测试
(check (char-ci<? #\space #\newline) => #f)
(check (char-ci<? #\tab #\space) => #t)
(check (char-ci<? #\! #\@) => #t)
(check (char-ci<? #\! #\!) => #f)

;; 多参数测试
(check (char-ci<? #\a #\B #\c #\D) => #t)
(check (char-ci<? #\A #\b #\C #\d) => #t)
(check (char-ci<? #\a #\A #\b #\B) => #f)  ; 等大写和小写
(check (char-ci<? #\a #\z #\A #\B) => #f)

;; 数字字符测试
(check (char-ci<? #\0 #\1) => #t)
(check (char-ci<? #\9 #\0) => #f)
(check (char-ci<? #\1 #\A) => #t)
(check (char-ci<? #\9 #\a) => #t)

;; 字母范围测试
(check (char-ci<? #\a #\z) => #t)
(check (char-ci<? #\A #\Z) => #t)
(check (char-ci<? #\z #\a) => #f)
(check (char-ci<? #\Z #\a) => #f)

;; 特殊字符边界测试
(check (char-ci<? #\0 #\! ) => #f)
(check (char-ci<? #\space #\!) => #t)
(check (char-ci<? #\tab #\newline) => #t)

;; 错误处理测试 - 更新为 type-error
(check-catch 'type-error (char-ci<? 1 #\A))
(check-catch 'type-error (char-ci<? #\A 'symbol))
(check-catch 'wrong-number-of-args (char-ci<?))
(check-catch 'wrong-number-of-args (char-ci<? #\A))

#|
char-ci>?
按大小写不敏感的方式比较字符是否大于。

函数签名
-----
(char-ci>? char1 char2 char3 ...) → boolean?

参数
----
char1, char2, char3, ... : character
要比较的字符

返回值
----
boolean?
如果字符序列在大小写不敏感的情况下严格降序，则返回 #t，否则返回 #f

描述
----
`char-ci>?` 用于按大小写不敏感的方式比较字符是否大于。该函数忽略字符的大小写差异，
将大写字母和小写字母视为相等，然后比较它们的顺序。

行为特征
------
- 对于相同字母的大小写不同形式（如 #\a 和 #\A），返回 #f（相等）
- 对于不同的字符，按大小写不敏感的方式比较顺序
- 支持多个参数，只有当所有字符在大小写不敏感的情况下严格降序时才返回 #t
- 遵循 R7RS 标准规范

错误处理
------
- 所有参数必须是字符类型，否则会抛出 `type-error` 异常
- 至少需要两个参数，否则会抛出 `wrong-number-of-args` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- 使用底层 S7 的 char-ci>? 函数实现核心功能
- 添加了参数类型检查，确保所有参数都是字符类型

相关函数
--------
- `char>?` : 按大小写敏感的方式比较字符是否大于
- `char-ci=?` : 按大小写不敏感的方式比较字符是否相等
- `char-ci<?` : 按大小写不敏感的方式比较字符是否小于
- `char-ci<=?` : 按大小写不敏感的方式比较字符是否小于等于
- `char-ci>=?' : 按大小写不敏感的方式比较字符是否大于等于
|#

;; char-ci>? 基本功能测试
(check (char-ci>? #\B #\a) => #t)
(check (char-ci>? #\b #\A) => #t)
(check (char-ci>? #\a #\A) => #f)  ; 等大写和小写不严格降序
(check (char-ci>? #\A #\a) => #f)
(check (char-ci>? #\a #\z) => #f)
(check (char-ci>? #\Z #\a) => #t)  ; Z > a

;; 大小写一致测试
(check (char-ci>? #\b #\a) => #t)
(check (char-ci>?  #\B #\A) => #t)
(check (char-ci>? #\a #\B) => #f)  ; a < B
(check (char-ci>? #\z #\A) => #t)

;; 相等字符测试（不严格降序）
(check (char-ci>? #\a #\A) => #f)
(check (char-ci>? #\A #\A) => #f)
(check (char-ci>? #\a #\a) => #f)

;; 特殊字符测试
(check (char-ci>? #\newline #\space) => #f)
(check (char-ci>? #\space #\tab) => #t)
(check (char-ci>? #\@ #\!) => #t)
(check (char-ci>? #\! #\!) => #f)

;; 多参数不敏感降序测试
(check (char-ci>? #\D #\c #\B #\a) => #t)
(check (char-ci>? #\d #\C #\b #\A) => #t)
(check (char-ci>? #\z #\z #\a) => #f)  ; 等大写和小写
(check (char-ci>? #\b #\a #\C) => #f)

;; 数字字符测试（大小写不影响）
(check (char-ci>? #\9 #\0) => #t)
(check (char-ci>?  #\0 #\9) => #f)
(check (char-ci>? #\z #\0) => #t)
(check (char-ci>? #\a #\0) => #t)

;; 边界测试
(check (char-ci>? #\z #\a) => #t)
(check (char-ci>? #\Z #\A) => #t)
(check (char-ci>? #\a #\z) => #f)
(check (char-ci>? #\A #\Z) => #f)

;; 非字母字符测试
(check (char-ci>? #\! #\0) => #f)
(check (char-ci>? #\~ #\0) => #t)
(check (char-ci>? #\~ #\space) => #t)

;; 错误处理测试 - 更新为 type-error
(check-catch 'type-error (char-ci>? 1 #\A))
(check-catch 'type-error (char-ci>? #\A 'symbol))
(check-catch 'wrong-number-of-args (char-ci>?))
(check-catch 'wrong-number-of-args (char-ci>? #\A))

#|
char-ci>=?
按大小写不敏感的方式比较字符是否大于等于。

函数签名
-----
(char-ci>=? char1 char2 char3 ...) → boolean?

参数
----
char1, char2, char3, ... : character
要比较的字符

返回值
----
boolean?
如果字符序列在大小写不敏感的情况下非严格降序，则返回 #t，否则返回 #f

描述
----
`char-ci>=?` 用于按大小写不敏感的方式比较字符是否大于等于。该函数忽略字符的大小写差异，
将大写字母和小写字母视为相等，然后比较它们的顺序。

行为特征
------
- 对于相同字母的大小写不同形式（如 #\a 和 #\A），返回 #t（相等）
- 对于不同的字符，按大小写不敏感的方式比较顺序
- 支持多个参数，只有当所有字符在大小写不敏感的情况下非严格降序时才返回 #t
- 遵循 R7RS 标准规范

错误处理
------
- 所有参数必须是字符类型，否则会抛出 `type-error` 异常
- 至少需要两个参数，否则会抛出 `wrong-number-of-args` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- 使用底层 S7 的 char-ci>=? 函数实现核心功能
- 添加了参数类型检查，确保所有参数都是字符类型

相关函数
--------
- `char>=?` : 按大小写敏感的方式比较字符是否大于等于
- `char-ci=?` : 按大小写不敏感的方式比较字符是否相等
- `char-ci<?` : 按大小写不敏感的方式比较字符是否小于
- `char-ci>?` : 按大小写不敏感的方式比较字符是否大于
- `char-ci<=?` : 按大小写不敏感的方式比较字符是否小于等于
|#

;; char-ci>=? 基本功能测试
(check (char-ci>=? #\B #\a) => #t)
(check (char-ci>=? #\b #\A) => #t)
(check (char-ci>=? #\a #\A) => #t)  ; 等大写和小写非严格降序
(check (char-ci>=? #\A #\a) => #t)
(check (char-ci>=? #\a #\z) => #f)
(check (char-ci>=? #\Z #\a) => #t)  ; Z >= a

;; 大小写一致测试
(check (char-ci>=? #\b #\a) => #t)
(check (char-ci>=? #\B #\A) => #t)
(check (char-ci>=? #\a #\B) => #f)  ; a < B
(check (char-ci>=? #\z #\A) => #t)

;; 相等字符测试（非严格降序）
(check (char-ci>=? #\a #\A) => #t)
(check (char-ci>=? #\A #\A) => #t)
(check (char-ci>=? #\a #\a) => #t)

;; 特殊字符测试
(check (char-ci>=? #\newline #\space) => #f)
(check (char-ci>=? #\space #\tab) => #t)
(check (char-ci>=? #\@ #\!) => #t)
(check (char-ci>=? #\! #\!) => #t)

;; 多参数非严格降序测试
(check (char-ci>=? #\D #\c #\B #\a) => #t)
(check (char-ci>=? #\d #\C #\b #\A) => #t)
(check (char-ci>=? #\z #\z #\a) => #t)  ; 等大写和小写
(check (char-ci>=? #\b #\a #\C) => #f)

;; 数字字符测试（大小写不影响）
(check (char-ci>=? #\9 #\0) => #t)
(check (char-ci>=? #\0 #\9) => #f)
(check (char-ci>=? #\z #\0) => #t)
(check (char-ci>=? #\a #\0) => #t)

;; 边界测试
(check (char-ci>=? #\z #\a) => #t)
(check (char-ci>=? #\Z #\A) => #t)
(check (char-ci>=? #\a #\z) => #f)
(check (char-ci>=? #\A #\Z) => #f)

;; 非字母字符测试
(check (char-ci>=? #\! #\0) => #f)
(check (char-ci>=? #\~ #\0) => #t)
(check (char-ci>=? #\~ #\space) => #t)

;; 错误处理测试 - 更新为 type-error
(check-catch 'type-error (char-ci>=? 1 #\A))
(check-catch 'type-error (char-ci>=? #\A 'symbol))
(check-catch 'wrong-number-of-args (char-ci>=?))
(check-catch 'wrong-number-of-args (char-ci>=? #\A))

#|
char-ci<=?
按大小写不敏感的方式比较字符是否小于等于。

函数签名
-----
(char-ci<=? char1 char2 char3 ...) → boolean?

参数
----
char1, char2, char3, ... : character
要比较的字符

返回值
----
boolean?
如果字符序列在大小写不敏感的情况下非严格升序，则返回 #t，否则返回 #f

描述
----
`char-ci<=?` 用于按大小写不敏感的方式比较字符是否小于等于。该函数忽略字符的大小写差异，
将大写字母和小写字母视为相等，然后比较它们的顺序。

行为特征
------
- 对于相同字母的大小写不同形式（如 #\a 和 #\A），返回 #t（相等）
- 对于不同的字符，按大小写不敏感的方式比较顺序
- 支持多个参数，只有当所有字符在大小写不敏感的情况下非严格升序时才返回 #t
- 遵循 R7RS 标准规范

错误处理
------
- 所有参数必须是字符类型，否则会抛出 `type-error` 异常
- 至少需要两个参数，否则会抛出 `wrong-number-of-args` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (scheme char) 库中提供
- 使用底层 S7 的 char-ci<=? 函数实现核心功能
- 添加了参数类型检查，确保所有参数都是字符类型

相关函数
--------
- `char<=?` : 按大小写敏感的方式比较字符是否小于等于
- `char-ci=?` : 按大小写不敏感的方式比较字符是否相等
- `char-ci<?` : 按大小写不敏感的方式比较字符是否小于
- `char-ci>?` : 按大小写不敏感的方式比较字符是否大于
- `char-ci>=?` : 按大小写不敏感的方式比较字符是否大于等于
|#

;; char-ci<=? 基本功能测试
(check (char-ci<=? #\a #\B) => #t)
(check (char-ci<=? #\A #\b) => #t)
(check (char-ci<=? #\A #\a) => #t)  ; 等大写和小写非严格升序
(check (char-ci<=? #\a #\A) => #t)
(check (char-ci<=? #\Z #\a) => #f)  ; A-Z在a-z之前大写
(check (char-ci<=? #\z #\a) => #f)

;; 大小写一致测试
(check (char-ci<=? #\a #\b) => #t)
(check (char-ci<=? #\A #\B) => #t)
(check (char-ci<=? #\B #\a) => #f)  ; B > a小写
(check (char-ci<=? #\z #\A) => #f)

;; 相等字符测试（非严格升序）
(check (char-ci<=? #\a #\A) => #t)
(check (char-ci<=? #\A #\a) => #t)
(check (char-ci<=? #\A #\A) => #t)
(check (char-ci<=? #\a #\a) => #t)

;; 多参数非严格升序测试
(check (char-ci<=? #\a #\B #\c #\D) => #t)
(check (char-ci<=? #\A #\a #\b #\B) => #t)  ; 等大写和小写
(check (char-ci<=? #\A #\A #\B #\b) => #t)
(check (char-ci<=? #\a #\a #\a) => #t)
(check (char-ci<=? #\z #\a #\b) => #f)

;; 字母范围测试
(check (char-ci<=? #\a #\z) => #t)
(check (char-ci<=? #\A #\Z) => #t)
(check (char-ci<=? #\z #\z) => #t)
(check (char-ci<=? #\0 #\9) => #t)

;; 特殊字符测试
(check (char-ci<=? #\space #\newline) => #f)
(check (char-ci<=? #\tab #\tab) => #t)  ; 相等返回 true
(check (char-ci<=? #\@ #\newline) => #f)
(check (char-ci<=? #\! #\") => #t)
(check (char-ci<=? #\! #\!) => #t)

;; 数字字符测试
(check (char-ci<=? #\0 #\A) => #t)
(check (char-ci<=? #\9 #\z) => #t)
(check (char-ci<=? #\A #\a #\Z) => #t)
(check (char-ci<=? #\Z #\a #\Z) => #f)

;; 错误处理测试 - 更新为 type-error
(check-catch 'type-error (char-ci<=? 1 #\A))
(check-catch 'type-error (char-ci<=? #\A 'symbol))
(check-catch 'wrong-number-of-args (char-ci<=?))
(check-catch 'wrong-number-of-args (char-ci<=? #\A))

(check-report)


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

(import (liii check)
        (liii unicode)
        (liii base)
) ;import

(check-set-mode! 'report-failed)

#|
utf8->string
将 UTF-8 编码的字节向量转换为字符串

函数签名
----
(utf8->string bytevector) → string

参数
----
bytevector : bytevector
包含 UTF-8 编码字节的字节向量

返回值
----
string
转换后的字符串

描述
----
`utf8->string` 用于将 UTF-8 编码的字节向量转换为字符串。
该函数遵循 R7RS 标准，支持所有有效的 Unicode 字符编码。

行为特征
------
- 支持所有有效的 Unicode 字符，包括基本多文种平面（BMP）和辅助平面字符
- 正确处理 ASCII 字符（单字节编码）
- 正确处理多字节 UTF-8 字符序列
- 空字节向量返回空字符串
- 与 `string->utf8` 函数形成互逆操作

编码规则
------
- ASCII 字符 (U+0000 到 U+007F): 1 字节编码
- 基本多文种平面字符 (U+0080 到 U+07FF): 2 字节编码
- 其他 BMP 字符 (U+0800 到 U+FFFF): 3 字节编码
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码

错误处理
------
- 如果字节向量包含无效的 UTF-8 编码序列，会抛出 `value-error` 异常
- 参数必须是字节向量类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数在 R7RS 标准库中定义，在 (liii base) 库中重新导出
- 与 `string->utf8` 函数形成互逆操作对

相关函数
--------
- `string->utf8` : 将字符串转换为 UTF-8 字节向量
- `utf8-string-length` : 获取字符串的 Unicode 字符数量
- `u8-substring` : 基于 Unicode 字符位置提取子字符串
|#

(check (utf8->string (bytevector #x48 #x65 #x6C #x6C #x6F)) => "Hello")
(check (utf8->string #u8(#xC3 #xA4)) => "ä")
(check (utf8->string #u8(#xE4 #xB8 #xAD)) => "中")
(check (utf8->string #u8(#xF0 #x9F #x91 #x8D)) => "👍")

;; UTF-8 错误处理测试
(check-catch 'value-error (utf8->string (bytevector #xFF #x65 #x6C #x6C #x6F)))

;; utf8->string 边界条件测试
(check (utf8->string #u8()) => "")
(check (utf8->string #u8(#x48)) => "H")
(check (utf8->string #u8(#x48 #x65)) => "He")

;; utf8->string 复杂 Unicode 字符测试
(check (utf8->string #u8(#xF0 #x9F #x9A #x80)) => "🚀")
(check (utf8->string #u8(#xF0 #x9F #x8E #x89)) => "🎉")
(check (utf8->string #u8(#xF0 #x9F #x8E #x8A)) => "🎊")
(check (utf8->string #u8(#xF0 #x9F #x91 #x8D #xF0 #x9F #x9A #x80)) => "👍🚀")

;; utf8->string 混合字符测试
(check (utf8->string #u8(#x48 #x65 #x6C #x6C #x6F #x20 #xF0 #x9F #x9A #x80 #x20 #x57 #x6F #x72 #x6C #x64)) => "Hello 🚀 World")
(check (utf8->string #u8(#xE4 #xBD #xA0 #xE5 #xA5 #xBD #x20 #xF0 #x9F #x8E #x89 #x20 #xE6 #xB5 #x8B #xE8 #xAF #x95)) => "你好 🎉 测试")

;; utf8->string 错误处理测试 - 更多无效 UTF-8 序列
(check-catch 'value-error (utf8->string (bytevector #x80)))
(check-catch 'value-error (utf8->string (bytevector #xF8 #x80 #x80 #x80 #x80)))
(check-catch 'value-error (utf8->string (bytevector #xFC #x80 #x80 #x80 #x80 #x80)))

;; utf8->string 与 string->utf8 互逆操作验证
(check (utf8->string (string->utf8 "")) => "")
(check (utf8->string (string->utf8 "H")) => "H")
(check (utf8->string (string->utf8 "Hello")) => "Hello")
(check (utf8->string (string->utf8 "ä")) => "ä")
(check (utf8->string (string->utf8 "中")) => "中")
(check (utf8->string (string->utf8 "👍")) => "👍")
(check (utf8->string (string->utf8 "🚀")) => "🚀")
(check (utf8->string (string->utf8 "汉字书写")) => "汉字书写")
(check (utf8->string (string->utf8 "Hello 你好 👍")) => "Hello 你好 👍")

;; utf8->string 单字符提取测试
(check (utf8->string #u8(#xE6 #xB1 #x89)) => "汉")
(check (utf8->string #u8(#xE5 #xAD #x97)) => "字")
(check (utf8->string #u8(#xF0 #x9F #x91 #x8D)) => "👍")


#|
string->utf8
将字符串转换为 UTF-8 编码的字节向量

函数签名
----
(string->utf8 string [start [end]]) → bytevector

参数
----
string : string
要转换的字符串

start : integer (可选，默认 0)
起始字符位置（基于字符计数）

end : integer (可选，默认字符串末尾)
结束字符位置（基于字符计数）

返回值
----
bytevector
包含 UTF-8 编码字节的字节向量

描述
----
`string->utf8` 用于将字符串转换为 UTF-8 编码的字节向量。
该函数遵循 R7RS 标准，支持所有有效的 Unicode 字符编码。

行为特征
------
- 支持所有有效的 Unicode 字符，包括基本多文种平面（BMP）和辅助平面字符
- 正确处理 ASCII 字符（单字节编码）
- 正确处理多字节 UTF-8 字符序列
- 支持可选参数 start 和 end 来指定字符串范围
- 空字符串返回空的字节向量

编码规则
------
- ASCII 字符 (U+0000 到 U+007F): 1 字节编码
- 基本多文种平面字符 (U+0080 到 U+07FF): 2 字节编码
- 其他 BMP 字符 (U+0800 到 U+FFFF): 3 字节编码
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码

错误处理
------
- 如果 start 或 end 超出字符串范围，会抛出 `out-of-range` 异常
- 参数必须是正确的类型，否则会抛出 `type-error` 异常
- 如果字符串包含无效的 Unicode 字符，行为取决于具体实现

实现说明
------
- 函数在 R7RS 标准库中定义，在 (liii base) 库中重新导出
- 支持与 `utf8->string` 函数的互逆操作

相关函数
--------
- `utf8->string` : 将 UTF-8 字节向量转换为字符串
- `utf8-string-length` : 获取字符串的 Unicode 字符数量
- `u8-substring` : 基于 Unicode 字符位置提取子字符串
|#


(check (string->utf8 "Hello") => (bytevector #x48 #x65 #x6C #x6C #x6F))
(check (string->utf8 "ä") => #u8(#xC3 #xA4))
(check (string->utf8 "中") => #u8(#xE4 #xB8 #xAD))
(check (string->utf8 "👍") => #u8(#xF0 #x9F #x91 #x8D))
(check (string->utf8 "") => #u8())

;; UTF-8 边界错误处理测试
(check-catch 'out-of-range (string->utf8 "Hello" 2 6))
(check-catch 'out-of-range (string->utf8 "汉字书写" 4))

;; string->utf8 更多边界测试
(check (string->utf8 "Hello" 0 0) => #u8())
(check (string->utf8 "Hello" 1 1) => #u8())
(check (string->utf8 "Hello" 2 3) => #u8(#x6C))  ; "l"
(check (string->utf8 "Hello" 3 5) => #u8(#x6C #x6F))  ; "lo"

;; string->utf8 复杂 Unicode 字符测试
(check (string->utf8 "🚀") => #u8(#xF0 #x9F #x9A #x80))
(check (string->utf8 "🎉") => #u8(#xF0 #x9F #x8E #x89))
(check (string->utf8 "🎊") => #u8(#xF0 #x9F #x8E #x8A))

;; string->utf8 混合字符测试
(check (string->utf8 "Hello 🚀 World") => #u8(#x48 #x65 #x6C #x6C #x6F #x20 #xF0 #x9F #x9A #x80 #x20 #x57 #x6F #x72 #x6C #x64))
(check (string->utf8 "你好 🎉 测试") => #u8(#xE4 #xBD #xA0 #xE5 #xA5 #xBD #x20 #xF0 #x9F #x8E #x89 #x20 #xE6 #xB5 #x8B #xE8 #xAF #x95))

;; string->utf8 默认参数行为测试
(check (string->utf8 "Hello") => (bytevector #x48 #x65 #x6C #x6C #x6F))
(check (string->utf8 "Hello" 2) => #u8(#x6C #x6C #x6F))  ; "llo"
(check (string->utf8 "Hello" 0 3) => #u8(#x48 #x65 #x6C))  ; "Hel"

;; string->utf8 单字符提取测试
(check (string->utf8 "汉") => #u8(#xE6 #xB1 #x89))
(check (string->utf8 "字") => #u8(#xE5 #xAD #x97))
(check (string->utf8 "👍") => #u8(#xF0 #x9F #x91 #x8D))


(check (utf8->string (string->utf8 "Hello" 1 2)) => "e")
(check (utf8->string (string->utf8 "Hello" 0 2)) => "He")
(check (utf8->string (string->utf8 "Hello" 2)) => "llo")
(check (utf8->string (string->utf8 "Hello" 2 5)) => "llo")

(check (utf8->string (string->utf8 "Hello")) => "Hello")
(check (utf8->string (string->utf8 "你好")) => "你好")
(check (utf8->string (string->utf8 "Hello 你好")) => "Hello 你好")

(check (utf8->string (string->utf8 "汉字书写")) => "汉字书写")
(check (utf8->string (string->utf8 "汉字书写" 1)) => "字书写")
(check (utf8->string (string->utf8 "汉字书写" 2)) => "书写")
(check (utf8->string (string->utf8 "汉字书写" 3)) => "写")

#|
utf8-string-length
计算 UTF-8 编码字符串的 Unicode 字符数量（码点数量）。

函数签名
----
(utf8-string-length string) → integer

参数
----
string : string
UTF-8 编码的字符串

返回值
----
integer
字符串中的 Unicode 字符数量（码点数量）

描述
----
`utf8-string-length` 用于计算 UTF-8 编码字符串中的 Unicode 字符数量，与 `string-length` 不同，
它返回的是 Unicode 码点（code point）的数量，而不是字节数量。

行为特征
------
- 对于纯 ASCII 字符串，结果与 `string-length` 相同
- 对于包含多字节 UTF-8 字符的字符串，返回实际的 Unicode 字符数量
- 正确处理各种 Unicode 字符，包括基本多文种平面（BMP）和辅助平面字符
- 支持所有有效的 UTF-8 编码序列

与 string-length 的区别
-------------------
- `string-length` : 返回字符串的字节数量
- `utf8-string-length` : 返回字符串的 Unicode 字符数量


错误处理
------
- 如果字符串包含无效的 UTF-8 编码序列，会抛出 `value-error` 异常
- 参数必须是字符串类型，否则会抛出 `type-error` 异常

实现原理
------
函数通过遍历字符串的 UTF-8 编码字节序列，使用 `bytevector-advance-utf8` 函数
逐个识别完整的 UTF-8 字符，并统计字符数量。

相关函数
--------
- `string-length` : 获取字符串的字节长度
- `u8-substring` : 基于 Unicode 字符位置提取子字符串
- `utf8->string` : 将 UTF-8 字节向量转换为字符串
- `string->utf8` : 将字符串转换为 UTF-8 字节向量
|#

(check (utf8-string-length "") => 0)
(check (utf8-string-length "Hello") => 5)
(check (utf8-string-length "你好") => 2)
(check (utf8-string-length "Hello 你好") => 8)
(check (utf8-string-length "👍") => 1)
(check (utf8-string-length "🚀") => 1)
(check (utf8-string-length "🎉") => 1)
(check (utf8-string-length "Hello 👍 World") => 13)
(check (utf8-string-length "你好 🚀 测试") => 7)

#|
u8-substring
基于 Unicode 字符位置提取子字符串

函数签名
----
(u8-substring string [start [end]]) → string

参数
----
string : string
UTF-8 编码的字符串

start : integer (可选，默认 0)
起始字符位置（基于 Unicode 字符计数）

end : integer (可选，默认字符串末尾)
结束字符位置（基于 Unicode 字符计数）

返回值
----
string
从 start 到 end 的子字符串

描述
----
`u8-substring` 用于从 UTF-8 编码的字符串中提取子字符串，与 `string-substring` 不同，
它基于 Unicode 字符位置而非字节位置进行提取。

行为特征
------
- 对于纯 ASCII 字符串，行为与 `string-substring` 相同
- 对于包含多字节 UTF-8 字符的字符串，基于 Unicode 字符位置进行提取
- 正确处理各种 Unicode 字符，包括基本多文种平面（BMP）和辅助平面字符
- 支持所有有效的 UTF-8 编码序列
- 支持可选参数，start 默认为 0，end 默认为字符串末尾

与 string-substring 的区别
-------------------
- `string-substring` : 基于字节位置提取子字符串
- `u8-substring` : 基于 Unicode 字符位置提取子字符串

错误处理
------
- 如果字符串包含无效的 UTF-8 编码序列，会抛出 `value-error` 异常
- 如果 start 或 end 超出字符串范围，会抛出 `out-of-range` 异常
- 参数必须是正确的类型，否则会抛出 `type-error` 异常

实现原理
------
函数通过 `string->utf8` 将字符串转换为 UTF-8 字节向量，基于 Unicode 字符位置
进行截取，然后使用 `utf8->string` 将字节向量转换回字符串。

相关函数
--------
- `utf8-string-length` : 获取字符串的 Unicode 字符数量
- `string-substring` : 基于字节位置提取子字符串
- `utf8->string` : 将 UTF-8 字节向量转换为字符串
- `string->utf8` : 将字符串转换为 UTF-8 字节向量
|#

(check (u8-substring "Hello 你好" 0 5) => "Hello")
(check (u8-substring "Hello 你好" 6 8) => "你好")

(check (u8-substring "汉字书写" 0 1) => "汉")
(check (u8-substring "汉字书写" 0 4) => "汉字书写")
(check (u8-substring "汉字书写" 0) => "汉字书写")

;; u8-substring 边界条件测试
(check (u8-substring "" 0) => "")
(check (u8-substring "" 0 0) => "")

;; u8-substring 复杂 Unicode 字符测试
(check (u8-substring "Hello 👍 World" 6 7) => "👍")
(check (u8-substring "你好 🚀 测试" 3 4) => "🚀")
(check (u8-substring "🎉🎊🎈" 0 2) => "🎉🎊")
(check (u8-substring "🎉🎊🎈" 1 3) => "🎊🎈")

;; u8-substring 默认参数行为测试
(check (u8-substring "Hello World") => "Hello World")
(check (u8-substring "Hello World" 6) => "World")
(check (u8-substring "Hello World" 0 5) => "Hello")

;; u8-substring 混合字符测试
(check (u8-substring "Hello 你好 🚀" 6 8) => "你好")
(check (u8-substring "Hello 你好 🚀" 9 10) => "🚀")
(check (u8-substring "🎉Hello🎊" 1 6) => "Hello")

;; u8-substring 单字符提取测试
(check (u8-substring "汉字" 0 1) => "汉")
(check (u8-substring "汉字" 1 2) => "字")
(check (u8-substring "👍" 0 1) => "👍")

;; u8-substring 错误处理测试（通过 string->utf8 间接测试）
(check-catch 'out-of-range (u8-substring "Hello" 0 6))
(check-catch 'out-of-range (u8-substring "汉字" 0 3))

#|
utf8->codepoint
将 UTF-8 编码的字节向量转换为 Unicode 码点

函数签名
----
(utf8->codepoint bytevector) → integer

参数
----
bytevector : bytevector
包含 UTF-8 编码字节的字节向量

返回值
----
integer
Unicode 码点值

描述
----
`utf8->codepoint` 用于将 UTF-8 编码的字节序列转换为 Unicode 码点。

解码规则
------
- 1 字节编码 (0xxxxxxx): ASCII 字符 (U+0000 到 U+007F)
- 2 字节编码 (110xxxxx 10xxxxxx): 基本多文种平面字符 (U+0080 到 U+07FF)
- 3 字节编码 (1110xxxx 10xxxxxx 10xxxxxx): 其他 BMP 字符 (U+0800 到 U+FFFF)
- 4 字节编码 (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx): 辅助平面字符 (U+10000 到 U+10FFFF)

错误处理
------
- 如果字节向量包含无效的 UTF-8 编码序列，会抛出 `value-error` 异常
- 参数必须是字节向量类型，否则会抛出 `type-error` 异常
- 如果字节向量为空，会抛出 `value-error` 异常

实现说明
------
- 函数根据字节序列的第一个字节确定编码长度
- 支持与 `codepoint->utf8` 函数的互逆操作

相关函数
--------
- `codepoint->utf8` : 将 Unicode 码点转换为 UTF-8 字节向量
- `string->utf8` : 将字符串转换为 UTF-8 字节向量
- `utf8->string` : 将 UTF-8 字节向量转换为字符串
|#

;; utf8->codepoint ASCII 字符测试 (1字节编码)
(check (utf8->codepoint (bytevector #x48)) => #x48)  ; "H"
(check (utf8->codepoint (bytevector #x65)) => #x65)  ; "e"
(check (utf8->codepoint (bytevector #x6C)) => #x6C)  ; "l"
(check (utf8->codepoint (bytevector #x6F)) => #x6F)  ; "o"
(check (utf8->codepoint (bytevector #x20)) => #x20)  ; 空格
(check (utf8->codepoint (bytevector #x0A)) => #x0A)  ; 换行符

;; utf8->codepoint 基本多文种平面字符测试 (2字节编码)
(check (utf8->codepoint #u8(#xC2 #xA4)) => #xA4)  ; "¤" (CURRENCY SIGN)
(check (utf8->codepoint #u8(#xC3 #xA4)) => #xE4)  ; "ä"
(check (utf8->codepoint #u8(#xC3 #xA9)) => #xE9)  ; "é"
(check (utf8->codepoint #u8(#xC3 #xB6)) => #xF6)  ; "ö"
(check (utf8->codepoint #u8(#xC3 #xBC)) => #xFC)  ; "ü"

;; utf8->codepoint 其他 BMP 字符测试 (3字节编码)
(check (utf8->codepoint #u8(#xE4 #xB8 #xAD)) => #x4E2D)  ; "中"
(check (utf8->codepoint #u8(#xE6 #xB1 #x89)) => #x6C49)  ; "汉"
(check (utf8->codepoint #u8(#xE5 #xAD #x97)) => #x5B57)  ; "字"
(check (utf8->codepoint #u8(#xE5 #x86 #x99)) => #x5199)  ; "写"

;; utf8->codepoint 辅助平面字符测试 (4字节编码)
(check (utf8->codepoint #u8(#xF0 #x9F #x91 #x8D)) => #x1F44D)  ; "👍"
(check (utf8->codepoint #u8(#xF0 #x9F #x9A #x80)) => #x1F680)  ; "🚀"
(check (utf8->codepoint #u8(#xF0 #x9F #x8E #x89)) => #x1F389)  ; "🎉"
(check (utf8->codepoint #u8(#xF0 #x9F #x8E #x8A)) => #x1F38A)  ; "🎊"

;; utf8->codepoint 边界值测试
(check (utf8->codepoint (bytevector #x00)) => 0)  ; 最小码点
(check (utf8->codepoint (bytevector #x7F)) => 127)  ; ASCII 最大
(check (utf8->codepoint #u8(#xC2 #x80)) => 128)  ; 2字节编码最小
(check (utf8->codepoint #u8(#xDF #xBF)) => 2047)  ; 2字节编码最大
(check (utf8->codepoint #u8(#xE0 #xA0 #x80)) => 2048)  ; 3字节编码最小
(check (utf8->codepoint #u8(#xEF #xBF #xBF)) => 65535)  ; 3字节编码最大
(check (utf8->codepoint #u8(#xF0 #x90 #x80 #x80)) => 65536)  ; 4字节编码最小
(check (utf8->codepoint #u8(#xF4 #x8F #xBF #xBF)) => #x10FFFF)  ; Unicode 最大码点

;; utf8->codepoint 错误处理测试
(check-catch 'value-error (utf8->codepoint #u8()))  ; 空字节向量
(check-catch 'value-error (utf8->codepoint (bytevector #x80)))  ; 无效的起始字节
(check-catch 'value-error (utf8->codepoint (bytevector #xC2)))  ; 不完整的2字节序列
(check-catch 'value-error (utf8->codepoint (bytevector #xE4 #xB8)))  ; 不完整的3字节序列
(check-catch 'value-error (utf8->codepoint (bytevector #xF0 #x9F #x91)))  ; 不完整的4字节序列
(check-catch 'value-error (utf8->codepoint (bytevector #xFF)))  ; 无效字节
(check-catch 'value-error (utf8->codepoint (bytevector #xF8 #x80 #x80 #x80 #x80)))  ; 5字节序列（无效）

;; utf8->codepoint 与 codepoint->utf8 互逆操作验证
(check (utf8->codepoint (codepoint->utf8 0)) => 0)
(check (utf8->codepoint (codepoint->utf8 127)) => 127)
(check (utf8->codepoint (codepoint->utf8 128)) => 128)
(check (utf8->codepoint (codepoint->utf8 2047)) => 2047)
(check (utf8->codepoint (codepoint->utf8 2048)) => 2048)
(check (utf8->codepoint (codepoint->utf8 65535)) => 65535)
(check (utf8->codepoint (codepoint->utf8 65536)) => 65536)
(check (utf8->codepoint (codepoint->utf8 #x10FFFF)) => #x10FFFF)
(check (utf8->codepoint (codepoint->utf8 #x48)) => #x48)
(check (utf8->codepoint (codepoint->utf8 #xE4)) => #xE4)
(check (utf8->codepoint (codepoint->utf8 #x4E2D)) => #x4E2D)
(check (utf8->codepoint (codepoint->utf8 #x1F44D)) => #x1F44D)

#|
codepoint->utf8
将 Unicode 码点转换为 UTF-8 编码的字节向量

函数签名
----
(codepoint->utf8 codepoint) → bytevector

参数
----
codepoint : integer
Unicode 码点值

返回值
----
bytevector
包含 UTF-8 编码字节的字节向量

描述
----
`codepoint->utf8` 用于将 Unicode 码点转换为 UTF-8 编码的字节序列。

编码规则
------
- ASCII 字符 (U+0000 到 U+007F): 1 字节编码
- 基本多文种平面字符 (U+0080 到 U+07FF): 2 字节编码
- 其他 BMP 字符 (U+0800 到 U+FFFF): 3 字节编码
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码

错误处理
------
- 如果码点超出 Unicode 范围 (0-0x10FFFF)，会抛出 `value-error` 异常
- 参数必须是整数类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数根据码点大小自动选择合适的 UTF-8 编码长度
- 返回字节向量便于与 `string->utf8` 保持一致

相关函数
--------
- `utf8->codepoint` : 将 UTF-8 字节向量转换为 Unicode 码点
- `string->utf8` : 将字符串转换为 UTF-8 字节向量
- `utf8->string` : 将 UTF-8 字节向量转换为字符串
|#

;; codepoint->utf8 ASCII 字符测试 (1字节编码)
(check (codepoint->utf8 #x48) => (bytevector #x48))  ; "H"
(check (codepoint->utf8 #x65) => (bytevector #x65))  ; "e"
(check (codepoint->utf8 #x6C) => (bytevector #x6C))  ; "l"
(check (codepoint->utf8 #x6F) => (bytevector #x6F))  ; "o"
(check (codepoint->utf8 #x20) => (bytevector #x20))  ; 空格
(check (codepoint->utf8 #x0A) => (bytevector #x0A))  ; 换行符

;; codepoint->utf8 基本多文种平面字符测试 (2字节编码)
(check (codepoint->utf8 #xA4) => #u8(#xC2 #xA4))  ; "¤" (CURRENCY SIGN)
(check (codepoint->utf8 #xE4) => #u8(#xC3 #xA4))  ; "ä"
(check (codepoint->utf8 #xE9) => #u8(#xC3 #xA9))  ; "é"
(check (codepoint->utf8 #xF6) => #u8(#xC3 #xB6))  ; "ö"
(check (codepoint->utf8 #xFC) => #u8(#xC3 #xBC))  ; "ü"

;; codepoint->utf8 其他 BMP 字符测试 (3字节编码)
(check (codepoint->utf8 #x4E2D) => #u8(#xE4 #xB8 #xAD))  ; "中"
(check (codepoint->utf8 #x6C49) => #u8(#xE6 #xB1 #x89))  ; "汉"
(check (codepoint->utf8 #x5B57) => #u8(#xE5 #xAD #x97))  ; "字"
(check (codepoint->utf8 #x5199) => #u8(#xE5 #x86 #x99))  ; "写"

;; codepoint->utf8 辅助平面字符测试 (4字节编码)
(check (codepoint->utf8 #x1F44D) => #u8(#xF0 #x9F #x91 #x8D))  ; "👍"
(check (codepoint->utf8 #x1F680) => #u8(#xF0 #x9F #x9A #x80))  ; "🚀"
(check (codepoint->utf8 #x1F389) => #u8(#xF0 #x9F #x8E #x89))  ; "🎉"
(check (codepoint->utf8 #x1F38A) => #u8(#xF0 #x9F #x8E #x8A))  ; "🎊"

;; codepoint->utf8 边界值测试
(check (codepoint->utf8 0) => (bytevector #x00))  ; 最小码点
(check (codepoint->utf8 127) => (bytevector #x7F))  ; ASCII 最大
(check (codepoint->utf8 128) => #u8(#xC2 #x80))  ; 2字节编码最小
(check (codepoint->utf8 2047) => #u8(#xDF #xBF))  ; 2字节编码最大
(check (codepoint->utf8 2048) => #u8(#xE0 #xA0 #x80))  ; 3字节编码最小
(check (codepoint->utf8 65535) => #u8(#xEF #xBF #xBF))  ; 3字节编码最大
(check (codepoint->utf8 65536) => #u8(#xF0 #x90 #x80 #x80))  ; 4字节编码最小
(check (codepoint->utf8 #x10FFFF) => #u8(#xF4 #x8F #xBF #xBF))  ; Unicode 最大码点

;; codepoint->utf8 错误处理测试
(check-catch 'value-error (codepoint->utf8 -1))  ; 负码点
(check-catch 'value-error (codepoint->utf8 #x110000))  ; 超出 Unicode 范围
(check-catch 'value-error (codepoint->utf8 #x200000))  ; 远超出范围

(check unicode-max-codepoint => #x10FFFF)
(check unicode-replacement-char => #xFFFD)

#|
hexstr->codepoint
将纯十六进制字符串转换为 Unicode 码点

函数签名
----
(hexstr->codepoint hex-string) → integer

参数
----
hex-string : string
纯十六进制字符串，不包含 "U+" 或 "0x" 前缀

返回值
----
integer
Unicode 码点值

描述
----
`hexstr->codepoint` 用于将纯十六进制字符串转换为 Unicode 码点。

行为特征
------
- 支持纯十六进制字符串（如 "1F600"）
- 不区分大小写
- 支持有效的 Unicode 码点范围 (0-0x10FFFF)

错误处理
------
- 如果字符串包含无效的十六进制字符，会抛出 `value-error` 异常
- 如果码点超出 Unicode 范围，会抛出 `value-error` 异常
- 参数必须是字符串类型，否则会抛出 `type-error` 异常

相关函数
--------
- `codepoint->hexstr` : 将 Unicode 码点转换为十六进制字符串
- `codepoint->utf8` : 将 Unicode 码点转换为 UTF-8 字节向量
- `utf8->codepoint` : 将 UTF-8 字节向量转换为 Unicode 码点
|#

#|
codepoint->hexstr
将 Unicode 码点转换为纯十六进制字符串

函数签名
----
(codepoint->hexstr codepoint) → string

参数
----
codepoint : integer
Unicode 码点值

返回值
----
string
纯十六进制字符串，不包含 "U+" 或 "0x" 前缀

描述
----
`codepoint->hexstr` 用于将 Unicode 码点转换为纯十六进制字符串。

行为特征
------
- 返回纯十六进制字符串（如 "1F600"）
- 输出使用大写字母
- 支持有效的 Unicode 码点范围 (0-0x10FFFF)

错误处理
------
- 如果码点超出 Unicode 范围，会抛出 `value-error` 异常
- 参数必须是整数类型，否则会抛出 `type-error` 异常

相关函数
--------
- `hexstr->codepoint` : 将十六进制字符串转换为 Unicode 码点
- `codepoint->utf8` : 将 Unicode 码点转换为 UTF-8 字节向量
- `utf8->codepoint` : 将 UTF-8 字节向量转换为 Unicode 码点
|#

;; hexstr->codepoint 基本功能测试
(check (hexstr->codepoint "48") => #x48)  ; "H"
(check (hexstr->codepoint "65") => #x65)  ; "e"
(check (hexstr->codepoint "6C") => #x6C)  ; "l"
(check (hexstr->codepoint "6F") => #x6F)  ; "o"
(check (hexstr->codepoint "20") => #x20)  ; 空格
(check (hexstr->codepoint "0A") => #x0A)  ; 换行符

;; hexstr->codepoint 基本多文种平面字符测试
(check (hexstr->codepoint "A4") => #xA4)  ; "¤" (CURRENCY SIGN)
(check (hexstr->codepoint "E4") => #xE4)  ; "ä"
(check (hexstr->codepoint "E9") => #xE9)  ; "é"
(check (hexstr->codepoint "F6") => #xF6)  ; "ö"
(check (hexstr->codepoint "FC") => #xFC)  ; "ü"

;; hexstr->codepoint 其他 BMP 字符测试
(check (hexstr->codepoint "4E2D") => #x4E2D)  ; "中"
(check (hexstr->codepoint "6C49") => #x6C49)  ; "汉"
(check (hexstr->codepoint "5B57") => #x5B57)  ; "字"
(check (hexstr->codepoint "5199") => #x5199)  ; "写"

;; hexstr->codepoint 辅助平面字符测试
(check (hexstr->codepoint "1F44D") => #x1F44D)  ; "👍"
(check (hexstr->codepoint "1F680") => #x1F680)  ; "🚀"
(check (hexstr->codepoint "1F389") => #x1F389)  ; "🎉"
(check (hexstr->codepoint "1F38A") => #x1F38A)  ; "🎊"

;; hexstr->codepoint 边界值测试
(check (hexstr->codepoint "0") => 0)  ; 最小码点
(check (hexstr->codepoint "7F") => 127)  ; ASCII 最大
(check (hexstr->codepoint "80") => 128)  ; 2字节编码最小
(check (hexstr->codepoint "7FF") => 2047)  ; 2字节编码最大
(check (hexstr->codepoint "800") => 2048)  ; 3字节编码最小
(check (hexstr->codepoint "FFFF") => 65535)  ; 3字节编码最大
(check (hexstr->codepoint "10000") => 65536)  ; 4字节编码最小
(check (hexstr->codepoint "10FFFF") => #x10FFFF)  ; Unicode 最大码点

;; hexstr->codepoint 不区分大小写测试
(check (hexstr->codepoint "1f44d") => #x1F44D)  ; "👍"
(check (hexstr->codepoint "1F44D") => #x1F44D)  ; "👍"
(check (hexstr->codepoint "1f44D") => #x1F44D)  ; "👍"

;; hexstr->codepoint 错误处理测试
(check-catch 'value-error (hexstr->codepoint ""))  ; 空字符串
(check-catch 'value-error (hexstr->codepoint "G"))  ; 无效十六进制字符
(check-catch 'value-error (hexstr->codepoint "1G"))  ; 包含无效字符
(check-catch 'value-error (hexstr->codepoint "110000"))  ; 超出 Unicode 范围
(check-catch 'value-error (hexstr->codepoint "200000"))  ; 远超出范围

;; codepoint->hexstr 基本功能测试
(check (codepoint->hexstr #x48) => "48")  ; "H"
(check (codepoint->hexstr #x65) => "65")  ; "e"
(check (codepoint->hexstr #x6C) => "6C")  ; "l"
(check (codepoint->hexstr #x6F) => "6F")  ; "o"
(check (codepoint->hexstr #x20) => "20")  ; 空格
(check (codepoint->hexstr #x0A) => "0A")  ; 换行符

;; codepoint->hexstr 基本多文种平面字符测试
(check (codepoint->hexstr #xA4) => "A4")  ; "¤" (CURRENCY SIGN)
(check (codepoint->hexstr #xE4) => "E4")  ; "ä"
(check (codepoint->hexstr #xE9) => "E9")  ; "é"
(check (codepoint->hexstr #xF6) => "F6")  ; "ö"
(check (codepoint->hexstr #xFC) => "FC")  ; "ü"

;; codepoint->hexstr 其他 BMP 字符测试
(check (codepoint->hexstr #x4E2D) => "4E2D")  ; "中"
(check (codepoint->hexstr #x6C49) => "6C49")  ; "汉"
(check (codepoint->hexstr #x5B57) => "5B57")  ; "字"
(check (codepoint->hexstr #x5199) => "5199")  ; "写"

;; codepoint->hexstr 辅助平面字符测试
(check (codepoint->hexstr #x1F44D) => "1F44D")  ; "👍"
(check (codepoint->hexstr #x1F680) => "1F680")  ; "🚀"
(check (codepoint->hexstr #x1F389) => "1F389")  ; "🎉"
(check (codepoint->hexstr #x1F38A) => "1F38A")  ; "🎊"

;; codepoint->hexstr 边界值测试
(check (codepoint->hexstr 0) => "0")  ; 最小码点
(check (codepoint->hexstr 127) => "7F")  ; ASCII 最大
(check (codepoint->hexstr 128) => "80")  ; 2字节编码最小
(check (codepoint->hexstr 2047) => "7FF")  ; 2字节编码最大
(check (codepoint->hexstr 2048) => "800")  ; 3字节编码最小
(check (codepoint->hexstr 65535) => "FFFF")  ; 3字节编码最大
(check (codepoint->hexstr 65536) => "10000")  ; 4字节编码最小
(check (codepoint->hexstr #x10FFFF) => "10FFFF")  ; Unicode 最大码点

;; codepoint->hexstr 错误处理测试
(check-catch 'value-error (codepoint->hexstr -1))  ; 负码点
(check-catch 'value-error (codepoint->hexstr #x110000))  ; 超出 Unicode 范围
(check-catch 'value-error (codepoint->hexstr #x200000))  ; 远超出范围

;; hexstr->codepoint 与 codepoint->hexstr 互逆操作验证
(check (hexstr->codepoint (codepoint->hexstr 0)) => 0)
(check (hexstr->codepoint (codepoint->hexstr 127)) => 127)
(check (hexstr->codepoint (codepoint->hexstr 128)) => 128)
(check (hexstr->codepoint (codepoint->hexstr 2047)) => 2047)
(check (hexstr->codepoint (codepoint->hexstr 2048)) => 2048)
(check (hexstr->codepoint (codepoint->hexstr 65535)) => 65535)
(check (hexstr->codepoint (codepoint->hexstr 65536)) => 65536)
(check (hexstr->codepoint (codepoint->hexstr #x10FFFF)) => #x10FFFF)
(check (hexstr->codepoint (codepoint->hexstr #x48)) => #x48)
(check (hexstr->codepoint (codepoint->hexstr #xE4)) => #xE4)
(check (hexstr->codepoint (codepoint->hexstr #x4E2D)) => #x4E2D)
(check (hexstr->codepoint (codepoint->hexstr #x1F44D)) => #x1F44D)

#|
codepoint->utf16be
将 Unicode 码点转换为 UTF-16BE 编码的字节向量

函数签名
----
(codepoint->utf16be codepoint) → bytevector

参数
----
codepoint : integer
Unicode 码点值

返回值
----
bytevector
包含 UTF-16BE 编码字节的字节向量

描述
----
`codepoint->utf16be` 用于将 Unicode 码点转换为 UTF-16BE 编码的字节序列。

编码规则
------
- 基本多文种平面字符 (U+0000 到 U+FFFF): 2 字节编码
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码（代理对）

错误处理
------
- 如果码点超出 Unicode 范围 (0-0x10FFFF)，会抛出 `value-error` 异常
- 如果码点在代理对范围 (U+D800 到 U+DFFF)，会抛出 `value-error` 异常
- 参数必须是整数类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数根据码点大小自动选择合适的 UTF-16BE 编码长度
- 对于辅助平面字符，使用代理对编码
- 返回字节向量便于与其他编码函数保持一致

相关函数
--------
- `utf16be->codepoint` : 将 UTF-16BE 字节向量转换为 Unicode 码点
- `codepoint->utf8` : 将 Unicode 码点转换为 UTF-8 字节向量
- `utf8->codepoint` : 将 UTF-8 字节向量转换为 Unicode 码点
|#

#|
utf16be->codepoint
将 UTF-16BE 编码的字节向量转换为 Unicode 码点

函数签名
----
(utf16be->codepoint bytevector) → integer

参数
----
bytevector : bytevector
包含 UTF-16BE 编码字节的字节向量

返回值
----
integer
Unicode 码点值

描述
----
`utf16be->codepoint` 用于将 UTF-16BE 编码的字节序列转换为 Unicode 码点。

解码规则
------
- 2 字节序列: 基本多文种平面字符 (U+0000 到 U+FFFF)
- 4 字节序列: 辅助平面字符 (U+10000 到 U+10FFFF)

错误处理
------
- 如果字节向量包含无效的 UTF-16BE 编码序列，会抛出 `value-error` 异常
- 参数必须是字节向量类型，否则会抛出 `type-error` 异常
- 如果字节向量为空，会抛出 `value-error` 异常
- 如果序列不完整，会抛出 `value-error` 异常

实现说明
------
- 函数根据字节序列的第一个码元判断是否为代理对
- 支持与 `codepoint->utf16be` 函数的互逆操作

相关函数
--------
- `codepoint->utf16be` : 将 Unicode 码点转换为 UTF-16BE 字节向量
- `utf8->codepoint` : 将 UTF-8 字节向量转换为 Unicode 码点
- `codepoint->utf8` : 将 Unicode 码点转换为 UTF-8 字节向量
|#

;; codepoint->utf16be ASCII 字符测试 (2字节编码)
(check (codepoint->utf16be #x48) => (bytevector #x00 #x48))  ; "H"
(check (codepoint->utf16be #x65) => (bytevector #x00 #x65))  ; "e"
(check (codepoint->utf16be #x6C) => (bytevector #x00 #x6C))  ; "l"
(check (codepoint->utf16be #x6F) => (bytevector #x00 #x6F))  ; "o"
(check (codepoint->utf16be #x20) => (bytevector #x00 #x20))  ; 空格
(check (codepoint->utf16be #x0A) => (bytevector #x00 #x0A))  ; 换行符

;; codepoint->utf16be 基本多文种平面字符测试 (2字节编码)
(check (codepoint->utf16be #xA4) => (bytevector #x00 #xA4))  ; "¤" (CURRENCY SIGN)
(check (codepoint->utf16be #xE4) => (bytevector #x00 #xE4))  ; "ä"
(check (codepoint->utf16be #xE9) => (bytevector #x00 #xE9))  ; "é"
(check (codepoint->utf16be #xF6) => (bytevector #x00 #xF6))  ; "ö"
(check (codepoint->utf16be #xFC) => (bytevector #x00 #xFC))  ; "ü"

;; codepoint->utf16be 其他 BMP 字符测试 (2字节编码)
(check (codepoint->utf16be #x4E2D) => (bytevector #x4E #x2D))  ; "中"
(check (codepoint->utf16be #x6C49) => (bytevector #x6C #x49))  ; "汉"
(check (codepoint->utf16be #x5B57) => (bytevector #x5B #x57))  ; "字"
(check (codepoint->utf16be #x5199) => (bytevector #x51 #x99))  ; "写"

;; codepoint->utf16be 辅助平面字符测试 (4字节编码)
(check (codepoint->utf16be #x1F44D) => (bytevector #xD8 #x3D #xDC #x4D))  ; "👍"
(check (codepoint->utf16be #x1F680) => (bytevector #xD8 #x3D #xDE #x80))  ; "🚀"
(check (codepoint->utf16be #x1F389) => (bytevector #xD8 #x3C #xDF #x89))  ; "🎉"
(check (codepoint->utf16be #x1F38A) => (bytevector #xD8 #x3C #xDF #x8A))  ; "🎊"

;; codepoint->utf16be 边界值测试
(check (codepoint->utf16be 0) => (bytevector #x00 #x00))  ; 最小码点
(check (codepoint->utf16be 127) => (bytevector #x00 #x7F))  ; ASCII 最大
(check (codepoint->utf16be 128) => (bytevector #x00 #x80))  ; 2字节编码最小
(check (codepoint->utf16be 2047) => (bytevector #x07 #xFF))  ; 2字节编码
(check (codepoint->utf16be 2048) => (bytevector #x08 #x00))  ; 3字节编码最小
(check (codepoint->utf16be 65535) => (bytevector #xFF #xFF))  ; BMP 最大
(check (codepoint->utf16be 65536) => (bytevector #xD8 #x00 #xDC #x00))  ; 4字节编码最小
(check (codepoint->utf16be #x10FFFF) => (bytevector #xDB #xFF #xDF #xFF))  ; Unicode 最大码点

;; codepoint->utf16be 错误处理测试
(check-catch 'value-error (codepoint->utf16be -1))  ; 负码点
(check-catch 'value-error (codepoint->utf16be #x110000))  ; 超出 Unicode 范围
(check-catch 'value-error (codepoint->utf16be #xD800))  ; 代理对范围 - 高代理
(check-catch 'value-error (codepoint->utf16be #xDC00))  ; 代理对范围 - 低代理

;; utf16be->codepoint ASCII 字符测试 (2字节编码)
(check (utf16be->codepoint (bytevector #x00 #x48)) => #x48)  ; "H"
(check (utf16be->codepoint (bytevector #x00 #x65)) => #x65)  ; "e"
(check (utf16be->codepoint (bytevector #x00 #x6C)) => #x6C)  ; "l"
(check (utf16be->codepoint (bytevector #x00 #x6F)) => #x6F)  ; "o"
(check (utf16be->codepoint (bytevector #x00 #x20)) => #x20)  ; 空格
(check (utf16be->codepoint (bytevector #x00 #x0A)) => #x0A)  ; 换行符

;; utf16be->codepoint 基本多文种平面字符测试 (2字节编码)
(check (utf16be->codepoint (bytevector #x00 #xA4)) => #xA4)  ; "¤" (CURRENCY SIGN)
(check (utf16be->codepoint (bytevector #x00 #xE4)) => #xE4)  ; "ä"
(check (utf16be->codepoint (bytevector #x00 #xE9)) => #xE9)  ; "é"
(check (utf16be->codepoint (bytevector #x00 #xF6)) => #xF6)  ; "ö"
(check (utf16be->codepoint (bytevector #x00 #xFC)) => #xFC)  ; "ü"

;; utf16be->codepoint 其他 BMP 字符测试 (2字节编码)
(check (utf16be->codepoint (bytevector #x4E #x2D)) => #x4E2D)  ; "中"
(check (utf16be->codepoint (bytevector #x6C #x49)) => #x6C49)  ; "汉"
(check (utf16be->codepoint (bytevector #x5B #x57)) => #x5B57)  ; "字"
(check (utf16be->codepoint (bytevector #x51 #x99)) => #x5199)  ; "写"

;; utf16be->codepoint 辅助平面字符测试 (4字节编码)
(check (utf16be->codepoint (bytevector #xD8 #x3D #xDC #x4D)) => #x1F44D)  ; "👍"
(check (utf16be->codepoint (bytevector #xD8 #x3D #xDE #x80)) => #x1F680)  ; "🚀"
(check (utf16be->codepoint (bytevector #xD8 #x3C #xDF #x89)) => #x1F389)  ; "🎉"
(check (utf16be->codepoint (bytevector #xD8 #x3C #xDF #x8A)) => #x1F38A)  ; "🎊"

;; utf16be->codepoint 边界值测试
(check (utf16be->codepoint (bytevector #x00 #x00)) => 0)  ; 最小码点
(check (utf16be->codepoint (bytevector #x00 #x7F)) => 127)  ; ASCII 最大
(check (utf16be->codepoint (bytevector #x00 #x80)) => 128)  ; 2字节编码最小
(check (utf16be->codepoint (bytevector #x07 #xFF)) => 2047)  ; 2字节编码
(check (utf16be->codepoint (bytevector #x08 #x00)) => 2048)  ; 3字节编码最小
(check (utf16be->codepoint (bytevector #xFF #xFF)) => 65535)  ; BMP 最大
(check (utf16be->codepoint (bytevector #xD8 #x00 #xDC #x00)) => 65536)  ; 4字节编码最小
(check (utf16be->codepoint (bytevector #xDB #xFF #xDF #xFF)) => #x10FFFF)  ; Unicode 最大码点

;; utf16be->codepoint 错误处理测试
(check-catch 'value-error (utf16be->codepoint #u8()))  ; 空字节向量
(check-catch 'value-error (utf16be->codepoint (bytevector #x00)))  ; 不完整序列
(check-catch 'value-error (utf16be->codepoint (bytevector #xD8 #x3D)))  ; 不完整代理对
(check-catch 'value-error (utf16be->codepoint (bytevector #xDC #x00 #x00 #x00)))  ; 低代理对作为第一个码元
(check-catch 'value-error (utf16be->codepoint (bytevector #xD8 #x3D #x00 #x00)))  ; 无效低代理对

;; codepoint->utf16be 与 utf16be->codepoint 互逆操作验证
(check (utf16be->codepoint (codepoint->utf16be 0)) => 0)
(check (utf16be->codepoint (codepoint->utf16be 127)) => 127)
(check (utf16be->codepoint (codepoint->utf16be 128)) => 128)
(check (utf16be->codepoint (codepoint->utf16be 2047)) => 2047)
(check (utf16be->codepoint (codepoint->utf16be 2048)) => 2048)
(check (utf16be->codepoint (codepoint->utf16be 65535)) => 65535)
(check (utf16be->codepoint (codepoint->utf16be 65536)) => 65536)
(check (utf16be->codepoint (codepoint->utf16be #x10FFFF)) => #x10FFFF)
(check (utf16be->codepoint (codepoint->utf16be #x48)) => #x48)
(check (utf16be->codepoint (codepoint->utf16be #xE4)) => #xE4)
(check (utf16be->codepoint (codepoint->utf16be #x4E2D)) => #x4E2D)
(check (utf16be->codepoint (codepoint->utf16be #x1F44D)) => #x1F44D)

#|
codepoint->utf16le
将 Unicode 码点转换为 UTF-16LE 编码的字节向量

函数签名
----
(codepoint->utf16le codepoint) → bytevector

参数
----
codepoint : integer
Unicode 码点值

返回值
----
bytevector
包含 UTF-16LE 编码字节的字节向量

描述
----
`codepoint->utf16le` 用于将 Unicode 码点转换为 UTF-16LE 编码的字节序列。

编码规则
------
- 基本多文种平面字符 (U+0000 到 U+FFFF): 2 字节编码
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码（代理对）

错误处理
------
- 如果码点超出 Unicode 范围 (0-0x10FFFF)，会抛出 `value-error` 异常
- 如果码点在代理对范围 (U+D800 到 U+DFFF)，会抛出 `value-error` 异常
- 参数必须是整数类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数根据码点大小自动选择合适的 UTF-16LE 编码长度
- 对于辅助平面字符，使用代理对编码
- 返回字节向量便于与其他编码函数保持一致

相关函数
--------
- `utf16le->codepoint` : 将 UTF-16LE 字节向量转换为 Unicode 码点
- `codepoint->utf8` : 将 Unicode 码点转换为 UTF-8 字节向量
- `codepoint->utf16be` : 将 Unicode 码点转换为 UTF-16BE 字节向量
|#

#|
utf16le->codepoint
将 UTF-16LE 编码的字节向量转换为 Unicode 码点

函数签名
----
(utf16le->codepoint bytevector) → integer

参数
----
bytevector : bytevector
包含 UTF-16LE 编码字节的字节向量

返回值
----
integer
Unicode 码点值

描述
----
`utf16le->codepoint` 用于将 UTF-16LE 编码的字节序列转换为 Unicode 码点。

解码规则
------
- 2 字节序列: 基本多文种平面字符 (U+0000 到 U+FFFF)
- 4 字节序列: 辅助平面字符 (U+10000 到 U+10FFFF)

错误处理
------
- 如果字节向量包含无效的 UTF-16LE 编码序列，会抛出 `value-error` 异常
- 参数必须是字节向量类型，否则会抛出 `type-error` 异常
- 如果字节向量为空，会抛出 `value-error` 异常
- 如果序列不完整，会抛出 `value-error` 异常

实现说明
------
- 函数根据字节序列的第一个码元判断是否为代理对
- 支持与 `codepoint->utf16le` 函数的互逆操作

相关函数
--------
- `codepoint->utf16le` : 将 Unicode 码点转换为 UTF-16LE 字节向量
- `utf8->codepoint` : 将 UTF-8 字节向量转换为 Unicode 码点
- `utf16be->codepoint` : 将 UTF-16BE 字节向量转换为 Unicode 码点
|#

;; codepoint->utf16le ASCII 字符测试 (2字节编码)
(check (codepoint->utf16le #x48) => (bytevector #x48 #x00))  ; "H"
(check (codepoint->utf16le #x65) => (bytevector #x65 #x00))  ; "e"
(check (codepoint->utf16le #x6C) => (bytevector #x6C #x00))  ; "l"
(check (codepoint->utf16le #x6F) => (bytevector #x6F #x00))  ; "o"
(check (codepoint->utf16le #x20) => (bytevector #x20 #x00))  ; 空格
(check (codepoint->utf16le #x0A) => (bytevector #x0A #x00))  ; 换行符

;; codepoint->utf16le 基本多文种平面字符测试 (2字节编码)
(check (codepoint->utf16le #xA4) => (bytevector #xA4 #x00))  ; "¤" (CURRENCY SIGN)
(check (codepoint->utf16le #xE4) => (bytevector #xE4 #x00))  ; "ä"
(check (codepoint->utf16le #xE9) => (bytevector #xE9 #x00))  ; "é"
(check (codepoint->utf16le #xF6) => (bytevector #xF6 #x00))  ; "ö"
(check (codepoint->utf16le #xFC) => (bytevector #xFC #x00))  ; "ü"

;; codepoint->utf16le 其他 BMP 字符测试 (2字节编码)
(check (codepoint->utf16le #x4E2D) => (bytevector #x2D #x4E))  ; "中"
(check (codepoint->utf16le #x6C49) => (bytevector #x49 #x6C))  ; "汉"
(check (codepoint->utf16le #x5B57) => (bytevector #x57 #x5B))  ; "字"
(check (codepoint->utf16le #x5199) => (bytevector #x99 #x51))  ; "写"

;; codepoint->utf16le 辅助平面字符测试 (4字节编码)
(check (codepoint->utf16le #x1F44D) => (bytevector #x3D #xD8 #x4D #xDC))  ; "👍"
(check (codepoint->utf16le #x1F680) => (bytevector #x3D #xD8 #x80 #xDE))  ; "🚀"
(check (codepoint->utf16le #x1F389) => (bytevector #x3C #xD8 #x89 #xDF))  ; "🎉"
(check (codepoint->utf16le #x1F38A) => (bytevector #x3C #xD8 #x8A #xDF))  ; "🎊"

;; codepoint->utf16le 边界值测试
(check (codepoint->utf16le 0) => (bytevector #x00 #x00))  ; 最小码点
(check (codepoint->utf16le 127) => (bytevector #x7F #x00))  ; ASCII 最大
(check (codepoint->utf16le 128) => (bytevector #x80 #x00))  ; 2字节编码最小
(check (codepoint->utf16le 2047) => (bytevector #xFF #x07))  ; 2字节编码
(check (codepoint->utf16le 2048) => (bytevector #x00 #x08))  ; 3字节编码最小
(check (codepoint->utf16le 65535) => (bytevector #xFF #xFF))  ; BMP 最大
(check (codepoint->utf16le 65536) => (bytevector #x00 #xD8 #x00 #xDC))  ; 4字节编码最小
(check (codepoint->utf16le #x10FFFF) => (bytevector #xFF #xDB #xFF #xDF))  ; Unicode 最大码点

;; codepoint->utf16le 错误处理测试
(check-catch 'value-error (codepoint->utf16le -1))  ; 负码点
(check-catch 'value-error (codepoint->utf16le #x110000))  ; 超出 Unicode 范围
(check-catch 'value-error (codepoint->utf16le #xD800))  ; 代理对范围 - 高代理
(check-catch 'value-error (codepoint->utf16le #xDC00))  ; 代理对范围 - 低代理

;; utf16le->codepoint ASCII 字符测试 (2字节编码)
(check (utf16le->codepoint (bytevector #x48 #x00)) => #x48)  ; "H"
(check (utf16le->codepoint (bytevector #x65 #x00)) => #x65)  ; "e"
(check (utf16le->codepoint (bytevector #x6C #x00)) => #x6C)  ; "l"
(check (utf16le->codepoint (bytevector #x6F #x00)) => #x6F)  ; "o"
(check (utf16le->codepoint (bytevector #x20 #x00)) => #x20)  ; 空格
(check (utf16le->codepoint (bytevector #x0A #x00)) => #x0A)  ; 换行符

;; utf16le->codepoint 基本多文种平面字符测试 (2字节编码)
(check (utf16le->codepoint (bytevector #xA4 #x00)) => #xA4)  ; "¤" (CURRENCY SIGN)
(check (utf16le->codepoint (bytevector #xE4 #x00)) => #xE4)  ; "ä"
(check (utf16le->codepoint (bytevector #xE9 #x00)) => #xE9)  ; "é"
(check (utf16le->codepoint (bytevector #xF6 #x00)) => #xF6)  ; "ö"
(check (utf16le->codepoint (bytevector #xFC #x00)) => #xFC)  ; "ü"

;; utf16le->codepoint 其他 BMP 字符测试 (2字节编码)
(check (utf16le->codepoint (bytevector #x2D #x4E)) => #x4E2D)  ; "中"
(check (utf16le->codepoint (bytevector #x49 #x6C)) => #x6C49)  ; "汉"
(check (utf16le->codepoint (bytevector #x57 #x5B)) => #x5B57)  ; "字"
(check (utf16le->codepoint (bytevector #x99 #x51)) => #x5199)  ; "写"

;; utf16le->codepoint 辅助平面字符测试 (4字节编码)
(check (utf16le->codepoint (bytevector #x3D #xD8 #x4D #xDC)) => #x1F44D)  ; "👍"
(check (utf16le->codepoint (bytevector #x3D #xD8 #x80 #xDE)) => #x1F680)  ; "🚀"
(check (utf16le->codepoint (bytevector #x3C #xD8 #x89 #xDF)) => #x1F389)  ; "🎉"
(check (utf16le->codepoint (bytevector #x3C #xD8 #x8A #xDF)) => #x1F38A)  ; "🎊"

;; utf16le->codepoint 边界值测试
(check (utf16le->codepoint (bytevector #x00 #x00)) => 0)  ; 最小码点
(check (utf16le->codepoint (bytevector #x7F #x00)) => 127)  ; ASCII 最大
(check (utf16le->codepoint (bytevector #x80 #x00)) => 128)  ; 2字节编码最小
(check (utf16le->codepoint (bytevector #xFF #x07)) => 2047)  ; 2字节编码
(check (utf16le->codepoint (bytevector #x00 #x08)) => 2048)  ; 3字节编码最小
(check (utf16le->codepoint (bytevector #xFF #xFF)) => 65535)  ; BMP 最大
(check (utf16le->codepoint (bytevector #x00 #xD8 #x00 #xDC)) => 65536)  ; 4字节编码最小
(check (utf16le->codepoint (bytevector #xFF #xDB #xFF #xDF)) => #x10FFFF)  ; Unicode 最大码点

;; utf16le->codepoint 错误处理测试
(check-catch 'value-error (utf16le->codepoint #u8()))  ; 空字节向量
(check-catch 'value-error (utf16le->codepoint (bytevector #x00)))  ; 不完整序列
(check-catch 'value-error (utf16le->codepoint (bytevector #x3D #xD8)))  ; 不完整代理对
(check-catch 'value-error (utf16le->codepoint (bytevector #x00 #xDC #x00 #x00)))  ; 低代理对作为第一个码元
(check-catch 'value-error (utf16le->codepoint (bytevector #x3D #xD8 #x00 #x00)))  ; 无效低代理对

;; codepoint->utf16le 与 utf16le->codepoint 互逆操作验证
(check (utf16le->codepoint (codepoint->utf16le 0)) => 0)
(check (utf16le->codepoint (codepoint->utf16le 127)) => 127)
(check (utf16le->codepoint (codepoint->utf16le 128)) => 128)
(check (utf16le->codepoint (codepoint->utf16le 2047)) => 2047)
(check (utf16le->codepoint (codepoint->utf16le 2048)) => 2048)
(check (utf16le->codepoint (codepoint->utf16le 65535)) => 65535)
(check (utf16le->codepoint (codepoint->utf16le 65536)) => 65536)
(check (utf16le->codepoint (codepoint->utf16le #x10FFFF)) => #x10FFFF)
(check (utf16le->codepoint (codepoint->utf16le #x48)) => #x48)
(check (utf16le->codepoint (codepoint->utf16le #xE4)) => #xE4)
(check (utf16le->codepoint (codepoint->utf16le #x4E2D)) => #x4E2D)
(check (utf16le->codepoint (codepoint->utf16le #x1F44D)) => #x1F44D)

#|
bytevector-advance-utf8
在 UTF-8 编码的字节向量中前进到下一个字符的起始位置

函数签名
----
(bytevector-advance-utf8 bytevector index [end]) → integer

参数
----
bytevector : bytevector
UTF-8 编码的字节向量

index : integer
当前字节位置

end : integer (可选，默认字节向量长度)
字节向量的结束位置

返回值
----
integer
下一个 UTF-8 字符的起始字节位置，或者当前位置（如果遇到无效序列）

描述
----
`bytevector-advance-utf8` 用于在 UTF-8 编码的字节向量中前进到下一个字符的起始位置。
该函数能够识别完整的 UTF-8 字符序列，并跳过无效或不完整的序列。

行为特征
------
- 对于有效的 UTF-8 序列，返回下一个字符的起始位置
- 对于无效的 UTF-8 序列，返回当前位置（不前进）
- 对于不完整的序列，返回当前位置（不前进）
- 支持所有有效的 Unicode 字符编码
- 正确处理边界条件（起始位置、结束位置等）

UTF-8 编码规则
------
- ASCII 字符 (U+0000 到 U+007F): 1 字节编码 (0xxxxxxx)
- 基本多文种平面字符 (U+0080 到 U+07FF): 2 字节编码 (110xxxxx 10xxxxxx)
- 其他 BMP 字符 (U+0800 到 U+FFFF): 3 字节编码 (1110xxxx 10xxxxxx 10xxxxxx)
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码 (11110xxx 10xxxxxx 10xxxxxx 10xxxxxx)

返回值说明
------
- 如果当前位置已经是结束位置，返回当前位置
- 如果遇到有效的 UTF-8 序列，返回下一个字符的起始位置
- 如果遇到无效的 UTF-8 序列，返回当前位置
- 如果遇到不完整的序列（字节不足），返回当前位置

实现说明
------
- 函数在 (scheme base) 库中定义，在 (liii base) 和 (liii unicode) 库中重新导出
- 被 `utf8-string-length`、`utf8->string`、`string->utf8` 等函数内部使用
- 提供 UTF-8 序列验证功能

相关函数
--------
- `utf8-string-length` : 获取字符串的 Unicode 字符数量
- `utf8->string` : 将 UTF-8 字节向量转换为字符串
- `string->utf8` : 将字符串转换为 UTF-8 字节向量
- `u8-substring` : 基于 Unicode 字符位置提取子字符串
|#

;; bytevector-advance-utf8 ASCII 字符测试 (1字节编码)
(check (bytevector-advance-utf8 (bytevector #x48 #x65 #x6C #x6C #x6F) 0) => 1)  ; "H" -> "e"
(check (bytevector-advance-utf8 (bytevector #x48 #x65 #x6C #x6C #x6F) 1) => 2)  ; "e" -> "l"
(check (bytevector-advance-utf8 (bytevector #x48 #x65 #x6C #x6C #x6F) 2) => 3)  ; "l" -> "l"
(check (bytevector-advance-utf8 (bytevector #x48 #x65 #x6C #x6C #x6F) 3) => 4)  ; "l" -> "o"
(check (bytevector-advance-utf8 (bytevector #x48 #x65 #x6C #x6C #x6F) 4) => 5)  ; "o" -> 结束

;; bytevector-advance-utf8 基本多文种平面字符测试 (2字节编码)
(check (bytevector-advance-utf8 #u8(#xC3 #xA4 #x48) 0) => 2)  ; "ä" -> "H"
(check (bytevector-advance-utf8 #u8(#xC3 #xA9 #x65) 0) => 2)  ; "é" -> "e"
(check (bytevector-advance-utf8 #u8(#xC3 #xB6 #x6C) 0) => 2)  ; "ö" -> "l"

;; bytevector-advance-utf8 其他 BMP 字符测试 (3字节编码)
(check (bytevector-advance-utf8 #u8(#xE4 #xB8 #xAD #x48) 0) => 3)  ; "中" -> "H"
(check (bytevector-advance-utf8 #u8(#xE6 #xB1 #x89 #x65) 0) => 3)  ; "汉" -> "e"
(check (bytevector-advance-utf8 #u8(#xE5 #xAD #x97 #x6C) 0) => 3)  ; "字" -> "l"

;; bytevector-advance-utf8 辅助平面字符测试 (4字节编码)
(check (bytevector-advance-utf8 #u8(#xF0 #x9F #x91 #x8D #x48) 0) => 4)  ; "👍" -> "H"
(check (bytevector-advance-utf8 #u8(#xF0 #x9F #x9A #x80 #x65) 0) => 4)  ; "🚀" -> "e"
(check (bytevector-advance-utf8 #u8(#xF0 #x9F #x8E #x89 #x6C) 0) => 4)  ; "🎉" -> "l"

;; bytevector-advance-utf8 混合字符序列测试
(check (bytevector-advance-utf8 #u8(#x48 #xC3 #xA4 #xE4 #xB8 #xAD #xF0 #x9F #x91 #x8D) 0) => 1)  ; "H" -> "ä"
(check (bytevector-advance-utf8 #u8(#x48 #xC3 #xA4 #xE4 #xB8 #xAD #xF0 #x9F #x91 #x8D) 1) => 3)  ; "ä" -> "中"
(check (bytevector-advance-utf8 #u8(#x48 #xC3 #xA4 #xE4 #xB8 #xAD #xF0 #x9F #x91 #x8D) 3) => 6)  ; "中" -> "👍"
(check (bytevector-advance-utf8 #u8(#x48 #xC3 #xA4 #xE4 #xB8 #xAD #xF0 #x9F #x91 #x8D) 6) => 10)  ; "👍" -> 结束

;; bytevector-advance-utf8 边界条件测试
(check (bytevector-advance-utf8 #u8() 0) => 0)  ; 空字节向量
(check (bytevector-advance-utf8 (bytevector #x48) 0) => 1)  ; 单字节字符
(check (bytevector-advance-utf8 (bytevector #x48) 1) => 1)  ; 结束位置

;; bytevector-advance-utf8 无效 UTF-8 序列测试
(check (bytevector-advance-utf8 (bytevector #x80) 0) => 0)  ; 无效起始字节
(check (bytevector-advance-utf8 (bytevector #xC2) 0) => 0)  ; 不完整的2字节序列
(check (bytevector-advance-utf8 (bytevector #xE4 #xB8) 0) => 0)  ; 不完整的3字节序列
(check (bytevector-advance-utf8 (bytevector #xF0 #x9F #x91) 0) => 0)  ; 不完整的4字节序列
(check (bytevector-advance-utf8 (bytevector #xFF) 0) => 0)  ; 无效字节

;; bytevector-advance-utf8 无效延续字节测试
(check (bytevector-advance-utf8 (bytevector #xC2 #x00) 0) => 0)  ; 无效延续字节
(check (bytevector-advance-utf8 (bytevector #xE4 #x00 #xAD) 0) => 0)  ; 无效延续字节
(check (bytevector-advance-utf8 (bytevector #xF0 #x9F #x00 #x8D) 0) => 0)  ; 无效延续字节

;; bytevector-advance-utf8 结束位置参数测试
(check (bytevector-advance-utf8 #u8(#x48 #x65 #x6C #x6C #x6F) 0 1) => 1)  ; "H" -> 结束
(check (bytevector-advance-utf8 #u8(#x48 #x65 #x6C #x6C #x6F) 0 2) => 1)  ; "H" -> "e"
(check (bytevector-advance-utf8 #u8(#xC3 #xA4 #x48) 0 2) => 2)  ; "ä" -> 结束
(check (bytevector-advance-utf8 #u8(#xC3 #xA4 #x48) 0 3) => 2)  ; "ä" -> "H"

;; bytevector-advance-utf8 在有效序列中的位置测试
(check (bytevector-advance-utf8 #u8(#x48 #x65 #x6C #x6C #x6F) 0) => 1)
(check (bytevector-advance-utf8 #u8(#x48 #x65 #x6C #x6C #x6F) 1) => 2)
(check (bytevector-advance-utf8 #u8(#x48 #x65 #x6C #x6C #x6F) 2) => 3)
(check (bytevector-advance-utf8 #u8(#x48 #x65 #x6C #x6C #x6F) 3) => 4)
(check (bytevector-advance-utf8 #u8(#x48 #x65 #x6C #x6C #x6F) 4) => 5)

#|
utf8->utf16le
将 UTF-8 编码的字节向量转换为 UTF-16LE 编码的字节向量

函数签名
----
(utf8->utf16le bytevector) → bytevector

参数
----
bytevector : bytevector
包含 UTF-8 编码字节的字节向量

返回值
----
bytevector
包含 UTF-16LE 编码字节的字节向量

描述
----
`utf8->utf16le` 用于将 UTF-8 编码的字节向量转换为 UTF-16LE 编码的字节向量。

转换规则
------
- 遍历 UTF-8 字节向量中的每个 Unicode 字符
- 使用 `utf8->codepoint` 将 UTF-8 字节转换为 Unicode 码点
- 使用 `codepoint->utf16le` 将码点转换为 UTF-16LE 字节向量
- 将转换后的字节向量拼接起来形成最终结果

错误处理
------
- 如果字节向量包含无效的 UTF-8 编码序列，会抛出 `value-error` 异常
- 参数必须是字节向量类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数使用 `bytevector-advance-utf8` 遍历 UTF-8 字节向量
- 支持所有有效的 Unicode 字符，包括基本多文种平面和辅助平面字符
- 正确处理代理对编码

相关函数
--------
- `codepoint->utf16le` : 将 Unicode 码点转换为 UTF-16LE 字节向量
- `utf8->codepoint` : 将 UTF-8 字节向量转换为 Unicode 码点
- `bytevector-advance-utf8` : 在 UTF-8 字节向量中前进到下一个字符
|#

;; utf8->utf16le ASCII 字符测试
(check (utf8->utf16le (bytevector #x48 #x65 #x6C #x6C #x6F)) => (bytevector #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F #x00))  ; "Hello"
(check (utf8->utf16le (bytevector #x20)) => (bytevector #x20 #x00))  ; 空格

;; utf8->utf16le 基本多文种平面字符测试
(check (utf8->utf16le #u8(#xC3 #xA4)) => (bytevector #xE4 #x00))  ; "ä"
(check (utf8->utf16le #u8(#xC3 #xA9)) => (bytevector #xE9 #x00))  ; "é"

;; utf8->utf16le 其他 BMP 字符测试
(check (utf8->utf16le #u8(#xE4 #xB8 #xAD)) => (bytevector #x2D #x4E))  ; "中"
(check (utf8->utf16le #u8(#xE6 #xB1 #x89)) => (bytevector #x49 #x6C))  ; "汉"

;; utf8->utf16le 辅助平面字符测试
(check (utf8->utf16le #u8(#xF0 #x9F #x91 #x8D)) => (bytevector #x3D #xD8 #x4D #xDC))  ; "👍"
(check (utf8->utf16le #u8(#xF0 #x9F #x9A #x80)) => (bytevector #x3D #xD8 #x80 #xDE))  ; "🚀"

;; utf8->utf16le 混合字符测试
(check (utf8->utf16le #u8(#x48 #xC3 #xA4 #xE4 #xB8 #xAD #xF0 #x9F #x91 #x8D)) => (bytevector #x48 #x00 #xE4 #x00 #x2D #x4E #x3D #xD8 #x4D #xDC))  ; "Hä中👍"

;; utf8->utf16le 边界条件测试
(check (utf8->utf16le #u8()) => #u8())  ; 空字节向量
(check (utf8->utf16le (bytevector #x48)) => (bytevector #x48 #x00))  ; 单字节字符

;; utf8->utf16le 错误处理测试
(check-catch 'value-error (utf8->utf16le (bytevector #x80)))  ; 无效 UTF-8 序列
(check-catch 'value-error (utf8->utf16le (bytevector #xC2)))  ; 不完整的 UTF-8 序列

#|
bytevector-utf16le-advance
在 UTF-16LE 编码的字节向量中前进到下一个字符的起始位置

函数签名
----
(bytevector-utf16le-advance bytevector index [end]) → integer

参数
----
bytevector : bytevector
UTF-16LE 编码的字节向量

index : integer
当前字节位置

end : integer (可选，默认字节向量长度)
字节向量的结束位置

返回值
----
integer
下一个 UTF-16LE 字符的起始字节位置，或者当前位置（如果遇到无效序列）

描述
----
`bytevector-utf16le-advance` 用于在 UTF-16LE 编码的字节向量中前进到下一个字符的起始位置。
该函数能够识别完整的 UTF-16LE 字符序列，包括代理对编码，并跳过无效或不完整的序列。

行为特征
------
- 对于基本多文种平面字符，前进 2 个字节
- 对于辅助平面字符（代理对），前进 4 个字节
- 对于无效的 UTF-16LE 序列，返回当前位置（不前进）
- 对于不完整的序列（字节不足），返回当前位置（不前进）
- 支持所有有效的 Unicode 字符编码
- 正确处理边界条件（起始位置、结束位置等）

UTF-16LE 编码规则
------
- 基本多文种平面字符 (U+0000 到 U+FFFF): 2 字节编码
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码（代理对）
  - 高代理对: 0xD800-0xDBFF
  - 低代理对: 0xDC00-0xDFFF

返回值说明
------
- 如果当前位置已经是结束位置，返回当前位置
- 如果遇到有效的 UTF-16LE 序列，返回下一个字符的起始位置
- 如果遇到无效的 UTF-16LE 序列，返回当前位置
- 如果遇到不完整的序列（字节不足），返回当前位置

实现说明
------
- 函数在 (liii unicode) 库中定义
- 与 `bytevector-advance-utf8` 函数形成对称设计
- 提供 UTF-16LE 序列验证功能

相关函数
--------
- `codepoint->utf16le` : 将 Unicode 码点转换为 UTF-16LE 字节向量
- `utf16le->codepoint` : 将 UTF-16LE 字节向量转换为 Unicode 码点
- `utf8->utf16le` : 将 UTF-8 字节向量转换为 UTF-16LE 字节向量
- `bytevector-advance-utf8` : 在 UTF-8 字节向量中前进到下一个字符
|#

;; bytevector-utf16le-advance ASCII 字符测试 (2字节编码)
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F #x00) 0) => 2)  ; "H" -> "e"
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F #x00) 2) => 4)  ; "e" -> "l"
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F #x00) 4) => 6)  ; "l" -> "l"
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F #x00) 6) => 8)  ; "l" -> "o"
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F #x00) 8) => 10)  ; "o" -> 结束

;; bytevector-utf16le-advance 基本多文种平面字符测试 (2字节编码)
(check (bytevector-utf16le-advance (bytevector #xE4 #x00 #x48 #x00) 0) => 2)  ; "ä" -> "H"
(check (bytevector-utf16le-advance (bytevector #xE9 #x00 #x65 #x00) 0) => 2)  ; "é" -> "e"
(check (bytevector-utf16le-advance (bytevector #x2D #x4E #x48 #x00) 0) => 2)  ; "中" -> "H"

;; bytevector-utf16le-advance 辅助平面字符测试 (4字节编码)
(check (bytevector-utf16le-advance (bytevector #x3D #xD8 #x4D #xDC #x48 #x00) 0) => 4)  ; "👍" -> "H"
(check (bytevector-utf16le-advance (bytevector #x3D #xD8 #x80 #xDE #x65 #x00) 0) => 4)  ; "🚀" -> "e"
(check (bytevector-utf16le-advance (bytevector #x3C #xD8 #x89 #xDF #x6C #x00) 0) => 4)  ; "🎉" -> "l"

;; bytevector-utf16le-advance 混合字符序列测试
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #xE4 #x00 #x2D #x4E #x3D #xD8 #x4D #xDC) 0) => 2)  ; "H" -> "ä"
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #xE4 #x00 #x2D #x4E #x3D #xD8 #x4D #xDC) 2) => 4)  ; "ä" -> "中"
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #xE4 #x00 #x2D #x4E #x3D #xD8 #x4D #xDC) 4) => 6)  ; "中" -> "👍"
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #xE4 #x00 #x2D #x4E #x3D #xD8 #x4D #xDC) 6) => 10)  ; "👍" -> 结束

;; bytevector-utf16le-advance 边界条件测试
(check (bytevector-utf16le-advance #u8() 0) => 0)  ; 空字节向量
(check (bytevector-utf16le-advance (bytevector #x48 #x00) 0) => 2)  ; 单字符
(check (bytevector-utf16le-advance (bytevector #x48 #x00) 2) => 2)  ; 结束位置

;; bytevector-utf16le-advance 不完整序列测试
(check (bytevector-utf16le-advance (bytevector #x48) 0) => 0)  ; 不完整序列（只有1字节）
(check (bytevector-utf16le-advance (bytevector #x3D #xD8) 0) => 0)  ; 不完整代理对（只有高代理）
(check (bytevector-utf16le-advance (bytevector #x3D #xD8 #x4D) 0) => 0)  ; 不完整代理对（缺少低代理字节）

;; bytevector-utf16le-advance 无效序列测试
(check (bytevector-utf16le-advance (bytevector #x00 #xDC #x00 #x00) 0) => 0)  ; 低代理对作为第一个码元
(check (bytevector-utf16le-advance (bytevector #x3D #xD8 #x00 #x00) 0) => 0)  ; 无效低代理对

;; bytevector-utf16le-advance 结束位置参数测试
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #x65 #x00 #x6C #x00) 0 2) => 2)  ; "H" -> 结束
(check (bytevector-utf16le-advance (bytevector #x48 #x00 #x65 #x00 #x6C #x00) 0 4) => 2)  ; "H" -> "e"
(check (bytevector-utf16le-advance (bytevector #xE4 #x00 #x48 #x00) 0 2) => 2)  ; "ä" -> 结束
(check (bytevector-utf16le-advance (bytevector #xE4 #x00 #x48 #x00) 0 4) => 2)  ; "ä" -> "H"

#|
utf16le->utf8
将 UTF-16LE 编码的字节向量转换为 UTF-8 编码的字节向量

函数签名
----
(utf16le->utf8 bytevector) → bytevector

参数
----
bytevector : bytevector
包含 UTF-16LE 编码字节的字节向量

返回值
----
bytevector
包含 UTF-8 编码字节的字节向量

描述
----
`utf16le->utf8` 用于将 UTF-16LE 编码的字节向量转换为 UTF-8 编码的字节向量。

转换规则
------
- 遍历 UTF-16LE 字节向量中的每个 Unicode 字符
- 使用 `utf16le->codepoint` 将 UTF-16LE 字节转换为 Unicode 码点
- 使用 `codepoint->utf8` 将码点转换为 UTF-8 字节向量
- 将转换后的字节向量拼接起来形成最终结果

错误处理
------
- 如果字节向量包含无效的 UTF-16LE 编码序列，会抛出 `value-error` 异常
- 参数必须是字节向量类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数使用 `bytevector-utf16le-advance` 遍历 UTF-16LE 字节向量
- 支持所有有效的 Unicode 字符，包括基本多文种平面和辅助平面字符
- 正确处理代理对编码

相关函数
--------
- `utf16le->codepoint` : 将 UTF-16LE 字节向量转换为 Unicode 码点
- `codepoint->utf8` : 将 Unicode 码点转换为 UTF-8 字节向量
- `utf8->utf16le` : 将 UTF-8 字节向量转换为 UTF-16LE 字节向量
- `bytevector-utf16le-advance` : 在 UTF-16LE 字节向量中前进到下一个字符
|#

;; utf16le->utf8 ASCII 字符测试
(check (utf16le->utf8 (bytevector #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F #x00)) => (bytevector #x48 #x65 #x6C #x6C #x6F))  ; "Hello"
(check (utf16le->utf8 (bytevector #x20 #x00)) => (bytevector #x20))  ; 空格

;; utf16le->utf8 基本多文种平面字符测试
(check (utf16le->utf8 (bytevector #xE4 #x00)) => #u8(#xC3 #xA4))  ; "ä"
(check (utf16le->utf8 (bytevector #xE9 #x00)) => #u8(#xC3 #xA9))  ; "é"

;; utf16le->utf8 其他 BMP 字符测试
(check (utf16le->utf8 (bytevector #x2D #x4E)) => #u8(#xE4 #xB8 #xAD))  ; "中"
(check (utf16le->utf8 (bytevector #x49 #x6C)) => #u8(#xE6 #xB1 #x89))  ; "汉"

;; utf16le->utf8 辅助平面字符测试
(check (utf16le->utf8 (bytevector #x3D #xD8 #x4D #xDC)) => #u8(#xF0 #x9F #x91 #x8D))  ; "👍"
(check (utf16le->utf8 (bytevector #x3D #xD8 #x80 #xDE)) => #u8(#xF0 #x9F #x9A #x80))  ; "🚀"

;; utf16le->utf8 混合字符测试
(check (utf16le->utf8 (bytevector #x48 #x00 #xE4 #x00 #x2D #x4E #x3D #xD8 #x4D #xDC)) => #u8(#x48 #xC3 #xA4 #xE4 #xB8 #xAD #xF0 #x9F #x91 #x8D))  ; "Hä中👍"

;; utf16le->utf8 边界条件测试
(check (utf16le->utf8 #u8()) => #u8())  ; 空字节向量
(check (utf16le->utf8 (bytevector #x48 #x00)) => (bytevector #x48))  ; 单字符

;; utf16le->utf8 错误处理测试
(check-catch 'value-error (utf16le->utf8 (bytevector #x48)))  ; 不完整的 UTF-16LE 序列
(check-catch 'value-error (utf16le->utf8 (bytevector #x00 #xDC #x00 #x00)))  ; 无效 UTF-16LE 序列

;; utf16le->utf8 与 utf8->utf16le 互逆操作验证
(check (utf16le->utf8 (utf8->utf16le (bytevector #x48 #x65 #x6C #x6C #x6F))) => (bytevector #x48 #x65 #x6C #x6C #x6F))  ; "Hello"
(check (utf16le->utf8 (utf8->utf16le #u8(#xC3 #xA4))) => #u8(#xC3 #xA4))  ; "ä"
(check (utf16le->utf8 (utf8->utf16le #u8(#xE4 #xB8 #xAD))) => #u8(#xE4 #xB8 #xAD))  ; "中"
(check (utf16le->utf8 (utf8->utf16le #u8(#xF0 #x9F #x91 #x8D))) => #u8(#xF0 #x9F #x91 #x8D))  ; "👍"

#|
utf8->utf16be
将 UTF-8 编码的字节向量转换为 UTF-16BE 编码的字节向量

函数签名
----
(utf8->utf16be bytevector) → bytevector

参数
----
bytevector : bytevector
包含 UTF-8 编码字节的字节向量

返回值
----
bytevector
包含 UTF-16BE 编码字节的字节向量

描述
----
`utf8->utf16be` 用于将 UTF-8 编码的字节向量转换为 UTF-16BE 编码的字节向量。

转换规则
------
- 遍历 UTF-8 字节向量中的每个 Unicode 字符
- 使用 `utf8->codepoint` 将 UTF-8 字节转换为 Unicode 码点
- 使用 `codepoint->utf16be` 将码点转换为 UTF-16BE 字节向量
- 将转换后的字节向量拼接起来形成最终结果

错误处理
------
- 如果字节向量包含无效的 UTF-8 编码序列，会抛出 `value-error` 异常
- 参数必须是字节向量类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数使用 `bytevector-advance-utf8` 遍历 UTF-8 字节向量
- 支持所有有效的 Unicode 字符，包括基本多文种平面和辅助平面字符
- 正确处理代理对编码

相关函数
--------
- `codepoint->utf16be` : 将 Unicode 码点转换为 UTF-16BE 字节向量
- `utf8->codepoint` : 将 UTF-8 字节向量转换为 Unicode 码点
- `bytevector-advance-utf8` : 在 UTF-8 字节向量中前进到下一个字符
|#

;; utf8->utf16be ASCII 字符测试
(check (utf8->utf16be (bytevector #x48 #x65 #x6C #x6C #x6F)) => (bytevector #x00 #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F))  ; "Hello"
(check (utf8->utf16be (bytevector #x20)) => (bytevector #x00 #x20))  ; 空格

;; utf8->utf16be 基本多文种平面字符测试
(check (utf8->utf16be #u8(#xC3 #xA4)) => (bytevector #x00 #xE4))  ; "ä"
(check (utf8->utf16be #u8(#xC3 #xA9)) => (bytevector #x00 #xE9))  ; "é"

;; utf8->utf16be 其他 BMP 字符测试
(check (utf8->utf16be #u8(#xE4 #xB8 #xAD)) => (bytevector #x4E #x2D))  ; "中"
(check (utf8->utf16be #u8(#xE6 #xB1 #x89)) => (bytevector #x6C #x49))  ; "汉"

;; utf8->utf16be 辅助平面字符测试
(check (utf8->utf16be #u8(#xF0 #x9F #x91 #x8D)) => (bytevector #xD8 #x3D #xDC #x4D))  ; "👍"
(check (utf8->utf16be #u8(#xF0 #x9F #x9A #x80)) => (bytevector #xD8 #x3D #xDE #x80))  ; "🚀"

;; utf8->utf16be 混合字符测试
(check (utf8->utf16be #u8(#x48 #xC3 #xA4 #xE4 #xB8 #xAD #xF0 #x9F #x91 #x8D)) => (bytevector #x00 #x48 #x00 #xE4 #x4E #x2D #xD8 #x3D #xDC #x4D))  ; "Hä中👍"

;; utf8->utf16be 边界条件测试
(check (utf8->utf16be #u8()) => #u8())  ; 空字节向量
(check (utf8->utf16be (bytevector #x48)) => (bytevector #x00 #x48))  ; 单字节字符

;; utf8->utf16be 错误处理测试
(check-catch 'value-error (utf8->utf16be (bytevector #x80)))  ; 无效 UTF-8 序列
(check-catch 'value-error (utf8->utf16be (bytevector #xC2)))  ; 不完整的 UTF-8 序列

;; utf8->utf16be 与 utf16be->codepoint 互逆操作验证
(check (utf16be->codepoint (utf8->utf16be (bytevector #x48))) => #x48)  ; "H"
(check (utf16be->codepoint (utf8->utf16be #u8(#xC3 #xA4))) => #xE4)  ; "ä"
(check (utf16be->codepoint (utf8->utf16be #u8(#xE4 #xB8 #xAD))) => #x4E2D)  ; "中"
(check (utf16be->codepoint (utf8->utf16be #u8(#xF0 #x9F #x91 #x8D))) => #x1F44D)  ; "👍"

#|
bytevector-utf16be-advance
在 UTF-16BE 编码的字节向量中前进到下一个字符的起始位置

函数签名
----
(bytevector-utf16be-advance bytevector index [end]) → integer

参数
----
bytevector : bytevector
UTF-16BE 编码的字节向量

index : integer
当前字节位置

end : integer (可选，默认字节向量长度)
字节向量的结束位置

返回值
----
integer
下一个 UTF-16BE 字符的起始字节位置，或者当前位置（如果遇到无效序列）

描述
----
`bytevector-utf16be-advance` 用于在 UTF-16BE 编码的字节向量中前进到下一个字符的起始位置。
该函数能够识别完整的 UTF-16BE 字符序列，包括代理对编码，并跳过无效或不完整的序列。

行为特征
------
- 对于基本多文种平面字符，前进 2 个字节
- 对于辅助平面字符（代理对），前进 4 个字节
- 对于无效的 UTF-16BE 序列，返回当前位置（不前进）
- 对于不完整的序列（字节不足），返回当前位置（不前进）
- 支持所有有效的 Unicode 字符编码
- 正确处理边界条件（起始位置、结束位置等）

UTF-16BE 编码规则
------
- 基本多文种平面字符 (U+0000 到 U+FFFF): 2 字节编码
- 辅助平面字符 (U+10000 到 U+10FFFF): 4 字节编码（代理对）
  - 高代理对: 0xD800-0xDBFF
  - 低代理对: 0xDC00-0xDFFF

返回值说明
------
- 如果当前位置已经是结束位置，返回当前位置
- 如果遇到有效的 UTF-16BE 序列，返回下一个字符的起始位置
- 如果遇到无效的 UTF-16BE 序列，返回当前位置
- 如果遇到不完整的序列（字节不足），返回当前位置

实现说明
------
- 函数在 (liii unicode) 库中定义
- 与 `bytevector-advance-utf8` 和 `bytevector-utf16le-advance` 函数形成对称设计
- 提供 UTF-16BE 序列验证功能

相关函数
--------
- `codepoint->utf16be` : 将 Unicode 码点转换为 UTF-16BE 字节向量
- `utf16be->codepoint` : 将 UTF-16BE 字节向量转换为 Unicode 码点
- `utf8->utf16be` : 将 UTF-8 字节向量转换为 UTF-16BE 字节向量
- `bytevector-advance-utf8` : 在 UTF-8 字节向量中前进到下一个字符
- `bytevector-utf16le-advance` : 在 UTF-16LE 字节向量中前进到下一个字符
|#

;; bytevector-utf16be-advance ASCII 字符测试 (2字节编码)
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F) 0) => 2)  ; "H" -> "e"
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F) 2) => 4)  ; "e" -> "l"
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F) 4) => 6)  ; "l" -> "l"
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F) 6) => 8)  ; "l" -> "o"
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F) 8) => 10)  ; "o" -> 结束

;; bytevector-utf16be-advance 基本多文种平面字符测试 (2字节编码)
(check (bytevector-utf16be-advance (bytevector #x00 #xE4 #x00 #x48) 0) => 2)  ; "ä" -> "H"
(check (bytevector-utf16be-advance (bytevector #x00 #xE9 #x00 #x65) 0) => 2)  ; "é" -> "e"
(check (bytevector-utf16be-advance (bytevector #x4E #x2D #x00 #x48) 0) => 2)  ; "中" -> "H"

;; bytevector-utf16be-advance 辅助平面字符测试 (4字节编码)
(check (bytevector-utf16be-advance (bytevector #xD8 #x3D #xDC #x4D #x00 #x48) 0) => 4)  ; "👍" -> "H"
(check (bytevector-utf16be-advance (bytevector #xD8 #x3D #xDE #x80 #x00 #x65) 0) => 4)  ; "🚀" -> "e"
(check (bytevector-utf16be-advance (bytevector #xD8 #x3C #xDF #x89 #x00 #x6C) 0) => 4)  ; "🎉" -> "l"

;; bytevector-utf16be-advance 混合字符序列测试
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #xE4 #x4E #x2D #xD8 #x3D #xDC #x4D) 0) => 2)  ; "H" -> "ä"
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #xE4 #x4E #x2D #xD8 #x3D #xDC #x4D) 2) => 4)  ; "ä" -> "中"
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #xE4 #x4E #x2D #xD8 #x3D #xDC #x4D) 4) => 6)  ; "中" -> "👍"
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #xE4 #x4E #x2D #xD8 #x3D #xDC #x4D) 6) => 10)  ; "👍" -> 结束

;; bytevector-utf16be-advance 边界条件测试
(check (bytevector-utf16be-advance #u8() 0) => 0)  ; 空字节向量
(check (bytevector-utf16be-advance (bytevector #x00 #x48) 0) => 2)  ; 单字符
(check (bytevector-utf16be-advance (bytevector #x00 #x48) 2) => 2)  ; 结束位置

;; bytevector-utf16be-advance 不完整序列测试
(check (bytevector-utf16be-advance (bytevector #x00) 0) => 0)  ; 不完整序列（只有1字节）
(check (bytevector-utf16be-advance (bytevector #xD8 #x3D) 0) => 0)  ; 不完整代理对（只有高代理）
(check (bytevector-utf16be-advance (bytevector #xD8 #x3D #xDC) 0) => 0)  ; 不完整代理对（缺少低代理字节）

;; bytevector-utf16be-advance 无效序列测试
(check (bytevector-utf16be-advance (bytevector #xDC #x00 #x00 #x00) 0) => 0)  ; 低代理对作为第一个码元
(check (bytevector-utf16be-advance (bytevector #xD8 #x3D #x00 #x00) 0) => 0)  ; 无效低代理对

;; bytevector-utf16be-advance 结束位置参数测试
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #x65 #x00 #x6C) 0 2) => 2)  ; "H" -> 结束
(check (bytevector-utf16be-advance (bytevector #x00 #x48 #x00 #x65 #x00 #x6C) 0 4) => 2)  ; "H" -> "e"
(check (bytevector-utf16be-advance (bytevector #x00 #xE4 #x00 #x48) 0 2) => 2)  ; "ä" -> 结束
(check (bytevector-utf16be-advance (bytevector #x00 #xE4 #x00 #x48) 0 4) => 2)  ; "ä" -> "H"

#|
utf16be->utf8
将 UTF-16BE 编码的字节向量转换为 UTF-8 编码的字节向量

函数签名
----
(utf16be->utf8 bytevector) → bytevector

参数
----
bytevector : bytevector
包含 UTF-16BE 编码字节的字节向量

返回值
----
bytevector
包含 UTF-8 编码字节的字节向量

描述
----
`utf16be->utf8` 用于将 UTF-16BE 编码的字节向量转换为 UTF-8 编码的字节向量。

转换规则
------
- 遍历 UTF-16BE 字节向量中的每个 Unicode 字符
- 使用 `utf16be->codepoint` 将 UTF-16BE 字节转换为 Unicode 码点
- 使用 `codepoint->utf8` 将码点转换为 UTF-8 字节向量
- 将转换后的字节向量拼接起来形成最终结果

错误处理
------
- 如果字节向量包含无效的 UTF-16BE 编码序列，会抛出 `value-error` 异常
- 参数必须是字节向量类型，否则会抛出 `type-error` 异常

实现说明
------
- 函数使用 `bytevector-utf16be-advance` 遍历 UTF-16BE 字节向量
- 支持所有有效的 Unicode 字符，包括基本多文种平面和辅助平面字符
- 正确处理代理对编码

相关函数
--------
- `utf16be->codepoint` : 将 UTF-16BE 字节向量转换为 Unicode 码点
- `codepoint->utf8` : 将 Unicode 码点转换为 UTF-8 字节向量
- `utf8->utf16be` : 将 UTF-8 字节向量转换为 UTF-16BE 字节向量
- `bytevector-utf16be-advance` : 在 UTF-16BE 字节向量中前进到下一个字符
|#

;; utf16be->utf8 ASCII 字符测试
(check (utf16be->utf8 (bytevector #x00 #x48 #x00 #x65 #x00 #x6C #x00 #x6C #x00 #x6F)) => (bytevector #x48 #x65 #x6C #x6C #x6F))  ; "Hello"
(check (utf16be->utf8 (bytevector #x00 #x20)) => (bytevector #x20))  ; 空格

;; utf16be->utf8 基本多文种平面字符测试
(check (utf16be->utf8 (bytevector #x00 #xE4)) => #u8(#xC3 #xA4))  ; "ä"
(check (utf16be->utf8 (bytevector #x00 #xE9)) => #u8(#xC3 #xA9))  ; "é"

;; utf16be->utf8 其他 BMP 字符测试
(check (utf16be->utf8 (bytevector #x4E #x2D)) => #u8(#xE4 #xB8 #xAD))  ; "中"
(check (utf16be->utf8 (bytevector #x6C #x49)) => #u8(#xE6 #xB1 #x89))  ; "汉"

;; utf16be->utf8 辅助平面字符测试
(check (utf16be->utf8 (bytevector #xD8 #x3D #xDC #x4D)) => #u8(#xF0 #x9F #x91 #x8D))  ; "👍"
(check (utf16be->utf8 (bytevector #xD8 #x3D #xDE #x80)) => #u8(#xF0 #x9F #x9A #x80))  ; "🚀"

;; utf16be->utf8 混合字符测试
(check (utf16be->utf8 (bytevector #x00 #x48 #x00 #xE4 #x4E #x2D #xD8 #x3D #xDC #x4D)) => #u8(#x48 #xC3 #xA4 #xE4 #xB8 #xAD #xF0 #x9F #x91 #x8D))  ; "Hä中👍"

;; utf16be->utf8 边界条件测试
(check (utf16be->utf8 #u8()) => #u8())  ; 空字节向量
(check (utf16be->utf8 (bytevector #x00 #x48)) => (bytevector #x48))  ; 单字符

;; utf16be->utf8 错误处理测试
(check-catch 'value-error (utf16be->utf8 (bytevector #x00)))  ; 不完整的 UTF-16BE 序列
(check-catch 'value-error (utf16be->utf8 (bytevector #xDC #x00 #x00 #x00)))  ; 无效 UTF-16BE 序列

;; utf16be->utf8 与 utf8->utf16be 互逆操作验证
(check (utf16be->utf8 (utf8->utf16be (bytevector #x48 #x65 #x6C #x6C #x6F))) => (bytevector #x48 #x65 #x6C #x6C #x6F))  ; "Hello"
(check (utf16be->utf8 (utf8->utf16be #u8(#xC3 #xA4))) => #u8(#xC3 #xA4))  ; "ä"
(check (utf16be->utf8 (utf8->utf16be #u8(#xE4 #xB8 #xAD))) => #u8(#xE4 #xB8 #xAD))  ; "中"
(check (utf16be->utf8 (utf8->utf16be #u8(#xF0 #x9F #x91 #x8D))) => #u8(#xF0 #x9F #x91 #x8D))  ; "👍"

(check-report)

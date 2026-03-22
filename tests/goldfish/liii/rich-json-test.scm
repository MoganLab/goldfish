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
        (liii rich-json)
        (liii rich-char)
        (liii base)
        (liii error))

; comment this line to show detailed check reports
; (check-set-mode! 'report-failed)

; shared test data
(define bob-j (rich-json
                    '((bob . ((age . 18)
                              (sex . male)
                              (name . "Bob"))))))


#|
rich-json%get
获取JSON对象的值。

语法
----
(rich-json-instance :get)

参数
----
无参数。

返回值
-----
返回JSON对象的值。

功能
----
- 对于null类型JSON对象，返回'null
- 对于其他类型JSON对象，返回其原始值

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)
|#

;; null
(check (rich-json :apply '() :get) => 'null)
;; 布尔值
(check (rich-json :apply #t :get) => 'true)
(check (rich-json :apply #f :get) => 'false)
;; 数字
(check (rich-json :apply 2 :get) => '2)
;; 字典。
(check (rich-json :apply `(("name" . "Bob") ("age" . 21)) :get) => `(("name" . "Bob") ("age" . 21)))
;; 数组
(check (rich-json :apply #(1 2 3) :get) => #(1 2 3))

#|
rich-json%get-or-else
获取JSON对象的值，如果为null则返回默认值。

语法
----
(rich-json-instance :get-or-else default-value)

参数
----
default-value : any
当JSON对象为null时返回的默认值。

返回值
-----
- 如果JSON对象不为null，返回其值
- 如果JSON对象为null，返回default-value

功能
----
提供安全的默认值机制，避免处理null值时出错。
|#

(check (rich-json :null :get-or-else bob-j) => bob-j)

#|
rich-json%keys
获取JSON对象的所有键名。

语法
----
(rich-json-instance :keys)

参数
----
无参数。

返回值
-----
返回JSON对象的所有键名列表。

功能
----
- 对于对象类型JSON，返回所有键名的列表
- 对于数组、null、布尔值等非对象类型，返回空列表
|#



(let ((j (rich-json '((bob . ((age . 18) (sex . male)))))))
  (check (j :keys) => '(bob))
  (check ((j 'bob) :keys) => '(age sex)))

(check ((rich-json :null) :keys) => '())
(check ((rich-json :true) :keys) => '())
(check ((rich-json :false) :keys) => '())
(check ((rich-json :parse "[1,2,3]") :keys) => '())

#|
rich-json%apply
通过键路径访问JSON对象的嵌套值。

语法
----
(rich-json-instance key1 key2 ...)

参数
----
key1, key2, ... : symbol | string | number | boolean
用于访问嵌套值的键路径。

返回值
-----
返回指定键路径对应的JSON对象。

功能
----
- 支持多层嵌套访问
- 如果键不存在，返回null类型的JSON对象
- 支持符号、字符串、数字和布尔值作为键
|#
(check (bob-j 'bob 'age) => (rich-json 18))
(check (bob-j 'bob 'sex) => (rich-json 'male))
(check (bob-j 'alice) => (rich-json :null))
(check (bob-j 'alice 'age) => (rich-json :null))
(check (bob-j 'bob 'name) => (rich-json "Bob"))

(let ((j (rich-json '((bob . ((age . 18) (sex . male)))))))
  (check ((j 'alice) :null?) => #t)
  (check ((j 'bob) :null?) => #f))
(let ((j (rich-json '((alice . ((age . 18) (sex . male)))))))
  (check ((j 'alice) :null?) => #f)
  (check ((j 'bob) :null?) => #t))


#|
rich-json%contains-key?
检查JSON对象是否包含指定的键。

语法
----
(rich-json-instance :contains-key? key)

参数
----
key : symbol | string | number | boolean
要检查的键名。

返回值
-----
返回布尔值：
- #t：如果键存在
- #f：如果键不存在

功能
----
- 仅检查当前层级的键
- 不检查嵌套对象中的键
- 对于非对象类型JSON，总是返回#f
|#

(let ((j (rich-json '((bob . ((age . 18) (sex . male)))))))
  (check-false (j :contains-key? 'alice))
  (check-true (j :contains-key? 'bob))
  (check-false (j :contains-key? 'age))
  (check-false (j :contains-key? 'sex)))

(let ((j (rich-json #(1 2 3))))
  (check (j :to-string) => "[1,2,3]"))

(check (rich-json '((a (b . 1) (c . 2)))) => (rich-json :parse "{a:{b:1,c:2}}"))
(check (rich-json '((a . ((b . 1) (c . 2))))) => (rich-json :parse "{a:{b:1,c:2}}"))
(check '((a . ((b . 1) (c . 2)))) => '((a (b . 1) (c . 2))))

(check-catch 'value-error (json->string '((a))))

(check (rich-json 'null)=> (rich-json :null))


#|
rich-json@parse
将json格式的字符串的转化成JSON对象。

语法
----
(rich-json :parse json_string)

参数
----
json_string : json格式的字符串


返回值
-----
返回对应的JSON对象。

功能
----
- 将json格式的字符串的转化成JSON对象。
- 包含object、array、string、number、“true”、“false”、“null”
|#
(check (rich-json :parse "[]") => (rich-json :apply #()))
(check (rich-json :parse "[true]") => (rich-json :apply #(true)))
(check (rich-json :parse "[false]") => (rich-json :apply #(false)))
(check (rich-json :parse "[1,2,3]") => (rich-json :apply #(1 2 3)))
(check (rich-json :parse "[{data: 1},{}]") => (rich-json :apply #(((data . 1)) (()))));; 数组里面有对象
(check (rich-json :parse "{}") => (rich-json :apply '(())))
(check (rich-json :parse "{args: {}}") => (rich-json :apply '((args ()))))
(check (rich-json :parse "{\"args\": {}}") => (rich-json :apply '(("args" ()))))
(check (rich-json :parse "{\"args\": {}, data: 1}") => (rich-json :apply '(("args" ()) (data . 1))))
(check (rich-json :parse "{\"args\": {}, data: [1,2,3]}") => (rich-json :apply '(("args" ()) (data . #(1 2 3))))) ;;JSON对象的值是数组
(check (rich-json :parse "{\"args\": {}, data: true}") => (rich-json :apply '(("args" ()) (data . true))))
(check (rich-json :parse "{\"args\": {}, data: null}") => (rich-json :apply '(("args" ()) (data . null))))

;; todo bug需要修复
; (check (rich-json :parse "[null]") => (rich-json :apply #()))
; (check (rich-json :parse "[true],[true]") => (rich-json :apply #t))
; (check (rich-json :parse "{\"args\": {}, data: [true]}") => (rich-json :apply '(("args" ()) (data . #t))))
; (check (rich-json :parse "{\"args\": {}, data: [null]}") => (rich-json :apply '(("args" ()) (data . '()))))


#|
json-string-escape
将字符串转换为JSON格式的转义字符串。

语法
----
(json-string-escape string)

参数
----
string : string
要转义的原始字符串。

返回值
-----
返回JSON格式的转义字符串，包含双引号。

功能
----
- 转义JSON特殊字符：\" \\ \/ \b \f \n \r \t
- 添加双引号包围
- 支持Unicode字符
- 优化处理Base64等安全字符串

边界条件
--------
- 空字符串返回""
- 包含特殊字符的字符串会被正确转义
- 支持中文字符和Unicode转义

性能特征
--------
- 时间复杂度：O(n)，n为字符串长度
- 空间复杂度：O(n)，存储转义后的字符串

兼容性
------
- 符合JSON标准
- 支持所有可打印Unicode字符
|#
; Basic json-string-escape tests
(check (json-string-escape "hello") => "\"hello\"")
(check (json-string-escape "hello\"world") => "\"hello\\\"world\"")
(check (json-string-escape "hello\\world") => "\"hello\\\\world\"")
(check (json-string-escape "hello/world") => "\"hello\\/world\"")
(check (json-string-escape "hello\bworld") => "\"hello\\bworld\"")
(check (json-string-escape "hello\fworld") => "\"hello\\fworld\"")
(check (json-string-escape "hello\nworld") => "\"hello\\nworld\"")
(check (json-string-escape "hello\rworld") => "\"hello\\rworld\"")
(check (json-string-escape "hello\tworld") => "\"hello\\tworld\"")

; Extended json-string-escape tests for optimized version
; Test empty string
(check (json-string-escape "") => "\"\"")

; Test single character strings
(check (json-string-escape "A") => "\"A\"")
(check (json-string-escape "\"") => "\"\\\"\"")
(check (json-string-escape "\\") => "\"\\\\\"")

; Test Base64-like strings (should use fast path for large strings)
(check (json-string-escape "ABC") => "\"ABC\"")
(check (json-string-escape "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+=") 
       => "\"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+=\"")

; Test typical Base64 encoded string (short) - these will use slow path now
(check (json-string-escape "SGVsbG8gV29ybGQ=") => "\"SGVsbG8gV29ybGQ=\"")
(check (json-string-escape "VGhpcyBpcyBhIHRlc3Q=") => "\"VGhpcyBpcyBhIHRlc3Q=\"")

; Test Base64 with padding
(check (json-string-escape "QWxhZGRpbjpvcGVuIHNlc2FtZQ==") => "\"QWxhZGRpbjpvcGVuIHNlc2FtZQ==\"")

; Test large Base64-like string WITHOUT slashes (should trigger fast path optimization)
; This is a 1024-character Base64-like string without slashes
(let ((large-base64 
        (string-append 
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+="
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567")))
  (check (json-string-escape large-base64) 
         => (string-append "\"" large-base64 "\"")))

; Test mixed content strings (should NOT use fast path)
(check (json-string-escape "Hello123+=") => "\"Hello123+=\"")
(check (json-string-escape "Base64WithNewline\nText") => "\"Base64WithNewline\\nText\"")
(check (json-string-escape "Base64With\"Quote") => "\"Base64With\\\"Quote\"")

; Test edge cases for sampling logic
; String exactly 1000 chars of Base64 (at threshold, should use slow path)
(let ((threshold-base64 
        (make-string 1000 #\A)))  ; 1000 'A' characters
  (check (json-string-escape threshold-base64) 
         => (string-append "\"" threshold-base64 "\"")))

; String just over 1000 chars with Base64 content (should use fast path)
(let ((large-base64-1001 
        (string-append (make-string 1001 #\A))))  ; 1001 'A' characters
  (check (json-string-escape large-base64-1001) 
         => (string-append "\"" large-base64-1001 "\"")))

; String over 1000 chars but with non-Base64 in first 100 chars (should use slow path)
(let ((mixed-large 
        (string-append "Quote\"InFirst100" (make-string 990 #\A))))
  (check (json-string-escape mixed-large) 
         => (string-append "\"Quote\\\"InFirst100" (make-string 990 #\A) "\"")))

; Test numeric strings (Base64-like)
(check (json-string-escape "1234567890") => "\"1234567890\"")
(check (json-string-escape "0123456789ABCDEFabcdef") => "\"0123456789ABCDEFabcdef\"")

; Test URL-safe Base64 characters (but no slashes)
(check (json-string-escape "URLsafe_Base64chars") => "\"URLsafe_Base64chars\"")

; Test performance edge case: very long string with escape chars
(let ((long-escaped 
        (make-string 50 #\")))  ; 50 quote characters
  (check (string-length (json-string-escape long-escaped)) => 102))  ; 50*2 + 2 quotes = 102

; Test that all Base64 safe characters are recognized correctly (no slashes)
(check (json-string-escape "ABCDEFGHIJKLMNOPQRSTUVWXYZ") => "\"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"")
(check (json-string-escape "abcdefghijklmnopqrstuvwxyz") => "\"abcdefghijklmnopqrstuvwxyz\"")
(check (json-string-escape "0123456789") => "\"0123456789\"")
(check (json-string-escape "+=") => "\"+=\"")

#|
string->json
将JSON字符串解析为Scheme数据结构。

语法
----
(string->json json-string)

参数
----
json-string : string
要解析的JSON格式字符串。

返回值
-----
返回对应的Scheme数据结构。

功能
----
- 解析JSON对象、数组、字符串、数字、布尔值、null
- 支持Unicode转义序列
- 支持代理对解析
- 自动处理转义字符

边界条件
--------
- 无效JSON字符串抛出parse-error异常
- 支持宽松语法（不带引号的键名）
- 支持中文字符和Unicode转义

性能特征
--------
- 时间复杂度：O(n)，n为字符串长度
- 空间复杂度：O(n)，存储解析后的数据结构

兼容性
------
- 符合JSON标准
- 支持扩展语法（不带引号的键名）
|#
(check (string->json "{\"age\":18}") => `(("age" . 18)))
(check (string->json "{age:18}") => `((age . 18)))
(check (string->json "{\"name\":\"中文\"}") => `(("name" . "中文"))) 

(check (string->json "{\"name\":\"Alice\\nBob\"}") => '(("name" . "Alice\nBob")))
(check (string->json "{\"name\":\"Alice\\tBob\"}") => '(("name" . "Alice\tBob")))
(check (string->json "{\"name\":\"Alice\\rBob\"}") => '(("name" . "Alice\rBob")))
(check (string->json "{\"name\":\"Alice\\bBob\"}") => '(("name" . "Alice\bBob")))
(check (string->json "{\"name\":\"Alice\\fBob\"}") => '(("name" . "Alice\fBob")))
(check (string->json "{\"name\":\"Alice\\\\Bob\"}") => '(("name" . "Alice\\Bob")))
(check (string->json "{\"name\":\"Alice\\\/Bob\"}") => '(("name" . "Alice/Bob")))
(check (string->json "{\"name\":\"Alice\\\"Bob\"}") => '(("name" . "Alice\"Bob")))

(check (string->json "[\"\\u0041\"]") => #("A"))
(check (string->json "[\"\\u0041\\u0042\"]") => #("AB"))  ; 多个 \u 转义字符
(check (string->json "[\"\\u4E2D\\u6587\"]") => #("中文"))  ; 中文字符
(check (string->json "[\"\\uD83D\\uDE00\"]") => #("😀"))  ; 代理对（假设支持代理对）

(check (string->json "{\"name\":\"\\u4E2D\\u6587\"}") => '(("name" . "中文")))  ; 嵌套 JSON
(check (string->json "{\"emoji\":\"\\uD83D\\uDE00\"}") => '(("emoji" . "😀")))  ; 嵌套 JSON 中的代理对

(check-catch 'parse-error (string->json "[\"\\u004G\"]"))  ; \u 后包含非十六进制字符
(check-catch 'parse-error (string->json "[\"\\a\"]"))

#|
json->string
将Scheme数据结构转换为JSON字符串。

语法
----
(json->string data)

参数
----
data : any
要转换的Scheme数据结构。

返回值
-----
返回JSON格式的字符串。

功能
----
- 支持对象、数组、字符串、数字、布尔值、null
- 自动转义特殊字符
- 支持嵌套数据结构
- 符合JSON标准格式

边界条件
--------
- 无效数据结构抛出value-error异常
- 空对象转换为{}
- 空数组转换为[]

性能特征
--------
- 时间复杂度：O(n)，n为数据结构大小
- 空间复杂度：O(n)，存储生成的JSON字符串

兼容性
------
- 符合JSON标准
- 支持所有标准JSON数据类型
|#
(check (json->string '(("age" . 18))) => "{\"age\":18}")
(check (json->string #(0 1 2 3)) => "[0,1,2,3]")

#|
rich-json%set
设置JSON对象中指定键的值。

语法
----
(rich-json-instance :set key1 key2 ... value)

参数
----
key1, key2, ... : symbol | string | number | boolean
用于定位嵌套值的键路径。

value : any | function
要设置的值或转换函数。

返回值
-----
返回新的JSON对象，包含更新后的值。

功能
----
- 支持多层嵌套设置
- 如果键不存在，会创建新的键值对
- 支持函数作为最后一个参数，用于转换现有值
- 支持符号、字符串、数字和布尔值作为键

边界条件
--------
- 不存在的键路径会创建新的嵌套结构
- 函数参数接收当前值并返回新值
- 支持任意深度的嵌套设置

性能特征
--------
- 时间复杂度：O(k)，k为键路径长度
- 空间复杂度：O(n)，创建新的JSON对象

兼容性
------
- 支持链式调用
|#
; rich-json%set
; 单层，键为符号
(let* ((j0 (rich-json `((age . 18) (sex . male))))
       (j1 (j0 :set 'age 19))
       (j2 (j0 :set 'age 'null)))
  (check (j0 'age) => (rich-json 18))
  (check (j1 'age) => (rich-json 19))
  (check (j2 'age) => (rich-json :null)))

; 单层，键为字符串
(let* ((j0-raw `(("age" . 18) ("sex" . male)))
       (j0 (rich-json j0-raw))
       (j1 (j0 :set "age" 19)))
  (check (j1 :get-number "age" 0) => 19)
  (check (j0 "age") => (rich-json 18)))

#|
rich-json%get-boolean
获取JSON对象中的布尔值，如果值不是布尔类型则返回默认值。

语法
----
(rich-json-instance :get-boolean key default-value)

参数
----
key : symbol | string | number | boolean
要获取的键名。

default-value : boolean
当值不是布尔类型或键不存在时返回的默认值。

返回值
-----
返回布尔值：
- 如果键存在且值为布尔类型，返回该值
- 否则返回default-value

功能
----
- 安全地获取布尔值，避免类型错误
- 支持符号、字符串、数字和布尔值作为键
|#
; rich-json%get-boolean
(let* ((j0 (rich-json '((active . #t) (verified . #f) (name . "Alice")))))
  (check (j0 :get-boolean 'active #f) => #t)
  (check (j0 :get-boolean 'verified #t) => #f)
  (check (j0 :get-boolean 'name #f) => #f)
  (check (j0 :get-boolean 'nonexistent #t) => #t))

; 单层，键为整数
(let* ((j0 (rich-json #(red green blue)))
       (j1 (j0 :set 0 'black)))
  (check (j0 :get) => #(red green blue))
  (check (j1 :get) => #(black green blue)))

; 单层，键为布尔值
(let* ((j0 (rich-json '((bob . 18) (jack . 16))))
       (j1 (j0 :set #t 3))
       (j2 (j0 :set #t (lambda (x) (+ x 1)))))
  (check (j1 :get) => '((bob . 3) (jack . 3)))
  (check (j2 :get) => '((bob . 19) (jack . 17))))

; 多层，键为符号
(let* ((j0 (rich-json '((person . ((name . "Alice") (age . 25))))))
       (j1 (j0 :set 'person 'age 26)))
  (check (j1 'person 'age) => (rich-json 26)))

; 多层，键为字符串
(let* ((j0 (rich-json
                 '((person . ((name . "Alice")
                              (age . 25)
                              (address . ((city . "Wonderland")
                                          (zip . "12345"))))))))
       (j1 (j0 :set 'person 'address 'city "Newland")))
  (check (j1 'person 'address 'city) => (rich-json "Newland")))

; 单层，最后一个参数不是值，而是一个函数
(let* ((j0 (rich-json '((name . "Alice") (age . 25))))
       (j1 (j0 :set 'age (lambda (x) (+ x 1)))))
  (check (j1 'age) => (rich-json 26)))

; 多层，最后一个参数不是值，而是一个函数
(let* ((j0 (rich-json '((person . ((name . "Alice") (age . 25))))))
       (j1 (j0 :set 'person 'age (lambda (x) (+ x 1)))))
  (check (j1 'person 'age) => (rich-json 26)))

; rich-json%set with rich-json object
(let* ((j0 (rich-json `((age . 18) (sex . male))))
       (j1 (rich-json 20))
       (j2 (j0 :set 'age j1)))
  (check (j2 'age) => (rich-json 20)))

(let* ((j0 (rich-json `((person . ((name . "Alice") (age . 25))))))
       (j1 (rich-json 26))
       (j2 (j0 :set 'person 'age j1)))
  (check (j2 'person 'age) => (rich-json 26)))

(let* ((j0 (rich-json `((person . ((name . "Alice") (age . 25))))))
       (j1 (rich-json `((name . "Bob") (age . 30))))
       (j2 (j0 :set 'person j1)))
  (check (j2 'person 'name) => (rich-json "Bob"))
  (check (j2 'person 'age) => (rich-json 30)))

#|
rich-json%transform
转换JSON对象中指定键的值。

语法
----
(rich-json-instance :transform key1 key2 ... transform-fn)
(rich-json-instance  :transform predicate-fn transform-fn)
(rich-json-instance  :transform #t transform-fn)
(rich-json-instance  :transform #f transform-fn)

参数
----
key1, key2, ... : symbol | string | number | boolean
用于定位嵌套值的键路径。

predicate-fn : function
用于选择要转换的键的谓词函数。

transform-fn : function
转换函数，接收键和值，返回新值。

返回值
-----
返回新的JSON对象，包含转换后的值。

功能
----
- 支持多层嵌套转换
- 支持谓词函数选择要转换的键
- 支持#t转换所有键，#f不转换任何键
- 转换函数接收键和值作为参数
|#
; rich-json%transform instance method
(let* ((j0 (rich-json '((name . "Alice") (age . 25))))
       (j1 (j0 :transform 'name (lambda (k v) (string-upcase v)))))
  (check (j1 'name) => (rich-json "ALICE"))
  (check (j1 'age) => (rich-json 25)))
  
(let* ((j0 (rich-json '((person . ((name . "Alice") (age . 25))))))
       (j1 (j0 :transform 'person (lambda (k v) v))))
  (check (j1 'person) => (rich-json '((name . "Alice") (age . 25)))))
  
(let* ((j0 (rich-json '((name . "Alice") (age . 25))))
       (j1 (j0 :transform (lambda (k) (equal? k 'age)) (lambda (k v) (+ v 1)))))
  (check (j1 'age) => (rich-json 26))
  (check (j1 'name) => (rich-json "Alice")))
  
(let* ((j0 (rich-json '((name . "Alice") (age . 25))))
       (j1 (j0 :transform #t (lambda (k v) (if (string? v) (string-upcase v) v)))))
  (check (j1 'name) => (rich-json "ALICE"))
  (check (j1 'age) => (rich-json 25)))
  
(let* ((j0 (rich-json '((name . "Alice") (age . 25))))
       (j1 (j0 :transform #f (lambda (k v) v))))
  (check (j1 'name) => (rich-json "Alice"))
  (check (j1 'age) => (rich-json 25)))
  
  
; Test rich-json%transform with multiple nested levels
(let* ((j0 (rich-json '((user . ((profile . ((contact . ((email . "alice@example.com")
                                                     (phone . "123-456-7890"))))))))))
       (j1 (j0 :transform 'user 'profile 'contact 'email (lambda (k v) (string-append v ".verified")))))
  (check (j1 'user 'profile 'contact 'email) => (rich-json "alice@example.com.verified")))
  
; Test rich-json%transform for conditional transformation with predicate function
(let* ((j0 (rich-json '((user . ((data . ((scores . #(85 90 78 92 88))
                                      (settings . ((notifications . #t)
                                                   (theme . "dark"))))))))))
       (j1 (j0 :transform 'user 'data (lambda (k) (equal? k 'scores)) (lambda (k v) 
                                                         (vector-map (lambda (score) (+ score 5)) v)))))
  (check (j1 'user 'data 'scores) => (rich-json #(90 95 83 97 93)))
  (check (j1 'user 'data 'settings 'theme) => (rich-json "dark")))


; Compare transform and set
(let* ((j0 (rich-json '((user . ((profile . ((name . "Alice")
                                       (age . 25)
                                       (scores . #(85 90 78)))))))))
       (j1 (j0 :transform 'user 'profile 'scores (lambda (k v) 
                                                  (vector-map (lambda (score) (+ score 5)) v))))
       (j2 (j0 :set 'user 'profile 'scores #(90 95 83))))
  (check (j1 'user 'profile 'scores) => (rich-json #(90 95 83)))
  (check (j2 'user 'profile 'scores) => (rich-json #(90 95 83)))
  (check (j1 'user 'profile 'name) => (rich-json "Alice"))
  (check (j2 'user 'profile 'name) => (rich-json "Alice")))



#|
rich-json%push
向JSON对象中添加新的键值对。

语法
----
(rich-json-instance :push key1 key2 ... value)

参数
----
key1, key2, ... : symbol | string | number | boolean
用于定位嵌套位置的键路径。

value : any
要添加的值。

返回值
-----
返回新的JSON对象，包含新增的键值对。

功能
----
- 支持多层嵌套添加
- 如果键已存在，会覆盖原有值
- 如果键不存在，会创建新的键值对
- 支持符号、字符串、数字和布尔值作为键
|#
; rich-json%push
; 多层，键为符号
(let* ((j0 (rich-json '((person . ((name . "Alice") (age . 25))))))
       (j1 (j0 :push 'person 'city "Wonderland")))
  (check (j1 'person 'city) => (rich-json "Wonderland")))

; 多层，键为字符串
(let* ((j0 (rich-json '(("person" . (("name" . "Alice") ("age" . 25))))))
       (j1 (j0 :push "person" "city" "Wonderland")))
  (check (j1 "person" "city") => (rich-json "Wonderland")))

; 多层，键为符号
(let* ((j0 (rich-json '((person . ((name . "Alice")
                              (age . 25)
                              (address . ((city . "Oldland")
                                          (zip . "12345"))))))))
       (j1 (j0 :push 'person 'address 'street "Main St")))
  (check (j1 'person 'address 'street) => (rich-json "Main St")))

; 多层，JSON是向量
(let* ((j0 (rich-json '((data . #(1 2 3)))))
       (j1 (j0 :push 'data 3 4)))
  (check (j1 :get) => '((data . #(1 2 3 4)))))

; 多层，JSON是二维向量
(let* ((j0 (rich-json '((data . #(#(1 2) #(3 4))))))
       (j1 (j0 :push 'data 1 2 5)))
  (check (j1 :get) => '((data . #(#(1 2) #(3 4 5))))))

; 多层，JSON的Key是数字
; (let* ((j0 (rich-json '((data . ((0 . "zero") (1 . "one"))))))
;        (j1 (j0 :push 'data 2 "two")))
;   (check (j1 'data 2) => (rich-json "two")))
; 因为 (j1 'data) 实际上是 rich-json 的主体，所以这里是错误的，暂时不修复

(let* ((j0 (rich-json '((flags . ((#t . "true") (#f . "false"))))))
       (j1 (j0 :push 'flags #t "yes")))
  (check (j1 'flags #t) => (rich-json "yes")))

#|
json-drop
从JSON数据结构中删除指定的键。

语法
----
(json-drop json key)
(json-drop json predicate-fn)

参数
----
json : list | vector
JSON数据结构。

key : symbol | string | number | boolean
要删除的键名。

predicate-fn : function
用于选择要删除的键的谓词函数。

返回值
-----
返回新的JSON数据结构，不包含被删除的键。

功能
----
- 从JSON对象中删除指定键
- 支持谓词函数选择要删除的键
- 如果键不存在，返回原始数据结构
|#
; json-drop
(let* ((json '((name . "Alice") (age . 25))))
  (let ((updated-json (json-drop json 'age)))
    (check (json-ref updated-json 'age) => '())))

(let* ((json '((name . "Alice")
               (age . 25)
               (address . ((city . "Wonderland")
                           (zip . "12345"))))))
  (let ((updated-json (json-drop* json 'address 'city)))
    (check (json-ref* updated-json 'address 'city) => '())))

(let* ((json '((name . "Alice")
               (age . 25)
               (address . ((city . "Wonderland")
                           (zip . "12345"))))))
  (let ((j1 (json-drop json (lambda (k) (equal? k 'city)))))
    (check (json-ref* j1 'address 'city) => "Wonderland"))
  (let ((j2 (json-drop json (lambda (k) (equal? k 'name)))))
    (check (json-ref* j2 'name) => '()))
  (let ((j3 (json-drop* json 'address (lambda (k) (equal? k 'city)))))
    (check (json-ref* j3 'address 'city) => '())))

#|
rich-json%drop
从 JSON 数据结构中删除指定的元素。

语法
----
1. 路径删除模式（Deep Delete）：
   (rich-json-instance :drop key1 key2 ... target-key)

2. 谓词删除模式（Shallow Filter）：
   (rich-json-instance :drop predicate-fn)

参数
----
key1, key2, ... : symbol | string | number | boolean
    用于定位要删除元素的层级路径。
    - 最后一个参数是要删除的目标键（对象）或索引（数组）。
    - 前面的参数是导航路径。

predicate-fn : function (lambda (key) ...)
    用于筛选要删除项的谓词函数。
    - 接收参数：
      * 对于对象 (Object)：接收 **键名 (Key)** (通常是 Symbol)。
      * 对于数组 (Array)：接收 **索引 (Index)** (整数)。
    - 返回值：如果不希望保留该项（即希望删除），返回 #t；否则返回 #f。
    - 注意：此函数**不接收**元素的值 (Value)。

返回值
-----
返回一个新的 rich-json 对象，其中指定的元素已被移除。

功能
----
1. **路径删除**：
   - 支持多层嵌套定位。
   - 就像文件系统路径一样，精准打击并删除路径末端的一个元素。
   - 如果路径不存在，操作无效，返回原对象。

2. **谓词删除**：
   - 仅作用于**当前层级**（浅层）。
   - 批量删除所有满足条件的项。
   - **陷阱提示**：对于数组，它判断的是下标（0, 1, 2...）而不是数组里的内容。

示例
----
;; 路径删除：删除 person 下 address 里的 zip 字段
(j :drop 'person 'address 'zip)

;; 谓词删除（对象）：删除所有键名为 string 类型或特定名称的键
(j :drop (lambda (k) (eq? k 'age))) 

;; 谓词删除（数组）：删除所有索引为偶数的元素（即删除第 0, 2, 4... 项）
(j :drop even?) 
|#
(let* ((j0 (rich-json '((name . "Alice") (age . 25) (city . "Wonderland"))))
       (j1 (j0 :drop 'age)))
  (check (j1 'age) => (rich-json :null))
  (check (j1 'name) => (rich-json "Alice"))
  (check (j1 'city) => (rich-json "Wonderland")))

(let* ((j0 (rich-json '((user . ((profile . ((name . "Alice")
                                       (age . 25)
                                       (scores . #(85 90 78)))))))))
       (j1 (j0 :drop 'user 'profile 'scores)))
  (check (j1 'user 'profile 'scores) => (rich-json :null))
  (check (j1 'user 'profile 'name) => (rich-json "Alice"))
  (check (j1 'user 'profile 'age) => (rich-json 25)))

(let* ((j0 (rich-json '((data . #(1 2 3 4 5)))))
       (j1 (j0 :drop 'data (lambda (k) (and (number? k) (even? k))))))
  (check (j1 'data) => (rich-json #(2 4))))

(let* ((j0 (rich-json '((settings . (("theme" . "dark")
                               (notifications . #t)
                               ("language" . "en"))))))
       (j1 (j0 :drop 'settings (lambda (k) (string? k)))))
  (check (j1 'settings "theme") => (rich-json :null))
  (check (j1 'settings "language") => (rich-json :null)))

(let* ((j0 (rich-json '((a . 1) (b . 2) (c . 3))))
       (j1 (j0 :drop (lambda (k) (member k '(a c))))))
  (check (j1 'a) => (rich-json :null))
  (check (j1 'b) => (rich-json 2))
  (check (j1 'c) => (rich-json :null)))


(let* ((j0 (rich-json #()))
       (j1 (j0 :drop 0)))
  (check (j1 :get) => #()))

#|
json-reduce
转换JSON数据结构中指定键的值。

语法
----
(json-reduce json key transform-fn)
(json-reduce json predicate-fn transform-fn)

参数
----
json : list | vector
JSON数据结构。

key : symbol | string | number | boolean
要转换的键名。

predicate-fn : function
用于选择要转换的键的谓词函数。

transform-fn : function
转换函数，接收键和值，返回新值。

返回值
-----
返回新的JSON数据结构，包含转换后的值。

功能
----
- 转换JSON对象中指定键的值
- 支持谓词函数选择要转换的键
- 转换函数接收键和值作为参数
- 支持多层嵌套转换
|#
; json-reduce
(let ((json '((name . "Alice") (age . 25))))
  (check (json-reduce json 'name (lambda (k v) (string-upcase v)))
         => '((name . "ALICE") (age . 25))))

(let ((json '((person . ((name . "Alice") (age . 25))))))
  (check (json-reduce json 'person (lambda (k v) v))
         => '((person . ((name . "Alice") (age . 25))))))

(let ((json '((name . "Alice") (age . 25))))
  (check (json-reduce json (lambda (k) (equal? k 'age)) (lambda (k v) (+ v 1)))
         => '((name . "Alice") (age . 26))))

(let ((json '((person . ((name . "Alice") (age . 25))))))
  (check (json-reduce json (lambda (k) (equal? k 'person)) (lambda (k v) v))
         => '((person . ((name . "Alice") (age . 25))))))

(let ((json '((name . "Alice") (age . 25))))
  (check (json-reduce json #t (lambda (k v) (if (string? v) (string-upcase v) v)))
         => '((name . "ALICE") (age . 25))))

(let ((json '((name . "Alice") (age . 25))))
  (check (json-reduce json #f (lambda (k v) v))
         => '((name . "Alice") (age . 25))))

(let ((json '()))
  (check (json-reduce json 'name (lambda (k v) v))
         => '()))

(let ((json #()))
  (check (json-reduce json 'name (lambda (k v) v))
         => #()))

(let ((json '((person . ((name . "Alice")
                         (age . 25)
                         (address . ((city . "Wonderland")
                                     (zip . "12345"))))))))
  (let ((updated-json (json-reduce* json 'person 'address 'city (lambda (x y) (string-upcase y)))))
    (check (json-ref* updated-json 'person 'address 'city) => "WONDERLAND")))


; rich-json%push with rich-json object
(let* ((j0 (rich-json `((person . ((name . "Alice") (age . 25))))))
       (j1 (rich-json "Wonderland"))
       (j2 (j0 :push 'person 'city j1)))
  (check (j2 'person 'city) => (rich-json "Wonderland")))

(let* ((j0 (rich-json `((person . ((name . "Alice") (age . 25))))))
       (j1 (rich-json `((city . "Wonderland") (zip . "12345"))))
       (j2 (j0 :push 'person 'address j1)))
  (check (j2 'person 'address 'city) => (rich-json "Wonderland"))
  (check (j2 'person 'address 'zip) => (rich-json "12345")))



; Test with nested rich-json objects
(let* ((j0 (rich-json `((person . ((name . "Alice") (age . 25))))))
       (j1 (rich-json `((address . ((city . "Wonderland") (zip . "12345"))))))
       (j2 (j0 :set 'person j1)))
  (check (j2 'person 'address 'city) => (rich-json "Wonderland"))
  (check (j2 'person 'address 'zip) => (rich-json "12345")))

; Test with mixed rich-json objects and primitive values
(let* ((j0 (rich-json `((person . ((name . "Alice") (age . 25))))))
       (j1 (rich-json "Wonderland"))
       (j2 ((j0 :push 'person 'city j1) :set 'person 'age 26)))
  (check (j2 'person 'city) => (rich-json "Wonderland"))
  (check (j2 'person 'age) => (rich-json 26)))

; Test with null rich-json object
(let* ((j0 (rich-json `((person . ((name . "Alice") (age . 25))))))
       (j1 (rich-json :null))
       (j2 (j0 :set 'person 'age j1)))
  (check (j2 'person 'age) => (rich-json :null)))

; Test with boolean rich-json object
(let* ((j0 (rich-json `((person . ((name . "Alice") (age . 25))))))
       (j1 (rich-json :true))
       (j2 (j0 :push 'person 'active j1)))
  (check (j2 'person 'active) => (rich-json :true)))

; Test with array rich-json object
(let* ((j0 (rich-json `((person . ((name . "Alice") (age . 25))))))
       (j1 (rich-json #(1 2 3)))
       (j2 (j0 :push 'person 'scores j1)))
  (check (j2 'person 'scores) => (rich-json #(1 2 3))))

(check
  (json->string
    `(("messages" . #((("role" . "user") ("content" . #(1 2 3)))
                      (("role" . "user") ("content" . "中文"))))))
  => "{\"messages\":[{\"role\":\"user\",\"content\":[1,2,3]},{\"role\":\"user\",\"content\":\"中文\"}]}")

(check
  (json->string
    `(("messages" . #(
        (("role" . "user") ("content" . #(
          (("text" . "1") ("type" . "text"))
          (("text" . "2") ("type" . "text"))
        )))
        (("role" . "user") ("content" . "中文"))
      ))))
  => "{\"messages\":[{\"role\":\"user\",\"content\":[{\"text\":\"1\",\"type\":\"text\"},{\"text\":\"2\",\"type\":\"text\"}]},{\"role\":\"user\",\"content\":\"中文\"}]}"
)

(check-report)
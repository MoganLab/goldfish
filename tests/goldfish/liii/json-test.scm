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
        (liii json)
        (liii base)
        (liii error))

; comment this line to show detailed check reports
;(check-set-mode! 'report-failed)

; shared test data
(define bob-j '((bob . ((age . 18)
                        (sex . male)
                        (name . "Bob")))))






#|
类型谓词测试 (Type Predicates)
检查 JSON 数据类型的判断函数。
包含：json-null?, json-object?, json-array?, json-string?, 
      json-number?, json-integer?, json-float?, json-boolean?
|#

;; 1. json-null?
(define bob-pp '((bob . ((age . 18)
  (sex . male)
  (name . "Bob")
  (empty . null)))))
(check-true (json-null? 'null))
(check-false (json-null? '((name . "Alice"))))      
(check-true (json-null? (json-ref bob-pp 'bob 'empty)))

;; 2. json-object?
;; 在 guenchi json 中，非空列表 (alist) 表示对象
(check-true (json-object? '((name . "Alice"))))
(check-true (json-object? '((a . 1) (b . 2))))
(check-false (json-object? '()))    
(check-false (json-object? #(1 2))) 
(check-false (json-object? "{}"))
(check-false (json-object? '(1 2 3)))   

;; 3. json-array?
;; 向量 (vector) 表示数组
(check-true (json-array? #(1 2 3)))
(check-true (json-array? #()))      ; 空向量是空数组
(check-true (json-array? #("a" "b")))
(check-false (json-array? '(1 2 3))); 列表不是数组
(check-false (json-array? "[]"))

;; 4. json-string?
(check-true (json-string? "hello"))
(check-true (json-string? ""))
(check-false (json-string? 'hello)) ; 符号不是字符串
(check-false (json-string? 123))

;; 5. json-number?
;; 包含整数和浮点数
(check-true (json-number? 123))
(check-true (json-number? 3.14))
(check-true (json-number? -10))
(check-true (json-number? 0))
(check-false (json-number? "123"))

;; 6. json-integer?
(check-true (json-integer? 100))
(check-true (json-integer? 0))
(check-true (json-integer? -5))
(check-false (json-integer? 3.14))
(check-false (json-integer? 1.0))  

;; 7. json-float?
(check-true (json-float? 3.14))
(check-true (json-float? -0.01))
(check-false (json-float? 100))

;; 8. json-boolean?
;; Scheme 的 #t 和 #f
(check-true (json-boolean? #t))
(check-true (json-boolean? #f))
(check-false (json-boolean? 0))
(check-false (json-boolean? "true"))

#|
json-ref (Basic)
获取JSON对象的值。

语法
----
(json-ref json key)
或直接使用数据本身。

返回值
-----
返回JSON对象的值。


|#


;; 字典。
(check (string->json "{\"name\":\"Bob\",\"age\":21}") => `(("name" . "Bob") ("age" . 21)))
;; 数组
(check (string->json "[1,2,3]") => #(1 2 3))

#|
json-get-or-else
如果 JSON 对象为 null，则返回默认值；否则返回 JSON 对象本身。

语法
----
(json-get-or-else json default-value)

参数
----
json : any
JSON 数据对象。

default-value : any
当 JSON 对象为 null 时返回的默认值。

返回值
-----
- 如果 JSON 对象不为 null，返回该对象本身
- 如果 JSON 对象为 null，返回 default-value

功能
----
提供安全的 null 值处理机制，当遇到 JSON null 时使用默认值替代。
|#

(check (json-get-or-else 'null bob-j) => bob-j)

#|
json-keys
获取JSON对象的所有键名。

语法
----
(json-keys json)

参数
----
json : any
JSON数据对象。

返回值
-----
返回JSON对象的所有键名列表。

功能
----
- 对于对象类型JSON，返回所有键名的列表
- 对于数组、null、布尔值等非对象类型，返回空列表
|#

(let ((j '((bob . ((age . 18) (sex . male))))))
  (check (json-keys j) => '(bob))
  (check (json-keys (json-ref j 'bob)) => '(age sex)))


(check (json-keys 'null) => '())
(check (json-keys 'true) => '())
(check (json-keys 'false) => '())
(check (json-keys (string->json "[1,2,3]")) => '())
(check (json-keys (string->json "{}")) => '())

#|
json-ref (Nested)
通过键路径访问JSON对象的嵌套值。

语法
----
(json-ref json key1 key2 ...)

参数
----
key1, key2, ... : symbol | string | number | boolean
用于访问嵌套值的键路径。

返回值
-----
返回指定键路径对应的JSON值。

功能
----
- 支持多层嵌套访问
- 如果键不存在，返回空列表 '() (相当于 missing/nil)
- 支持符号、字符串、数字和布尔值作为键
|#
(check (json-ref bob-j 'bob 'age) => 18)
(check (json-ref bob-j 'bob 'sex) => 'male)
(check (json-ref bob-j 'alice) => '())
(check (json-ref bob-j 'alice 'age) => '())
(check (json-ref bob-j 'bob 'name) => "Bob")

(let ((j '((bob . ((age . 18) (sex . male))))))
  (check (json-null? (json-ref j 'alice)) => #f)
  (check (null? (json-ref j 'alice)) => #t)      ; 确认为 Scheme 空列表
  (check (json-null? (json-ref j 'bob)) => #f))

(let ((j '((alice . ((age . 18) (sex . male))))))
  (check (json-null? (json-ref j 'alice)) => #f)
  (check (null? (json-ref j 'bob)) => #t))


#|
json-contains-key?
检查JSON对象是否包含指定的键。

语法
----
(json-contains-key? json key)

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

(let ((j '((bob . ((age . 18) (sex . male))))))
  (check-false (json-contains-key? j 'alice))
  (check-true (json-contains-key? j 'bob))
  (check-false (json-contains-key? j 'age))
  (check-false (json-contains-key? j 'sex)))


(check-false (json-contains-key? (string->json "{}") "a"))

(let ((j #(1 2 3)))
  (check (json->string j) => "[1,2,3]"))

(check (string->json "{a:{b:1,c:2}}") => '((a . ((b . 1) (c . 2)))))
; json->string 生成宽松格式（key无引号）
(check (json->string '((a . ((b . 1) (c . 2))))) => "{a:{b:1,c:2}}")

(check-catch 'value-error (json->string '((a))))



#|
string->json (Parse)
将json格式的字符串的转化成JSON对象。

语法
----
(string->json json_string)

参数
----
json_string : json格式的字符串

返回值
-----
返回对应的JSON数据结构。

功能
----
- 将json格式的字符串的转化成JSON对象。
- 包含object、array、string、number、“true”、“false”、“null”
|#
(check (string->json "[]") => #())
(check (string->json "[true]") => #(true))
(check (string->json "[false]") => #(false))
(check (string->json "[1,2,3]") => #(1 2 3))
(check (string->json "[{data: 1},{}]") => #(((data . 1)) (()))) ;; 数组里面有对象
(check (string->json "{}") => '(()))
(check (string->json "{args: {}}") => '((args ())))
(check (string->json "{\"args\": {}}") => '(("args" ())))
(check (string->json "{\"args\": {}, data: 1}") => '(("args" ()) (data . 1)))
(check (string->json "{\"args\": {}, data: [1,2,3]}") => '(("args" ()) (data . #(1 2 3)))) ;;JSON对象的值是数组
(check (string->json "{\"args\": {}, data: true}") => `(("args" ()) (data . true)))
(check (string->json "{\"args\": {}, data: null}") => `(("args" ()) (data . null)))

;; todo bug需要修复
; (check (string->json "[null]") => #(null))
; (check (string->json "[true],[true]") => #t)
; (check (string->json "{\"args\": {}, data: [true]}") => '(("args" ()) (data . #(#t))))
; (check (string->json "{\"args\": {}, data: [null]}") => '(("args" ()) (data . #(null))))


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
string->json (Advanced)
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
(check (string->json "[\"\\uD83D\\uDE00\"]") => #("😀"))  ; 代理对

(check (string->json "{\"name\":\"\\u4E2D\\u6587\"}") => '(("name" . "中文")))  ; 嵌套 JSON
(check (string->json "{\"emoji\":\"\\uD83D\\uDE00\"}") => '(("emoji" . "😀")))  ; 嵌套 JSON 中的代理对

(check-catch 'parse-error (string->json "[\"\\u004G\"]"))  ; \u 后包含非十六进制字符
(check-catch 'parse-error (string->json "[\"\\a\"]"))
(check (string->json "") => (eof-object))
(check (string->json ".") => (eof-object))
(check-catch 'read-error (string->json "["))

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
- 无效数据结构（非法 JSON 数据，如循环引用、不支持的类型等）抛出 value-error 异常
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

(check (json->string #()) => "[]")
(check (json->string #(1 2 3)) => "[1,2,3]")
(check (json->string #("a" "b")) => "[\"a\",\"b\"]")
(check (json->string #(1 "a" true null)) => "[1,\"a\",true,null]")
(check (json->string '(("name" . "Alice"))) => "{\"name\":\"Alice\"}")
;; 多个键值对
(check (json->string '(("id" . 1) ("active" . true))) => "{\"id\":1,\"active\":true}")
(check (json->string '((name . "Bob"))) => "{name:\"Bob\"}")
(check (json->string '((x . 10) (y . 20))) => "{x:10,y:20}")
(check (json->string #((("id" . 1)) (("id" . 2)))) => "[{\"id\":1},{\"id\":2}]")
(check (json->string '(("scores" . #(85 90 95)))) => "{\"scores\":[85,90,95]}")
(check (json->string 
         '(("user" . (("name" . "Dave") 
                      ("tags" . #("admin" "editor")))))) 
       => "{\"user\":{\"name\":\"Dave\",\"tags\":[\"admin\",\"editor\"]}}")
(check (json->string '(("text" . "Line1\nLine2"))) => "{\"text\":\"Line1\\nLine2\"}")
(check (json->string #( "He said \"Hello\"" )) => "[\"He said \\\"Hello\\\"\"]")

(check-catch 'value-error (json->string '((a))))


#|
json-set
设置JSON对象中指定键的值。

语法
----
(json-set json key1 key2 ... value)

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
- 支持链式调用 (通过嵌套函数调用)
|#
; json-set
; 单层，键为符号
(let* ((j0 `((age . 18) (sex . male)))
       (j1 (json-set j0 'age 19))
       (j2 (json-set j0 'age 'null)))
  (check (json-ref j0 'age) => 18)
  (check (json-ref j1 'age) => 19)
  (check (json-ref j2 'age) => 'null))

; 单层，键为字符串
(let* ((j0 `(("age" . 18) ("sex" . male)))
       (j1 (json-set j0 "age" 19)))
  (check (json-ref-number j1 "age" 0) => 19)
  (check (json-ref j0 "age") => 18))

; 单层，键为整数 (Array set)
(let* ((j0 #(red green blue))
       (j1 (json-set j0 0 'black)))
  (check j0 => #(red green blue))
  (check j1 => #(black green blue)))

; 单层，键为布尔值 (不常见，但测试覆盖)
(let* ((j0 '((bob . 18) (jack . 16)))
       (j1 (json-set j0 #t 3))
       (j2 (json-set j0 #t (lambda (x) (+ x 1)))))
  (check j1 => '((bob . 3) (jack . 3)))
  (check j2 => '((bob . 19) (jack . 17))))

; 多层，键为符号
(let* ((j0 '((person . ((name . "Alice") (age . 25)))))
       (j1 (json-set j0 'person 'age 26)))
  (check (json-ref j1 'person 'age) => 26))

; 多层，键为字符串
(let* ((j0 '((person . ((name . "Alice")
                        (age . 25)
                        (address . ((city . "Wonderland")
                                    (zip . "12345")))))))
       (j1 (json-set j0 'person 'address 'city "Newland")))
  (check (json-ref j1 'person 'address 'city) => "Newland"))

; 单层，最后一个参数不是值，而是一个函数
(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-set j0 'age (lambda (x) (+ x 1)))))
  (check (json-ref j1 'age) => 26))

; 多层，最后一个参数不是值，而是一个函数
(let* ((j0 '((person . ((name . "Alice") (age . 25)))))
       (j1 (json-set j0 'person 'age (lambda (x) (+ x 1)))))
  (check (json-ref j1 'person 'age) => 26))

; set with nested structure
(let* ((j0 `((age . 18) (sex . male)))
       (j1 20)
       (j2 (json-set j0 'age j1)))
  (check (json-ref j2 'age) => 20))

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 26)
       (j2 (json-set j0 'person 'age j1)))
  (check (json-ref j2 'person 'age) => 26))

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 `((name . "Bob") (age . 30)))
       (j2 (json-set j0 'person j1)))
  (check (json-ref j2 'person 'name) => "Bob")
  (check (json-ref j2 'person 'age) => 30))

#|
json-ref-boolean
获取JSON对象中的布尔值，如果值不是布尔类型则返回默认值。

语法
----
(json-ref-boolean json key default-value)

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
; json-ref-boolean
(let* ((j0 '((active . #t) (verified . #f) (name . "Alice"))))
  (check (json-ref-boolean j0 'active #f) => #t)
  (check (json-ref-boolean j0 'verified #t) => #f)
  (check (json-ref-boolean j0 'name #f) => #f)
  (check (json-ref-boolean j0 'nonexistent #t) => #t))

#|
json-reduce (Transform)
转换JSON对象中指定键的值。

语法
----
(json-reduce json key1 key2 ... transform-fn)
(json-reduce json predicate-fn transform-fn)
(json-reduce json #t transform-fn)
(json-reduce json #f transform-fn)

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
; json-reduce
(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-reduce j0 'name (lambda (k v) (string-upcase v)))))
  (check (json-ref j1 'name) => "ALICE")
  (check (json-ref j1 'age) => 25))
  
(let* ((j0 '((person . ((name . "Alice") (age . 25)))))
       (j1 (json-reduce j0 'person (lambda (k v) v))))
  (check (json-ref j1 'person) => '((name . "Alice") (age . 25))))
  
(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-reduce j0 (lambda (k) (equal? k 'age)) (lambda (k v) (+ v 1)))))
  (check (json-ref j1 'age) => 26)
  (check (json-ref j1 'name) => "Alice"))
  
(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-reduce j0 #t (lambda (k v) (if (string? v) (string-upcase v) v)))))
  (check (json-ref j1 'name) => "ALICE")
  (check (json-ref j1 'age) => 25))
  
(let* ((j0 '((name . "Alice") (age . 25)))
       (j1 (json-reduce j0 #f (lambda (k v) v))))
  (check (json-ref j1 'name) => "Alice")
  (check (json-ref j1 'age) => 25))
  

; Test json-reduce with multiple nested levels
(let* ((j0 '((user . ((profile . ((contact . ((email . "alice@example.com")
                                              (phone . "123-456-7890")))))))))
       (j1 (json-reduce j0 'user 'profile 'contact 'email 
                         (lambda (k v) (string-append v ".verified")))))
  (check (json-ref j1 'user 'profile 'contact 'email) => "alice@example.com.verified"))

; Test json-reduce for conditional transformation with predicate function
(let* ((j0 '((user . ((data . ((scores . #(85 90 78 92 88))
                               (settings . ((notifications . #t)
                                            (theme . "dark")))))))))
       (j1 (json-reduce j0 'user 'data 
                         (lambda (k) (equal? k 'scores)) 
                         (lambda (k v) (vector-map (lambda (score) (+ score 5)) v)))))
  (check (json-ref j1 'user 'data 'scores) => #(90 95 83 97 93))
  (check (json-ref j1 'user 'data 'settings 'theme) => "dark"))

; Compare transform (reduce) and set
(let* ((j0 '((user . ((profile . ((name . "Alice")
                                  (age . 25)
                                  (scores . #(85 90 78))))))))
       (j1 (json-reduce j0 'user 'profile 'scores (lambda (k v) 
                                                  (vector-map (lambda (score) (+ score 5)) v))))
       (j2 (json-set j0 'user 'profile 'scores #(90 95 83))))
  (check (json-ref j1 'user 'profile 'scores) => #(90 95 83))
  (check (json-ref j2 'user 'profile 'scores) => #(90 95 83))
  (check (json-ref j1 'user 'profile 'name) => "Alice")
  (check (json-ref j2 'user 'profile 'name) => "Alice"))

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
  (let ((updated-json (json-reduce json 'person 'address 'city (lambda (x y) (string-upcase y)))))
    (check (json-ref updated-json 'person 'address 'city) => "WONDERLAND")))



#|
json-push
向JSON对象中添加新的键值对。

语法
----
(json-push json key1 key2 ... value)

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
- 如果键已存在，会覆盖原有值 (对于对象)
- 如果键不存在，会创建新的键值对
- 支持符号、字符串、数字和布尔值作为键
|#
; json-push
; 多层，键为符号
(let* ((j0 '((person . ((name . "Alice") (age . 25)))))
       (j1 (json-push j0 'person 'city "Wonderland")))
  (check (json-ref j1 'person 'city) => "Wonderland"))

; 多层，键为字符串
(let* ((j0 '(("person" . (("name" . "Alice") ("age" . 25)))))
       (j1 (json-push j0 "person" "city" "Wonderland")))
  (check (json-ref j1 "person" "city") => "Wonderland"))

; 多层，键为符号
(let* ((j0 '((person . ((name . "Alice")
                        (age . 25)
                        (address . ((city . "Oldland")
                                    (zip . "12345")))))))
       (j1 (json-push j0 'person 'address 'street "Main St")))
  (check (json-ref j1 'person 'address 'street) => "Main St"))

; 多层，JSON是向量
(let* ((j0 '((data . #(1 2 3))))
       (j1 (json-push j0 'data 3 4)))
  (check (json-ref j1 'data) => #(1 2 3 4)))

; 多层，JSON是二维向量
(let* ((j0 '((data . #(#(1 2) #(3 4)))))
       (j1 (json-push j0 'data 1 2 5)))
  ;; 索引1是 #(3 4)，push key 2 val 5 -> #(3 4 5)
  (check (json-ref j1 'data) => #(#(1 2) #(3 4 5))))

(let* ((j0 '((flags . ((#t . "true") (#f . "false")))))
       (j1 (json-push j0 'flags #t "yes")))
  (check (json-ref j1 'flags #t) => "yes"))



#|
json-drop (Deep Drop)
从 JSON 数据结构中删除指定的元素。

语法
----
1. 路径删除模式（Deep Delete）：
   (json-drop json key1 key2 ... target-key)

2. 谓词删除模式（Shallow Filter）：
   (json-drop json predicate-fn)

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
返回一个新的数据结构，其中指定的元素已被移除。

功能
----
1. **路径删除**：
   - 支持多层嵌套定位。
   - 就像文件系统路径一样，精准打击并删除路径末端的一个元素。
   - 如果路径不存在，操作无效，返回原对象。

2. **谓词删除**：
   - 仅作用于**当前层级**（浅层）。
   - 批量删除所有满足条件的项。

示例
----
;; 路径删除：删除 person 下 address 里的 zip 字段
(json-drop j 'person 'address 'zip)

;; 谓词删除（对象）：删除所有键名为 string 类型或特定名称的键
(json-drop j (lambda (k) (eq? k 'age))) 
|#



(let* ((json '((name . "Alice") (age . 25))))
  (let ((updated-json (json-drop json 'age)))
    (check (json-ref updated-json 'age) => '())))

(let* ((json '((name . "Alice")
               (age . 25)
               (address . ((city . "Wonderland")
                           (zip . "12345"))))))
  (let ((updated-json (json-drop json 'address 'city)))
    (check (json-ref updated-json 'address 'city) => '())))

(let* ((json '((name . "Alice")
               (age . 25)
               (address . ((city . "Wonderland")
                           (zip . "12345"))))))
  (let ((j1 (json-drop json (lambda (k) (equal? k 'city)))))
    (check (json-ref j1 'address 'city) => "Wonderland")) ; city在address下，顶层drop不影响
  (let ((j2 (json-drop json (lambda (k) (equal? k 'name)))))
    (check (json-ref j2 'name) => '()))
  (let ((j3 (json-drop json 'address (lambda (k) (equal? k 'city)))))
    (check (json-ref j3 'address 'city) => '())))

(let* ((j0 '((name . "Alice") (age . 25) (city . "Wonderland")))
       (j1 (json-drop j0 'age)))
  (check (json-ref j1 'age) => '())
  (check (json-ref j1 'name) => "Alice")
  (check (json-ref j1 'city) => "Wonderland"))

(let* ((j0 '((user . ((profile . ((name . "Alice")
                                  (age . 25)
                                  (scores . #(85 90 78))))))))
       (j1 (json-drop j0 'user 'profile 'scores)))
  (check (json-ref j1 'user 'profile 'scores) => '())
  (check (json-ref j1 'user 'profile 'name) => "Alice")
  (check (json-ref j1 'user 'profile 'age) => 25))

(let* ((j0 '((data . #(1 2 3 4 5))))
       (j1 (json-drop j0 'data (lambda (k) (and (number? k) (even? k))))))
  ;; 删除偶数索引: 0(1), 2(3), 4(5). 剩下索引 1(2), 3(4).
  (check (json-ref j1 'data) => #(2 4)))

(let* ((j0 '((settings . (("theme" . "dark")
                          (notifications . #t)
                          ("language" . "en")))))
       (j1 (json-drop j0 'settings (lambda (k) (string? k)))))
  (check (json-ref j1 'settings "theme") => '())
  (check (json-ref j1 'settings "language") => '()))

(let* ((j0 '((a . 1) (b . 2) (c . 3)))
       (j1 (json-drop j0 (lambda (k) (member k '(a c))))))
  (check (json-ref j1 'a) => '())
  (check (json-ref j1 'b) => 2)
  (check (json-ref j1 'c) => '()))


(let* ((j0 #())
       (j1 (json-drop j0 0)))
  (check j1 => #()))



(let ((json '((person . ((name . "Alice")
                         (age . 25)
                         (address . ((city . "Wonderland")
                                     (zip . "12345"))))))))
  (let ((updated-json (json-reduce json 'person 'address 'city (lambda (x y) (string-upcase y)))))
    (check (json-ref updated-json 'person 'address 'city) => "WONDERLAND")))


; json-push with objects
(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 "Wonderland")
       (j2 (json-push j0 'person 'city j1)))
  (check (json-ref j2 'person 'city) => "Wonderland"))

(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 `((city . "Wonderland") (zip . "12345")))
       (j2 (json-push j0 'person 'address j1)))
  (check (json-ref j2 'person 'address 'city) => "Wonderland")
  (check (json-ref j2 'person 'address 'zip) => "12345"))



; Test with nested objects
(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 `((address . ((city . "Wonderland") (zip . "12345")))))
       (j2 (json-set j0 'person j1)))
  (check (json-ref j2 'person 'address 'city) => "Wonderland")
  (check (json-ref j2 'person 'address 'zip) => "12345"))

; Test with mixed objects and primitive values
(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 "Wonderland")
       (j2 (json-set (json-push j0 'person 'city j1) 'person 'age 26)))
  (check (json-ref j2 'person 'city) => "Wonderland")
  (check (json-ref j2 'person 'age) => 26))

; Test with null
(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 'null)
       (j2 (json-set j0 'person 'age j1)))
  (check (json-ref j2 'person 'age) => 'null))

; Test with boolean
(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 'true)
       (j2 (json-push j0 'person 'active j1)))
  (check (json-ref j2 'person 'active) => #t))

; Test with array
(let* ((j0 `((person . ((name . "Alice") (age . 25)))))
       (j1 #(1 2 3))
       (j2 (json-push j0 'person 'scores j1)))
  (check (json-ref j2 'person 'scores) => #(1 2 3)))

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

(define sample-j
  '((user . ((id . 1001)
             (name . "Alice")
             (active . #t)
             (email . null)
             (tags . #("dev" "scheme" "json"))
             (profile . ((age . 21)
                         (height . 168.5)
                         (hobbies . #("music" "reading"))))))
    (scores . #(98 87 93))))
(check (json->string sample-j)
  => "{user:{id:1001,name:\"Alice\",active:true,email:null,tags:[\"dev\",\"scheme\",\"json\"],profile:{age:21,height:168.5,hobbies:[\"music\",\"reading\"]}},scores:[98,87,93]}")

#|
Error Handling Tests
|#

;; json-ref invalid structure
(check-catch 'type-error (json-ref "not-a-json" 'key))
(check-catch 'type-error (json-ref 123 'key))

;; json-set invalid structure
(check-catch 'type-error (json-set "not-a-json" 'key "val"))
(check-catch 'type-error (json-set 123 'key "val"))

;; json-push invalid structure
(check-catch 'type-error (json-push "not-a-json" 'key "val"))
(check-catch 'type-error (json-push 123 'key "val"))

;; json-drop invalid structure
(check-catch 'type-error (json-drop "not-a-json" 'key))
(check-catch 'type-error (json-drop 123 'key))

;; json-reduce invalid structure
(check-catch 'type-error (json-reduce "not-a-json" 'key (lambda (k v) v)))
(check-catch 'type-error (json-reduce 123 'key (lambda (k v) v)))

(check-report)
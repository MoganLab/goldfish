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
        (scheme write)
) ;import

(check-set-mode! 'report-failed)

(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define

#|
write
将对象按可读回的 Scheme 表示写入输出端口。

语法
----
(write obj)
(write obj port)

参数
----
obj : any
  要输出的对象。
port : output-port? 可选
  输出端口。省略时，写入当前输出端口。

返回值
----
unspecified
  主要用于副作用输出。

说明
----
1. `write` 面向“可读回”的文本表示。
2. 字符串会带双引号。
3. 列表、符号等按 Scheme 语法形式输出。
|#

(check-true (procedure? write))

(check (capture-output
         (lambda (port)
           (write '(1 2 3) port)
         ) ;lambda
       ) ;capture-output
       => "(1 2 3)"
) ;check

(check (capture-output
         (lambda (port)
           (write "goldfish" port)
         ) ;lambda
       ) ;capture-output
       => "\"goldfish\""
) ;check

(check (capture-output
         (lambda (port)
           (write 'hello-world port)
         ) ;lambda
       ) ;capture-output
       => "hello-world"
) ;check

#|
display
将对象按更接近用户阅读的方式写入输出端口。

语法
----
(display obj)
(display obj port)

参数
----
obj : any
  要展示的对象。
port : output-port? 可选
  输出端口。省略时，写入当前输出端口。

返回值
----
unspecified
  主要用于副作用输出。

说明
----
1. `display` 更偏向面向用户的展示输出。
2. 对字符串通常不输出双引号。
3. 对符号和数字等对象则输出其展示形式。
|#

(check-true (procedure? display))

(check (capture-output
         (lambda (port)
           (display "goldfish" port)
         ) ;lambda
       ) ;capture-output
       => "goldfish"
) ;check

(check (capture-output
         (lambda (port)
           (display 42 port)
         ) ;lambda
       ) ;capture-output
       => "42"
) ;check

(check (capture-output
         (lambda (port)
           (display 'hello port)
         ) ;lambda
       ) ;capture-output
       => "hello"
) ;check

#|
newline
向输出端口写入一个换行符。

语法
----
(newline)
(newline port)

参数
----
port : output-port? 可选
  输出端口。省略时，写入当前输出端口。

返回值
----
unspecified
  主要用于副作用输出。

说明
----
`newline` 常用于组织多行输出，与 `display` 或 `write` 搭配使用。
|#

(check-true (procedure? newline))

(check (capture-output
         (lambda (port)
           (newline port)
         ) ;lambda
       ) ;capture-output
       => "\n"
) ;check

(check (capture-output
         (lambda (port)
           (display "a" port)
           (newline port)
           (display "b" port)
         ) ;lambda
       ) ;capture-output
       => "a\nb"
) ;check

#|
write-char
向输出端口写入一个字符。

语法
----
(write-char char)
(write-char char port)

参数
----
char : char?
  要输出的字符。
port : output-port? 可选
  输出端口。省略时，写入当前输出端口。

返回值
----
unspecified
  主要用于副作用输出。

错误处理
--------
wrong-type-arg
  当第一个参数不是字符时抛出。
|#

(check-true (procedure? write-char))

(check (capture-output
         (lambda (port)
           (write-char #\A port)
         ) ;lambda
       ) ;capture-output
       => "A"
) ;check

(check (capture-output
         (lambda (port)
           (write-char #\space port)
           (write-char #\B port)
         ) ;lambda
       ) ;capture-output
       => " B"
) ;check

(check-catch 'wrong-type-arg
  (let ((port (open-output-string)))
    (write-char 1 port)
  ) ;let
) ;check-catch

#|
write-simple
在当前实现中，提供与 `write` 一致的兼容输出行为。

语法
----
(write-simple obj)
(write-simple obj port)

参数
----
obj : any
  要输出的对象。
port : output-port? 可选
  输出端口。省略时，写入当前输出端口。

返回值
----
unspecified
  主要用于副作用输出。

说明
----
当前底层没有独立的 `write-simple` 原生过程，因此这里验证它与 `write`
保持一致的现有兼容行为。
|#

(check-true (procedure? write-simple))

(check (capture-output
         (lambda (port)
           (write-simple '(a b) port)
         ) ;lambda
       ) ;capture-output
       => "(a b)"
) ;check

(check (capture-output
         (lambda (port)
           (write-simple "goldfish" port)
         ) ;lambda
       ) ;capture-output
       => "\"goldfish\""
) ;check

#|
write-shared
在当前实现中，提供与 `write` 一致的兼容输出行为。

语法
----
(write-shared obj)
(write-shared obj port)

参数
----
obj : any
  要输出的对象。
port : output-port? 可选
  输出端口。省略时，写入当前输出端口。

返回值
----
unspecified
  主要用于副作用输出。

说明
----
当前底层没有独立的 `write-shared` 原生过程，因此这里验证它与 `write`
保持一致的现有兼容行为。
|#

(check-true (procedure? write-shared))

(check (capture-output
         (lambda (port)
           (write-shared '(a b) port)
         ) ;lambda
       ) ;capture-output
       => "(a b)"
) ;check

(check (capture-output
         (lambda (port)
           (write-shared "goldfish" port)
         ) ;lambda
       ) ;capture-output
       => "\"goldfish\""
) ;check

(check-report)

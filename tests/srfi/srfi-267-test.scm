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
        (srfi srfi-267)
) ;import

(check-set-mode! 'report-failed)

(define (capture-read-error thunk)
  (guard (err
          ((raw-string-read-error? err) err)
          (else 'unexpected-error))
    (thunk)
    'no-error)
) ;define

(define (capture-write-error thunk)
  (guard (err
          ((raw-string-write-error? err) err)
          (else 'unexpected-error))
    (thunk)
    'no-error)
) ;define

;; =======================================
;; Reader syntax
;; =======================================

; 基本 raw string 字面量
(check #"""" => "")
(check #""a"" => "a")
(check #"-"""-" => "\"")
(check #"tag with space"hello"tag with space" => "hello")
(check #"(())"#""a"""(())" => "#\"\"a\"\"")

; 规范中的歧义情况：#""""" 解析为 #"""" 加上后续普通 "
(let ((port (open-input-string "#\"\"\"\"\"")))
  (check (read-raw-string port) => "")
  (check (read-char port) => #\")
) ;let

;; =======================================
;; raw-string-read-error?
;; =======================================

#|
raw-string-read-error?
判断对象是否为 raw string 读取错误。

语法
----
(raw-string-read-error? obj)

参数
----
obj : any
    任意对象，通常为通过 catch/guard 捕获的错误对象

返回值
----
boolean
    若 obj 表示 raw string 读取错误，则返回 #t
|#

(check-false (raw-string-read-error? 'plain-symbol))

(let ((err (capture-read-error
            (lambda ()
              (read-raw-string (open-input-string "#x"))))))
  (check (raw-string-read-error? err) => #t)
) ;let

;; =======================================
;; raw-string-write-error?
;; =======================================

#|
raw-string-write-error?
判断对象是否为 raw string 写出错误。

语法
----
(raw-string-write-error? obj)

参数
----
obj : any
    任意对象，通常为通过 catch/guard 捕获的错误对象

返回值
----
boolean
    若 obj 表示 raw string 写出错误，则返回 #t
|#

(check-false (raw-string-write-error? 'plain-symbol))

(let ((err (capture-write-error
            (lambda ()
              (write-raw-string "\"" "" (open-output-string))))))
  (check (raw-string-write-error? err) => #t)
) ;let

;; =======================================
;; read-raw-string
;; =======================================

#|
read-raw-string
从输入端口读取完整的 raw string 字面量。

语法
----
(read-raw-string [input-port])

参数
----
input-port : input-port, optional
    输入端口；省略时使用当前输入端口

返回值
----
string
    读取到的字符串内容，不做转义处理
|#

(check (read-raw-string (open-input-string "#\"\"a\"\"")) => "a")

(check
 (read-raw-string
  (open-input-string "#\"tag with space\"hello\"tag with space\""))
 => "hello")

(check
 (read-raw-string
  (open-input-string "#\"(())\"#\"\"a\"\"\"(())\""))
 => "#\"\"a\"\"")

(check-catch 'raw-string-read-error
             (read-raw-string (open-input-string "#x")))

;; =======================================
;; read-raw-string-after-prefix
;; =======================================

#|
read-raw-string-after-prefix
从 #" 之后的位置继续读取 raw string。

语法
----
(read-raw-string-after-prefix [input-port])

参数
----
input-port : input-port, optional
    输入端口；读取位置应位于 #" 之后

返回值
----
string
    读取到的字符串内容
|#

(check (read-raw-string-after-prefix (open-input-string "\"a\"\"")) => "a")

(check
 (read-raw-string-after-prefix
  (open-input-string "tag with space\"hello\"tag with space\""))
 => "hello")

(check
 (read-raw-string-after-prefix
  (open-input-string "(())\"#\"\"a\"\"\"(())\""))
 => "#\"\"a\"\"")

(check-catch 'raw-string-read-error
             (read-raw-string-after-prefix (open-input-string "unterminated")))

;; =======================================
;; can-delimit?
;; =======================================

#|
can-delimit?
检查某个 delimiter 是否可以无歧义地包裹给定字符串。

语法
----
(can-delimit? string delimiter)

参数
----
string : string
    要编码为 raw string 的字符串
delimiter : string
    候选分隔符，不能包含双引号

返回值
----
boolean
    若 delimiter 可以合法包裹 string，则返回 #t
|#

(check (can-delimit? "abc" "") => #t)
(check (can-delimit? "\"" "") => #f)
(check (can-delimit? "\"\"" "") => #f)
(check (can-delimit? "\"\"" "-") => #t)
(check (can-delimit? "abc" "\"") => #f)
(check (can-delimit? "abc" "tag with space") => #t)
(check (can-delimit? "abc" "(())") => #t)

;; =======================================
;; generate-delimiter
;; =======================================

#|
generate-delimiter
为指定字符串自动生成可用的 delimiter。

语法
----
(generate-delimiter string)

参数
----
string : string
    要编码为 raw string 的字符串

返回值
----
string
    某个合法 delimiter，满足 can-delimit? 条件
|#

(let* ((payload "\"-\" \"--\" \"---\" \"----\"")
       (delimiter (generate-delimiter payload)))
  (check (string? delimiter) => #t)
  (check (can-delimit? payload delimiter) => #t)
) ;let*

(let* ((payload "#\"\"a\"\" and \"quotes\"")
       (delimiter (generate-delimiter payload)))
  (check (string? delimiter) => #t)
  (check (can-delimit? payload delimiter) => #t)
) ;let*

;; =======================================
;; write-raw-string
;; =======================================

#|
write-raw-string
将字符串按 raw string 字面量格式写入输出端口。

语法
----
(write-raw-string string delimiter [output-port])

参数
----
string : string
    要写出的字符串内容
delimiter : string
    使用的 delimiter
output-port : output-port, optional
    输出端口；省略时使用当前输出端口

返回值
----
未指定
    副作用为向端口写出 raw string 表示
|#

(let ((out (open-output-string)))
  (write-raw-string "hello" "" out)
  (check (get-output-string out) => "#\"\"hello\"\"")
) ;let

(let* ((payload "#\"\"a\"\" and \"quotes\"")
       (delimiter (generate-delimiter payload))
       (out (open-output-string)))
  (write-raw-string payload delimiter out)
  (check (read-raw-string (open-input-string (get-output-string out))) => payload)
) ;let*

(check-catch 'raw-string-write-error
             (write-raw-string "\"" "" (open-output-string)))

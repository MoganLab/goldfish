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
        (scheme read)
) ;import

(check-set-mode! 'report-failed)

#|
read
从当前输入端口或指定输入端口中读取一个完整的 Scheme datum。

语法
----
(read)
(read port)

参数
----
port : input-port? 可选
  输入端口。省略时，从当前输入端口读取。

返回值
----
any
  读取到的 datum。若输入已结束，则返回 EOF 对象。

说明
----
1. `read` 会按 Scheme 语法解析文本输入。
2. 可以读取数字、布尔值、字符串、符号、列表等数据。
3. 每次调用只消费一个完整 datum。
4. 输入端口为空时，返回 EOF 对象。

错误处理
--------
wrong-type-arg
  当 `port` 不是输入端口时抛出。
read-error
  当输入不是合法的 Scheme datum 时抛出。
|#

(check-true (procedure? read))

; 从当前输入端口读取数字
(check (with-input-from-string "123"
         (lambda ()
           (read)
         ) ;lambda
       ) ;with-input-from-string
       => 123
) ;check

; 从当前输入端口连续读取多个 datum
(check (with-input-from-string "1 2"
         (lambda ()
           (list (read)
                 (read)
           ) ;list
         ) ;lambda
       ) ;with-input-from-string
       => '(1 2)
) ;check

; 读取布尔值
(check (with-input-from-string "#t"
         (lambda ()
           (read)
         ) ;lambda
       ) ;with-input-from-string
       => #t
) ;check

; 读取字符串
(check (let ((port (open-input-string "\"goldfish\"")))
         (read port)
       ) ;let
       => "goldfish"
) ;check

; 读取符号
(check (let ((port (open-input-string "hello-world")))
         (read port)
       ) ;let
       => 'hello-world
) ;check

; 读取列表
(check (let ((port (open-input-string "(1 2 (3 4))")))
         (read port)
       ) ;let
       => '(1 2 (3 4))
) ;check

; 读取空列表
(check (let ((port (open-input-string "()")))
         (read port)
       ) ;let
       => '()
) ;check

; 空输入返回 EOF 对象
(check-true
  (let ((port (open-input-string "")))
    (eof-object? (read port))
  ) ;let
) ;check-true

; 非输入端口应报错
(check-catch 'wrong-type-arg (read 123))

(check-report)

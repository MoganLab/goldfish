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
        (liii goldfix)
) ;import

(check-set-mode! 'report-failed)

#|
count-content-paren-issues
统计一段 Scheme 内容里多出来的右括号数量和缺失的右括号数量。

语法
----
(count-content-paren-issues content)

返回值
----
pair
  返回 `(extra-right-parens . missing-right-parens)`。

说明
----
1. 会忽略字符串中的括号。
2. 会忽略注释中的括号。
3. 会忽略块注释和字符字面量中的括号。
|#

(check (count-content-paren-issues "") => '(0 . 0))
(check (count-content-paren-issues "(define x 1)") => '(0 . 0))
(check (count-content-paren-issues "(()") => '(0 . 1))
(check (count-content-paren-issues "())") => '(1 . 0))
(check (count-content-paren-issues "())(") => '(1 . 1))
(check (count-content-paren-issues "))((") => '(2 . 2))

; 忽略字符串里的括号
(check (count-content-paren-issues "(display \"(\")") => '(0 . 0))
(check (count-content-paren-issues "(display \")\")") => '(0 . 0))

; 忽略行注释里的括号
(check (count-content-paren-issues "(define x 1) ; )))") => '(0 . 0))

; 忽略块注释里的括号
(check (count-content-paren-issues "#| ( ) )) (( |#") => '(0 . 0))
(check (count-content-paren-issues "(define x 1) #| ) ) )\n( ( ( |#") => '(0 . 0))

; 忽略字符字面量里的括号
(check (count-content-paren-issues "(list #\\( #\\))") => '(0 . 0))

; 多行内容
(check (count-content-paren-issues "(define (f x)\n  (+ x 1))") => '(0 . 0))
(check (count-content-paren-issues "(define (f x)\n  (+ x 1)") => '(0 . 1))
(check (count-content-paren-issues "(define (f x))\n  (+ x 1))") => '(1 . 0))
(check (count-content-paren-issues "; )))\n(()") => '(0 . 1))

(check-report)

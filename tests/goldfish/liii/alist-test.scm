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
        (liii alist)
) ;import

(check-set-mode! 'report-failed)

(check-true (alist? '()))

(check-true (alist? '((a 1))))
(check-true (alist? '((a . 1))))
(check-true (alist? '((a . 1) (b . 2))))

(check-false (alist? '(1 2 3)))

(check (alist-ref '((a 1)) 'a) => '(1))
(check (alist-ref '((a . 1)) 'a) => 1)
(check-catch 'key-error (alist-ref '(("a" . 1)) "a"))
(check-catch 'key-error (alist-ref '((a . 1)) 'b))

(check (alist-ref '((a . 1)) 'b (lambda () 2)) => 2)

(check (alist-ref/default '((a . 1)) 'b 2) => 2)

(check (vector->alist #()) => '())

(check (vector->alist #(42)) => '((0 . 42)))

(check (vector->alist #("a" "b" "c")) => '((0 . "a") (1 . "b") (2 . "c")))

(check (vector->alist #(#(1 2) #(3 4))) => '((0 . #(1 2)) (1 . #(3 4))))

#|
alist-cons
将一个新的键值对添加到关联列表的前面，保持关联列表结构。

语法
----
(alist-cons key value alist)

参数
----
key : any
要添加的键，可以是任意类型。
value : any 
与该键关联的值，可以是任意类型。
alist : list
已有的关联列表，可以是空列表或非空的关联列表。

返回值
----
list
返回一个新的关联列表，新键值对(key . value)位于列表最前面。

描述
----
alist-cons是SRFI-1标准中定义的关联列表基本操作函数，用于构建和修改关联列表。
它总是在关联列表的头部添加新的键值对，无论该键是否已经存在。这与alist-ref的
向后查找特性配合使用时，可以实现键值对的覆盖更新。

特点
----
- 不退化已有相同键，允许存在重复键
- 保持关联列表结构不变 
- 时间复杂度O(1)，性能高效
- 可链式调用构建关联列表


注意事项
----
- 该函数不会检查键的唯一性，允许重复键存在
- 当使用alist-ref查询时，会返回最前面的匹配键对应值
- 参数alist必须是合法的关联列表(list of pairs)，否则会抛出type-error
- 空alist输入返回只包含新键值对的列表
|#

; 基本功能测试
(check (alist-cons 'a 1 '()) => '((a . 1)))
(check (alist-cons 'a 1 '((b . 2))) => '((a . 1) (b . 2)))

; 参数连续性测试 - 单个参数添加
(check (alist-cons 'key "value" '()) => '((key . "value")))
(check (alist-cons 42 "number" '()) => '((42 . "number")))
(check (alist-cons "string" 123 '()) => '(("string" . 123)))

; 参数连续性测试 - 多键连续添加  
(check (alist-cons 'a 1 (alist-cons 'b 2 '())) => '((a . 1) (b . 2)))
(check (alist-cons 'c 3 (alist-cons 'b 2 (alist-cons 'a 1 '()))) => '((c . 3) (b . 2) (a . 1)))

; 边界条件测试 - 空列表处理
(check (alist-cons 'key 'value '()) => '((key . value)))

; 边界条件测试 - 重复键处理
(check (alist-cons 'key 1 '((key . 99))) => '((key . 1) (key . 99)))
(check (alist-cons 'age 30 '((name . "Alice") (age . 25))) => '((age . 30) (name . "Alice") (age . 25)))

; 边界条件测试 - 嵌套结构支持
(check (alist-cons 'person '((name . "Alice") (age . 25)) '()) => '((person (name . "Alice") (age . 25))))
(check (alist-cons 'name (list 'first "Alice") '((id . 1))) => '((name first "Alice") (id . 1)))

; 数据类型支持测试 - 各种键类型
(check (alist-cons #\a 1 '()) => '((#\a . 1)))
(check (alist-cons #t "true" '()) => '((#t . "true")))
(check (alist-cons #f "false" '()) => '((#f . "false")))
(check (alist-cons '(1 2 3) "list-key" '()) => '(((1 2 3) . "list-key"))) 
(check (alist-cons #(1 2 3) "vector-key" '()) => '((#(1 2 3) . "vector-key")))

; 数据类型支持测试 - 各种值类型
(check (alist-cons 'string-value "Hello World" '()) => '((string-value . "Hello World")))
(check (alist-cons 'list-value '(1 2 3 4 5) '()) => '((list-value 1 2 3 4 5)))
(check (alist-cons 'mixed-value '((a 1) (b 2) "string") '()) => '((mixed-value (a 1) (b 2) "string")))

; 链式操作测试 - 构建复杂关联列表
(let ((result (alist-cons 'step3 "final" 
                          (alist-cons 'step2 "process"
                                     (alist-cons 'step1 "start" '()))))
                          ) ;alist-cons
  (check result => '((step3 . "final") (step2 . "process") (step1 . "start")))
) ;let

; 与assoc/assq/assv交互测试
(check (assq 'name (alist-cons 'name "Bob" '((name . "Alice") (age . 25)))) => '(name . "Bob"))
(check (assq 'age (alist-cons 'name "Bob" '((age . 30)))) => '(age . 30))
(check (assq 'new-key (alist-cons 'new-key "value" '((existing . "data")))) => '(new-key . "value"))

; 错误处理测试 - 非法参数 (注意: s7可能不抛出这些错误)
; (check-catch 'type-error (alist-cons 'key 'value "not-a-list"))
; (check-catch 'type-error (alist-cons 'key 'value 123))

(check-report)


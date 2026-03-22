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

(import (liii oop) (liii check) (liii error) (liii base) (liii case) (liii rich-string))
(check-set-mode! 'report-failed)

#|
@
创建一个部分应用函数，允许指定部分参数，使用下划线 `_` 作为占位符。

语法
----
(@ func arg1 arg2 ...)

参数
----
func : procedure
要部分应用的函数，可以是任何可调用的过程。

args : any
参数列表，可以包含任意数量的下划线 `_` 作为占位符。

返回值
----
procedure
返回一个新的函数，该函数接受与占位符数量相同的参数，
并将这些参数填充到原函数对应的位置。

描述
----
@ 是 (liii oop) 模块中用于函数式编程的核心宏，它实现了部分应用(partial application)
的功能。通过指定部分参数和占位符，可以创建新的函数，这些函数在调用时会自动将
提供的参数填充到占位符位置。

该宏在定义时计算所有非占位符参数的值，这意味着如果这些参数涉及变量引用，
它们会在定义时被捕获，而不是在调用时重新计算。

特点
----
- 支持任意数量的占位符
- 占位符可以出现在参数列表的任意位置
- 支持嵌套使用，可以组合多个 @ 表达式
- 在定义时计算非占位符参数的值
- 保持原函数的语义和行为

注意事项
----
- 占位符必须使用下划线 `_` 符号
- 返回的函数参数数量必须与占位符数量一致
- 非占位符参数在定义时求值，可能捕获当前环境中的变量值
- 支持任意类型的参数，包括过程、列表、符号等
|#

(check ((@ + _ 2) 1) => 3)
(check ((@ list 1 _ 3 _ 5) 2 4) => (list 1 2 3 4 5))
(check ((@ list _ _) 'a 'b) => (list 'a 'b))

(check (let ((a 10))
         (define add (@ + (* a 2) _))
         (set! a 100)
         (add 5)) => 25)

(let ((x 5))
  (check ((@ cons (+ x 1) _) 'y) => (cons 6 'y))
) ;let

(check (procedure? (@ list 1 2)) => #t)
(check ((@ list 1 2)) => '(1 2))

(check ((@ _ 'a 'b) list) => (list 'a 'b))
(check ((@ map _ '(1 2 3)) (lambda (x) (+ x 1))) => '(2 3 4))
(check ((@ apply _ '(1 2 3)) +) => 6)

(check ((@ (@ + _ 1) _) 2) => 3)
(check ((@ _ _) (@ * _ 2) 3) => 6)

#|
typed-define
定义一个带有类型检查和默认值的函数。

语法
----
(typed-define (name (param1 type-pred1 default1) (param2 type-pred2 default2) ...)
  body-expr
  ...)

参数
----
name : symbol
要定义的函数名称。

param : (symbol predicate [default])
参数定义，包含：
- 参数名称 (symbol)
- 类型谓词 (procedure)，用于参数类型检查
- 可选默认值 (any)，当参数未提供时使用

body-expr : any
函数体表达式，可以包含多个表达式。

返回值
----
procedure
返回一个函数，该函数接受关键字参数，支持类型检查和默认值。

描述
----
typed-define 是 (liii oop) 模块中用于定义类型安全函数的宏。它允许为函数的每个参数
指定类型谓词和默认值，在函数调用时会自动进行类型检查，确保参数类型正确。

该宏生成的函数使用关键字参数调用方式，参数顺序可以任意排列。每个参数都会在运行时
进行类型检查，如果类型不匹配会抛出 'type-error 异常。

特点
----
- 支持运行时类型检查
- 支持参数默认值
- 使用关键字参数调用方式
- 参数顺序可以任意排列
- 提供清晰的错误信息

注意事项
----
- 类型谓词必须是返回布尔值的函数
- 默认值必须符合类型谓词的要求
- 函数调用时必须使用关键字参数语法
- 所有参数都会进行类型检查，包括默认值
- 类型错误会抛出 'type-error 异常
|#

(typed-define (person (name string? "Bob") (age integer?))
  (string-append name " is " (number->string age) " years old")
) ;typed-define

(check (person :age 21) => "Bob is 21 years old")
(check (person :name "Alice" :age 25) => "Alice is 25 years old")
(check-catch 'type-error (person :name 123 :age 25))

;; 测试带有默认值的 typed-define
(typed-define (greet (message string? "Hello") (times integer? 1))
  (apply string-append (make-list times message))
) ;typed-define

(check (greet) => "Hello")
(check (greet :message "Hi" :times 3) => "HiHiHi")
(check-catch 'type-error (greet :times "not-a-number"))


#|
define-case-class
定义类似 Scala 的 case class，提供类型安全的样本类。

语法
----
(define-case-class class-name fields . private-fields-and-methods)

参数
----
class-name : symbol
要定义的 case class 名称。

fields : list
字段定义列表，每个字段格式为 (field-name type-predicate [default-value])。

private-fields-and-methods : any
可选的私有字段和方法定义。

返回值
----
procedure
返回一个函数，该函数可以用于创建 case class 实例或调用静态方法。

描述
----
`define-case-class` 是 (liii oop) 模块中用于定义样本类的核心宏。
它创建类型安全的 case class，支持字段验证、方法分发和不可变数据结构。

字段定义中每个字段由三部分组成：
- field-name: 字段名称（符号）
- type-predicate: 类型断言函数，用于验证字段值的类型
- default-value: 可选，字段的默认值

方法类型包括：
- 静态方法: 以 `@` 开头的函数定义，通过类名调用
- 实例方法: 以 `%` 开头的函数定义，通过实例调用
- 内部方法: 普通函数定义，仅在类内部可见

私有字段使用 `define` 定义，仅在类内部可见。

特点
----
- 类型安全: 创建实例时会自动验证字段类型
- 不可变性: 字段默认不可变，通过关键字参数创建新实例
- 模式匹配: 支持通过字段名访问字段值
- 方法分发: 支持静态方法和实例方法
- 相等性比较: 自动实现 `:equals` 方法
- 字符串表示: 自动实现 `:to-string` 方法
- 类型检查: 自动生成 `:is-type-of` 静态方法

自动生成的方法
----
`define-case-class` 会自动为每个样本类生成以下方法：

**实例方法**
- `:equals` - 相等性比较方法
  - 比较两个样本类实例是否相等
  - 检查两个实例是否为同一类型
  - 使用 `equal?` 比较所有字段的值
  - 如果比较的对象不是样本类实例，会抛出 `type-error`

- `:to-string` - 字符串表示方法
  - 返回样本类实例的字符串表示
  - 格式为 `(class-name :field1 value1 :field2 value2 ...)`

- `:is-instance-of` - 实例类型检查方法
  - 检查实例是否属于指定的类
  - 与 `:is-type-of` 静态方法配合使用

**静态方法**
- `:is-type-of` - 类型检查方法
  - 检查对象是否为该样本类的实例
  - 通过调用对象的 `:is-instance-of` 方法实现
  - 返回布尔值，表示对象是否属于该类

- `:apply` - 实例创建方法
  - 通过位置参数创建样本类实例
  - 参数顺序与字段定义顺序一致
  - 提供位置参数调用的便捷方式

注意事项
----
- 方法名不能与字段名冲突
- 字段类型验证在运行时进行
- 实例方法通过 `%` 前缀定义
- 静态方法通过 `@` 前缀定义
- 私有字段仅在类内部可见
|#

(define-case-class person
  ((name string? "Bob")
   (age integer?)
  ) ;
) ;define-case-class

(let ((bob (person :name "Bob" :age 21)))
  (check (bob 'name) => "Bob")
  (check (bob 'age) => 21)
  (check ((bob :name "hello") 'name) => "hello")
  (check-catch 'value-error (bob 'sex))
  (check-catch 'value-error (bob :sex))
  (check-true (bob :is-instance-of 'person))
  (check-true (person :is-type-of bob))
  (check (bob :to-string) => "(person :name \"Bob\" :age 21)")
) ;let

(check-catch 'type-error (person 1 21))

(let ((bob (person "Bob" 21))
      (get-name (lambda (x)
                 (case* x
                   ((#<procedure?>) (x 'name))
                   (else (value-error))))
                 ) ;case*
      ) ;get-name
  (check (get-name bob) => "Bob")
  (check-catch 'value-error (get-name 1))
) ;let

(define-case-class jerson
  ((name string?)
   (age integer?)
  ) ;
  
  (define (%to-string)
    (string-append "I am " name " " (number->string age) " years old!")
  ) ;define
  (define (%greet x)
    (string-append "Hi " x ", " (%to-string))
  ) ;define
 ;define
) ;define-case-class

(let ((bob (jerson "Bob" 21)))
  (check (bob :to-string) => "I am Bob 21 years old!")
  (check (bob :greet "Alice") => "Hi Alice, I am Bob 21 years old!")
) ;let

(define-case-class anonymous ()
  (define name "")

  (define (%get-name) name)

  (define (%set-name! x)
    (set! name x)
  ) ;define
 ;define
) ;define-case-class

(let ((p (anonymous)))
  (p :set-name! "Alice")
  (check (p :get-name) => "Alice")
) ;let

(define-case-class my-bool ()
  (define data #t)

  (define (%set-true!)
    (set! data #t)
  ) ;define
  (define (%set-false!)
    (set! data #f)
  ) ;define
 
  (define (%true?) data)
  (define (%false?) (not (%true?)))
  
  (define (@apply x)
    (let ((r (my-bool)))
      (cond ((eq? x 'true)
             (r :set-true!))
            ((eq? x 'false)
             (r :set-false!)
            ) ;
            ((boolean? x)
             (if x (r :set-true!) (r :set-false!))
            ) ;
            (else (r :set-false!))
      ) ;cond
      r)
    ) ;let
 ;define
) ;define-case-class

(check-true ((my-bool 'true) :true?))
(check-true ((my-bool 'false) :false?))
(check-true ((my-bool #t) :true?))
(check-true ((my-bool #f) :false?))
(check-true (my-bool :is-type-of (my-bool 'true)))

(define-case-class test-case-class
  ((name string?))
  
  (define (@this-is-a-static-method)
    (test-case-class "static")
  ) ;define
  
  (define (%this-is-a-instance-method)
    (test-case-class (string-append name "instance"))
  ) ;define
 ;define
) ;define-case-class

(let ((hello (test-case-class "hello ")))
  (check-catch 'value-error (hello :this-is-a-static-method))
  (check (test-case-class :this-is-a-static-method) => (test-case-class "static"))
) ;let

(let ()
  (define-case-class person ((name string?) (country string?))
    (define (@default)
      (person "Andy" "China")
    ) ;define
    (define (%set-country! c . xs)
      (set! country c)
      (apply (%this) (if (null? xs) '(:this) xs))
    ) ;define
    (define (%set-name! n . xs)
      (set! name n)
      (apply (%this) (if (null? xs) '(:this) xs))
    ) ;define
    (define (%to-string)
      (format #f "Hello ~a from ~a" name country)
    ) ;define
  ) ;define-case-class

  (define Andy (person :default))
  (check-catch 'wrong-type-arg (person :this))
  (check (Andy :to-string) => "Hello Andy from China")
  (check (Andy :set-country! "USA" :to-string) => "Hello Andy from USA")
  (check (Andy :to-string) => "Hello Andy from USA")
  (check (Andy :set-country! "China" :set-name! "Ancker-0" :to-string) => "Hello Ancker-0 from China")
  (check (Andy :set-country! "China") => (person "Ancker-0" "China"))
  (check (Andy :this :set-country! "USA" :this :set-name! "Andy" :this :to-string) => "Hello Andy from USA")
  (check-true (person :is-type-of Andy))
) ;let

(let ()
  (define-case-class person ((name string?) (country string?))
    (chained-define (@default)
      (person "Andy" "China")
    ) ;chained-define
    (chained-define (set-country! c)
      (set! country c)
      (%this)
    ) ;chained-define
    (chained-define (set-name! n)
      (set! name n)
      (%this)
    ) ;chained-define
    (chained-define (%set-both! n c)
      (set-country! c)
      (set-name! n)
      (%this)
    ) ;chained-define
    (chained-define (%to-string)
      (rich-string (format #f "Hello ~a from ~a" name country))
    ) ;chained-define
  ) ;define-case-class
  (check (person :default :to-string :get) => "Hello Andy from China")
  (check (person :default :set-both! "Bob" "Russia" :to-string :get) => "Hello Bob from Russia")
  (check-catch 'value-error (person :default :set-country! "French"))
) ;let

(check-catch 'syntax-error
  (eval
    '(define-case-class instance-methods-conflict-test
      ((name string?)
       (age integer?))
      (define (%name)
        name))
  ) ;eval
) ;check-catch

(check-catch 'syntax-error
  (eval
    '(define-case-class static-methods-conflict-test
      ((name string?)
       (age integer?))
      (define (@name)
        name))
  ) ;eval
) ;check-catch

(check-catch 'syntax-error
  (eval
    '(define-case-class internal-methods-conflict-test
       ((name string?)
        (test-name string?)
        (age integer?))
       (define (test-name str)
         (string-append str " ")))
  ) ;eval
) ;check-catch

;; 测试自动生成的 %equals 方法
(let ()
  (define-case-class point
    ((x integer?)
     (y integer?)
    ) ;
  ) ;define-case-class

  (define p1 (point :x 1 :y 2))
  (define p2 (point :x 1 :y 2))
  (define p3 (point :x 3 :y 4))

  ;; 测试相同值的实例相等
  (check-true (p1 :equals p2))
  (check-true (p2 :equals p1))

  ;; 测试不同值的实例不相等
  (check-false (p1 :equals p3))
  (check-false (p3 :equals p1))

  ;; 测试实例与自身相等
  (check-true (p1 :equals p1))
  (check-true (p2 :equals p2))
  (check-true (p3 :equals p3))
) ;let

;; 测试 %equals 方法的类型检查
(let ()
  (define-case-class person
    ((name string?)
     (age integer?)
    ) ;
  ) ;define-case-class

  (define bob (person "Bob" 21))

  ;; 测试与非样本类对象比较抛出 type-error
  (check-catch 'type-error (bob :equals "not-a-sample-class"))
  (check-catch 'type-error (bob :equals 123))
  (check-catch 'type-error (bob :equals +))
) ;let

;; 测试不同类型样本类实例的比较
(let ()
  (define-case-class person
    ((name string?)
     (age integer?)
    ) ;
  ) ;define-case-class

  (define-case-class point
    ((x integer?)
     (y integer?)
    ) ;
  ) ;define-case-class

  (define bob (person "Bob" 21))
  (define p1 (point :x 1 :y 2))

  ;; 测试不同类型样本类实例不相等
  (check-false (bob :equals p1))
  (check-false (p1 :equals bob))
) ;let

;; 测试 %equals 方法在复杂样本类中的行为
(let ()
  (define-case-class complex-class
    ((name string?)
     (numbers list?)
     (flag boolean? #f)
    ) ;
  ) ;define-case-class

  (define c1 (complex-class :name "test" :numbers '(1 2 3) :flag #t))
  (define c2 (complex-class :name "test" :numbers '(1 2 3) :flag #t))
  (define c3 (complex-class :name "test" :numbers '(4 5 6) :flag #t))

  ;; 测试复杂字段的相等性比较
  (check-true (c1 :equals c2))
  (check-false (c1 :equals c3))
) ;let

;; 测试 %equals 方法在带有默认值的样本类中的行为
(let ()
  (define-case-class person-with-default
    ((name string? "Unknown")
     (age integer? 0)
    ) ;
  ) ;define-case-class

  (define p1 (person-with-default))
  (define p2 (person-with-default :name "Unknown" :age 0))
  (define p3 (person-with-default :name "Alice" :age 25))

  ;; 测试默认值实例的相等性
  (check-true (p1 :equals p2))
  (check-false (p1 :equals p3))
) ;let

;; 测试 %equals 方法在带有私有字段的样本类中的行为
(let ()
  (define-case-class person-with-private
    ((name string?)
     (age integer?)
    ) ;

    (define secret "private")
  ) ;define-case-class

  (define p1 (person-with-private "Bob" 21))
  (define p2 (person-with-private "Bob" 21))

  ;; 测试私有字段不影响相等性比较
  (check-true (p1 :equals p2))
) ;let


#|
define-object
定义一个具有静态方法的对象。

语法
-----
(define-object object-name definition ...)

参数
-----
object-name : symbol
要定义的对象名称，必须是一个符号。

definition : any
对象的定义内容，可以是变量定义或函数定义。

返回值
-----
返回 #t，表示对象定义成功。

描述
-----
define-object 是 (liii oop) 模块中用于创建对象的宏，它创建一个具有静态方法的对象。
对象通过消息传递机制调用方法，使用 `:method-name` 语法。

该宏会自动识别以 `@` 开头的函数定义作为静态方法，并将这些方法映射到对应的消息关键字。
例如，定义 `(@concat x y)` 会创建一个可以通过 `object-name :concat arg1 arg2` 调用的方法。

对象可以包含普通变量定义和静态方法定义，所有定义都在对象的私有环境中执行。

特点
-----
- 支持静态方法，通过 `@` 前缀定义
- 使用消息传递机制调用方法
- 支持对象间的相互引用
- 方法调用使用关键字语法（`:method-name`）
- 对象可以包含任意数量的变量和方法定义

注意事项
-----
- 对象名称必须是符号
- 静态方法必须以 `@` 开头
- 调用不存在的静态方法会抛出 value-error
- 对象可以包含普通变量定义，这些变量在对象内部可见
- 对象可以返回其他对象，支持对象组合
|#

(define-object string-utils
  (define (@concat x y)
    (string-append x y)
  ) ;define
) ;define-object

(check (string-utils :concat "a" "b") => "ab")

(define-object object1
  (define x 0)
  (define (@concat x y) 
    (string-append x y)
  ) ;define
) ;define-object

(define-object object2
  (define y 0)
  (define (@return-object1) object1)
) ;define-object

(check ((object2 :return-object1) :concat "a" "b") => "ab")

;; 测试调用不存在的方法
(check-catch 'value-error
  (string-utils :nonexistent-method)
) ;check-catch

(check-catch 'value-error
  (object1 :unknown-method "arg1" "arg2")
) ;check-catch

;; 测试空参数调用
(check-catch 'value-error
  (string-utils)
) ;check-catch

#|
define-class
定义一个具有私有字段和自动生成 getter/setter 的类。

语法
-----
(define-class class-name ((field-name type-predicate [default-value]) ...) method-definition ...)

参数
-----
class-name : symbol
要定义的类名称，必须是一个符号。

field-name : symbol
私有字段的名称。

type-predicate : procedure
字段的类型断言函数，用于验证字段值的类型。

default-value : any (可选)
字段的默认值，如果未提供则使用空列表。

method-definition : any
类的方法定义，可以是静态方法、实例方法或内部方法。

返回值
-----
返回 #t，表示类定义成功。

描述
-----
define-class 是 (liii oop) 模块中用于创建类的宏，它基于 define-case-class 构建，
提供了自动生成私有字段的 getter 和 setter 方法的功能。

该宏会自动为每个私有字段生成：
- 字段定义：使用默认值初始化字段
- Getter 方法：格式为 `:get-fieldname`，返回字段值
- Setter 方法：格式为 `:set-fieldname!`，设置字段值（带类型检查）

此外，宏还会自动生成以下方法：
- `:equals` - 相等性比较方法
  - 比较两个样本类实例是否相等
  - 检查两个实例是否为同一类型
  - 使用 `equal?` 比较所有私有字段的值
  - 如果比较的对象不是样本类实例，会抛出 `type-error`

生成的 getter 和 setter 方法通过消息传递机制调用，例如：
- `(instance :get-name)` 获取 name 字段的值
- `(instance :set-name! "Alice")` 设置 name 字段的值
- `(instance :equals other)` 比较两个实例是否相等

特点
-----
- 自动为私有字段生成 getter 和 setter 方法
- 自动生成 `:equals` 相等性比较方法
- 支持类型检查和默认值
- 基于 define-case-class 构建，继承其所有特性
- 支持静态方法（@前缀）、实例方法（%前缀）和内部方法
- 类型验证在运行时进行
- 自动生成 `:is-type-of` 静态方法用于类型检查

关于 `:equals` 方法的说明
-----
自动生成的 `:equals` 方法使用 `equal?` 来比较所有私有字段的值。
如果私有字段包含其他样本类实例，`equal?` 会比较它们的引用而不是内容。
如果需要深度比较嵌套的样本类实例，可以自定义 `%equals` 方法。

注意事项
-----
- 类名称必须是符号
- 字段类型断言函数必须是一个过程
- setter 方法会进行类型检查，类型不匹配会抛出 type-error
- 默认值在类定义时计算，如果涉及变量引用会捕获当前环境
- 支持任意数量的私有字段和方法定义
|#
(let ()
  (define-class person
    ((name string? "")
     (age integer? 0)
    ) ;
    
    (define (@apply name)
      (let ((r (person)))
        (r :set-name! name)
        (r :set-age! 10)
        r
      ) ;let
    ) ;define
  ) ;define-class
  
  ;; 测试@apply
  (define p1 (person))
  (define p2 (person "Bob"))
  
  ;; 测试setter和getter
  (p1 :set-name! "Alice")
  (p1 :set-age! 25)
  (check (p1 :get-name) => "Alice")
  (check (p1 :get-age) => 25)
  (check (p2 :get-name) => "Bob")
  (check (p2 :get-age) => 10)
  
  (check-true (person :is-type-of p1))
  (check-true (person :is-type-of p2))

  ;; 测试类型检查
  (check-catch 'type-error (p1 :set-name! 123))
  (check-catch 'type-error (p1 :set-age! "invalid"))

  ;; 测试 %equals 方法
  (check-true (p1 :equals p1))
  (check-true (p2 :equals p2))
  (check-false (p1 :equals p2))
) ;let

;; 测试 define-class 的 %equals 方法
(let ()
  (define-class point
    ((x integer? 0)
     (y integer? 0)
    ) ;
  ) ;define-class

  (define p1 (point))
  (define p2 (point))
  (define p3 (point))

  (p1 :set-x! 1)
  (p1 :set-y! 2)
  (p2 :set-x! 1)
  (p2 :set-y! 2)
  (p3 :set-x! 3)
  (p3 :set-y! 4)

  ;; 测试相同值的实例相等
  (check-true (p1 :equals p2))
  (check-true (p2 :equals p1))

  ;; 测试不同值的实例不相等
  (check-false (p1 :equals p3))
  (check-false (p3 :equals p1))

  ;; 测试实例与自身相等
  (check-true (p1 :equals p1))
  (check-true (p2 :equals p2))
  (check-true (p3 :equals p3))
) ;let

;; 测试 define-class %equals 方法的类型检查
(let ()
  (define-class person
    ((name string? "")
     (age integer? 0)
    ) ;
  ) ;define-class

  (define bob (person))
  (bob :set-name! "Bob")
  (bob :set-age! 21)

  ;; 测试与非样本类对象比较抛出 type-error
  (check-catch 'type-error (bob :equals "not-a-sample-class"))
  (check-catch 'type-error (bob :equals 123))
  (check-catch 'type-error (bob :equals +))
) ;let

;; 测试不同类型 define-class 实例的比较
(let ()
  (define-class person
    ((name string? "")
     (age integer? 0)
    ) ;
  ) ;define-class

  (define-class point
    ((x integer? 0)
     (y integer? 0)
    ) ;
  ) ;define-class

  (define bob (person))
  (bob :set-name! "Bob")
  (bob :set-age! 21)

  (define p1 (point))
  (p1 :set-x! 1)
  (p1 :set-y! 2)

  ;; 测试不同类型样本类实例不相等
  (check-false (bob :equals p1))
  (check-false (p1 :equals bob))
) ;let

;; 测试 define-class %equals 方法在复杂类中的行为
(let ()
  (define-class complex-class
    ((name string? "")
     (numbers list? '())
     (flag boolean? #f)
    ) ;
  ) ;define-class

  (define c1 (complex-class))
  (c1 :set-name! "test")
  (c1 :set-numbers! '(1 2 3))
  (c1 :set-flag! #t)

  (define c2 (complex-class))
  (c2 :set-name! "test")
  (c2 :set-numbers! '(1 2 3))
  (c2 :set-flag! #t)

  (define c3 (complex-class))
  (c3 :set-name! "test")
  (c3 :set-numbers! '(4 5 6))
  (c3 :set-flag! #t)

  ;; 测试复杂字段的相等性比较
  (check-true (c1 :equals c2))
  (check-false (c1 :equals c3))
) ;let

;; 测试 define-class %equals 方法在带有默认值的类中的行为
(let ()
  (define-class person-with-default
    ((name string? "Unknown")
     (age integer? 0)
    ) ;
  ) ;define-class

  (define p1 (person-with-default))
  (define p2 (person-with-default))
  (define p3 (person-with-default))

  (p3 :set-name! "Alice")
  (p3 :set-age! 25)

  ;; 测试默认值实例的相等性
  (check-true (p1 :equals p2))
  (check-false (p1 :equals p3))
) ;let

;; 测试 define-class %equals 方法在嵌套类中的行为（仅使用 equal? 比较）
(let ()
  (define-class inner-class
    ((value integer? 0))
  ) ;define-class

  (define-class outer-class
    ((inner inner-class? (inner-class))
     (name string? "")
    ) ;
  ) ;define-class

  (define o1 (outer-class))
  (define o2 (outer-class))
  (define o3 (outer-class))

  (o1 :set-name! "test")
  (o2 :set-name! "test")
  (o3 :set-name! "different")

  ;; 测试嵌套类实例的相等性（仅使用 equal? 比较，不会递归调用 %equals）
  ;; 注意：由于 inner 字段是样本类实例，equal? 会比较它们的引用而不是内容
  ;; 因此 o1 和 o2 的 inner 字段引用不同，导致 :equals 返回 #f
  (check-false (o1 :equals o2))
  (check-false (o1 :equals o3))
) ;let

;; 测试 define-class %equals 方法在自定义方法冲突时的行为
(let ()
  (define-class person-with-custom-equals
    ((name string? "")
     (age integer? 0)
    ) ;

    ;; 自定义的 %equals 方法会覆盖自动生成的方法
    (define (%equals that)
      (and (that :is-instance-of 'person-with-custom-equals)
           (equal? name (that :get-name))
      ) ;and
    ) ;define
  ) ;define-class

  (define p1 (person-with-custom-equals))
  (p1 :set-name! "Bob")
  (p1 :set-age! 21)

  (define p2 (person-with-custom-equals))
  (p2 :set-name! "Bob")
  (p2 :set-age! 30)

  (define p3 (person-with-custom-equals))
  (p3 :set-name! "Alice")
  (p3 :set-age! 21)

  ;; 测试自定义 %equals 方法的行为（只比较 name，不比较 age）
  (check-true (p1 :equals p2))
  (check-false (p1 :equals p3))
) ;let

#|
case-class?
判断一个对象是否为样本类（case class）实例。

语法
-----
(case-class? obj)

参数
-----
obj : any
待检查的对象，可以是任何 Goldfish Scheme 值。

返回值
-----
boolean
如果对象是样本类实例则返回 #t，否则返回 #f。

描述
-----
case-class? 是 (liii oop) 模块中用于类型检查的函数，它判断给定的对象是否是通过
`define-case-class` 或 `define-class` 宏创建的样本类实例。

该函数通过分析对象的源代码结构来识别样本类，具体检查：
- 对象是否为过程
- 过程源代码是否具有特定的结构
- 过程体中是否包含样本类特有的消息分发模式
- 是否包含 `:is-instance-of` 和 `:equals` 方法

特点
-----
- 运行时类型检查：在运行时动态判断对象类型
- 结构识别：通过源代码结构识别样本类
- 通用性：适用于所有通过 define-case-class 和 define-class 创建的对象
- 精确性：能够准确区分样本类实例和普通过程

注意事项
-----
- 只能识别通过 define-case-class 和 define-class 创建的样本类
- 对于其他类型的对象（包括普通过程、数字、字符串等）返回 #f
- 依赖于过程源代码的结构，不适用于编译后优化的代码
- 是底层类型检查函数，通常使用 `:is-type-of` 方法进行类型检查更直观
|#
(check-false (case-class? (lambda (x) x)))
(check-false (case-class? +))
(check-false (case-class? identity))

(let ((bob (person "Bob" 21)))
  (check-true (case-class? bob))
  (check-false (case-class? +))
  (check-false (case-class? 42))
) ;let


#|
define-final-class
定义一个性能优化的 final class，解决 define-case-class 的闭包初始化时间过长问题。

语法
----
(define-final-class class-name fields . private-fields-and-methods)

参数
----
class-name : symbol
要定义的 final class 名称。

fields : list
字段定义列表，每个字段格式为 (field-name type-predicate [default-value])。

private-fields-and-methods : any
可选的私有字段和方法定义。

返回值
----
procedure
返回一个函数，该函数可以用于创建 final class 实例或调用静态方法。

描述
----
`define-final-class` 是 (liii oop) 模块中用于定义高性能样本类的宏。
它通过将实例方法预编译到独立的对象中，解决了 `define-case-class` 在实例创建时
为每个方法创建闭包导致的初始化时间过长问题。

与 `define-case-class` 的主要区别：
- **性能优化**：实例方法在类定义时预编译，避免每次实例创建时的闭包创建
- **方法调用**：方法调用通过预编译的对象进行，减少运行时开销
- **内存使用**：相同方法在多个实例间共享，减少内存占用

字段定义中每个字段由三部分组成：
- field-name: 字段名称（符号）
- type-predicate: 类型断言函数，用于验证字段值的类型
- default-value: 可选，字段的默认值

方法类型包括：
- 静态方法: 以 `@` 开头的函数定义，通过类名调用
- 实例方法: 以 `%` 开头的函数定义，通过实例调用
- 内部方法: 普通函数定义，仅在类内部可见

特点
----
- **高性能**: 实例方法预编译，避免运行时闭包创建
- **类型安全**: 创建实例时会自动验证字段类型
- **不可变性**: 字段默认不可变，通过关键字参数创建新实例
- **模式匹配**: 支持通过字段名访问字段值
- **方法分发**: 支持静态方法和实例方法
- **相等性比较**: 自动实现 `:equals` 方法
- **字符串表示**: 自动实现 `:to-string` 方法
- **类型检查**: 自动生成 `:is-type-of` 静态方法

性能优势
----
`define-final-class` 相比 `define-case-class` 在以下方面具有显著优势：

1. **实例创建时间**: 当实例方法数量增加时，`define-final-class` 的实例创建时间
   基本保持不变，而 `define-case-class` 的创建时间会线性增长。

2. **方法调用性能**: 方法调用通过预编译的对象进行，避免了 `define-case-class`
   中的消息分发开销。

3. **内存使用**: 相同的方法实现在多个实例间共享，减少了内存占用。

自动生成的方法
----
`define-final-class` 会自动为每个样本类生成以下方法：

**实例方法**
- `:equals` - 相等性比较方法
  - 比较两个样本类实例是否相等
  - 检查两个实例是否为同一类型
  - 使用 `equal?` 比较所有字段的值
  - 如果比较的对象不是样本类实例，会抛出 `type-error`

- `:to-string` - 字符串表示方法
  - 返回样本类实例的字符串表示
  - 格式为 `(class-name :field1 value1 :field2 value2 ...)`

- `:is-instance-of` - 实例类型检查方法
  - 检查实例是否属于指定的类
  - 与 `:is-type-of` 静态方法配合使用

**静态方法**
- `:is-type-of` - 类型检查方法
  - 检查对象是否为该样本类的实例
  - 通过调用对象的 `:is-instance-of` 方法实现
  - 返回布尔值，表示对象是否属于该类

- `:apply` - 实例创建方法
  - 通过位置参数创建样本类实例
  - 参数顺序与字段定义顺序一致
  - 提供位置参数调用的便捷方式

注意事项
----
- 方法名不能与字段名冲突
- 字段类型验证在运行时进行
- 实例方法通过 `%` 前缀定义
- 静态方法通过 `@` 前缀定义
- 私有字段仅在类内部可见
- 相比 `define-case-class`，`define-final-class` 在类定义时会有额外的编译开销，
  但实例创建和方法调用性能更好

适用场景
----
- 需要创建大量实例的类
- 实例方法数量较多的类
- 对性能要求较高的场景
- 需要频繁调用实例方法的场景

功能限制
----
`define-final-class` 相比 `define-case-class` 有以下功能限制：

1. **不支持内部状态缓存**：无法在实例中维护内部状态变量
   - 例如：`rich-string` 需要缓存字符串长度以提高性能，`define-final-class` 无法实现
   - 无法维护实例级别的私有状态

2. **不支持状态可变性**：无法实现可变状态的方法
   - 无法实现类似 `%set-name!` 这样的可变方法
   - 所有状态都通过字段传递，无法在方法间共享状态

3. **方法间无法共享状态**：实例方法无法访问其他方法维护的状态
   - 每个方法都是独立的，无法维护跨方法的状态

性能对比
----
根据性能测试，当实例方法数量达到 32 个时，`define-final-class` 的实例创建性能
比 `define-case-class` 提升约 20%。方法调用性能也有相应提升。

选择建议
----
- **使用 `define-final-class`**：当需要高性能且不需要内部状态缓存时
- **使用 `define-case-class`**：当需要内部状态缓存、可变状态或复杂状态管理时
- **使用 `define-class`**：当需要自动生成 getter/setter 和私有字段时
|#


;; 测试转换函数
(let* ((object-name 'person-object)
       (field-names '(name age))
       (methods '((define (%to-string)
                    (string-append "I am " name ", " (number->string age) " years old!"))
                  (define (%greet other-name)
                    (string-append "Hi " other-name ", " (%to-string)))
                  ) ;define
       ) ;methods
       (transformed (transform-instance-methods methods object-name field-names)))

  ;; 检查转换后的方法定义
  (check (length transformed) => 2)

  ;; 检查第一个方法 (%to-string)
  (let ((to-string-method (car transformed)))
    (check (car to-string-method) => 'define)
    (check (cadr to-string-method) => '(%to-string))
    (check (caddr to-string-method) => '(string-append "I am " name ", " (number->string age) " years old!"))
  ) ;let

  ;; 检查第二个方法 (%greet) - 应该将 (%to-string) 转换为 ((person-object :to-string name age))
  (let ((greet-method (cadr transformed)))
    (check (car greet-method) => 'define)
    (check (cadr greet-method) => '(%greet other-name))
    (check (caddr greet-method) => '(string-append "Hi " other-name ", " ((person-object :to-string name age))))
  ) ;let
) ;let*

(define-final-class person
  ((name string? "Bob")
   (age integer?)
  ) ;
) ;define-final-class

(let ((bob (person :name "Bob" :age 21)))
  (check (bob 'name) => "Bob")
  (check (bob 'age) => 21)
  (check ((bob :name "hello") 'name) => "hello")
  (check-catch 'value-error (bob 'sex))
  (check-catch 'value-error (bob :sex))
  (check-true (bob :is-instance-of 'person))
  (check-true (person :is-type-of bob))
  (check (bob :to-string) => "(person :name \"Bob\" :age 21)")
) ;let

(check-catch 'type-error (person 1 21))

(let ((bob (person "Bob" 21))
      (get-name (lambda (x)
                 (case* x
                   ((#<procedure?>) (x 'name))
                   (else (value-error))))
                 ) ;case*
      ) ;get-name
  (check (get-name bob) => "Bob")
  (check-catch 'value-error (get-name 1))
) ;let

(define-final-class jerson
  ((name string?)
   (age integer?)
  ) ;
  
  (define (%to-string)
    (string-append "I am " name " " (number->string age) " years old!")
  ) ;define
  (define (%greet x)
    (string-append "Hi " x ", " (%to-string))
  ) ;define
  (define (%i-greet x)
    (string-append name ": " (%greet x)) 
  ) ;define
) ;define-final-class

(check-true (procedure? (jerson-object :to-string "name" 21)))

(let ((bob (jerson "Bob" 21)))
  (check (bob :to-string) => "I am Bob 21 years old!")
  (check (bob :greet "Alice") => "Hi Alice, I am Bob 21 years old!")
  (check (bob :i-greet "Alice") => "Bob: Hi Alice, I am Bob 21 years old!")
) ;let



(define-final-class test-case-class
  ((name string?))
  
  (define (@this-is-a-static-method)
    (test-case-class "static")
  ) ;define
  
  (define (%this-is-a-instance-method)
    (test-case-class (string-append name "instance"))
  ) ;define
 ;define
) ;define-final-class

(let ((hello (test-case-class "hello ")))
  (check-catch 'value-error (hello :this-is-a-static-method))
  (check (test-case-class :this-is-a-static-method) => (test-case-class "static"))
) ;let

(check-catch 'syntax-error
  (eval
    '(define-final-class instance-methods-conflict-test
      ((name string?)
       (age integer?))
      (define (%name)
        name))
  ) ;eval
) ;check-catch

(check-catch 'syntax-error
  (eval
    '(define-final-class static-methods-conflict-test
      ((name string?)
       (age integer?))
      (define (@name)
        name))
  ) ;eval
) ;check-catch

(check-catch 'syntax-error
  (eval
    '(define-final-class internal-methods-conflict-test
       ((name string?)
        (test-name string?)
        (age integer?))
       (define (test-name str)
         (string-append str " ")))
  ) ;eval
) ;check-catch

;; 测试自动生成的 %equals 方法
(let ()
  (define-final-class point
    ((x integer?)
     (y integer?)
    ) ;
  ) ;define-final-class

  (define p1 (point :x 1 :y 2))
  (define p2 (point :x 1 :y 2))
  (define p3 (point :x 3 :y 4))

  ;; 测试相同值的实例相等
  (check-true (p1 :equals p2))
  (check-true (p2 :equals p1))

  ;; 测试不同值的实例不相等
  (check-false (p1 :equals p3))
  (check-false (p3 :equals p1))

  ;; 测试实例与自身相等
  (check-true (p1 :equals p1))
  (check-true (p2 :equals p2))
  (check-true (p3 :equals p3))
) ;let

;; 测试 %equals 方法的类型检查
(let ()
  (define-final-class person
    ((name string?)
     (age integer?)
    ) ;
  ) ;define-final-class

  (define bob (person "Bob" 21))

  ;; 测试与非样本类对象比较抛出 type-error
  (check-catch 'type-error (bob :equals "not-a-sample-class"))
  (check-catch 'type-error (bob :equals 123))
  (check-catch 'type-error (bob :equals +))
) ;let

;; 测试不同类型样本类实例的比较
(let ()
  (define-final-class person
    ((name string?)
     (age integer?)
    ) ;
  ) ;define-final-class

  (define-final-class point
    ((x integer?)
     (y integer?)
    ) ;
  ) ;define-final-class

  (define bob (person "Bob" 21))
  (define p1 (point :x 1 :y 2))

  ;; 测试不同类型样本类实例不相等
  (check-false (bob :equals p1))
  (check-false (p1 :equals bob))
) ;let

;; 测试 %equals 方法在复杂样本类中的行为
(let ()
  (define-final-class complex-class
    ((name string?)
     (numbers list?)
     (flag boolean? #f)
    ) ;
  ) ;define-final-class

  (define c1 (complex-class :name "test" :numbers '(1 2 3) :flag #t))
  (define c2 (complex-class :name "test" :numbers '(1 2 3) :flag #t))
  (define c3 (complex-class :name "test" :numbers '(4 5 6) :flag #t))

  ;; 测试复杂字段的相等性比较
  (check-true (c1 :equals c2))
  (check-false (c1 :equals c3))
) ;let

;; 测试 %equals 方法在带有默认值的样本类中的行为
(let ()
  (define-final-class person-with-default
    ((name string? "Unknown")
     (age integer? 0)
    ) ;
  ) ;define-final-class

  (define p1 (person-with-default))
  (define p2 (person-with-default :name "Unknown" :age 0))
  (define p3 (person-with-default :name "Alice" :age 25))

  ;; 测试默认值实例的相等性
  (check-true (p1 :equals p2))
  (check-false (p1 :equals p3))
) ;let

;; 测试 %equals 方法在带有私有字段的样本类中的行为
(let ()
  (define-final-class person-with-private
    ((name string?)
     (age integer?)
    ) ;

    (define secret "private")
  ) ;define-final-class

  (define p1 (person-with-private "Bob" 21))
  (define p2 (person-with-private "Bob" 21))

  ;; 测试私有字段不影响相等性比较
  (check-true (p1 :equals p2))
) ;let

(check-report)


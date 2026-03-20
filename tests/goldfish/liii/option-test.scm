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
        (liii option)
        (liii lang)
) ;import

(check-set-mode! 'report-failed)

#|
option
创建包含值的option对象。

语法
----
(option value)

参数
----
value : any
用于包装到option中的值。

返回值
-----
以option形式返回包装后的值对象。

说明
----
将任意值包装为option对象，用于函数式编程中处理可能缺失的值。

边界条件
--------
- 非空值：创建包含该值的option
- 空值：创建空的option

性能特征
--------
- 时间复杂度：O(1)，直接包装现有值
- 空间复杂度：O(1)，需要存储值引用

兼容性
------
- 支持所有option实例方法
- 与none函数配合使用
|#

;;; 测试构造函数
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '())))
  (check (opt1 :defined?) => #t)
  (check (opt2 :defined?) => #t)
  (check (opt3 :empty?) => #t)
) ;let

#|
none
创建空的option对象。

语法
----
(none)

参数
----
无参数。

返回值
-----
返回空的option对象。

说明
----
创建表示缺失值的空option对象。

边界条件
--------
- 总是返回空的option对象

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)

兼容性
------
- 支持所有option实例方法
|#

;;; 测试none函数
(let ((opt (none)))
  (check (opt :empty?) => #t)
  (check (opt :defined?) => #f)
) ;let

#|
option%get
获取option对象中的值。

语法
----
(option%get)

参数
----
无参数。

返回值
-----
返回option对象中包装的值。

说明
----
从option对象中提取包装的值。如果option为空，会抛出错误。

边界条件
--------
- 非空option：返回包装的值
- 空option：抛出错误

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
|#

;;; 测试option%get方法
(let ((opt1 (option 42))
      (opt2 (option "hello")))
  (check (opt1 :get) => 42)
  (check (opt2 :get) => "hello")
) ;let

#|
option%get-or-else
安全获取option对象中的值，如果option为空则返回默认值。

语法
----
(option%get-or-else default)

参数
----
default : any
当option为空时返回的默认值，可以是任意值或返回值的函数。

返回值
-----
如果option非空，返回包装的值；如果option为空，返回默认值。

说明
----
安全地从option对象中提取包装的值，避免空option的错误。

边界条件
--------
- 非空option：返回包装的值
- 空option：返回默认值
- 默认值为函数：调用函数并返回结果

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
|#

;;; 测试option%get-or-else方法
(let ((opt1 (option 42))
      (opt2 (none)))
  (check (opt1 :get-or-else 0) => 42)
  (check (opt2 :get-or-else 0) => 0)
  (check (opt2 :get-or-else (lambda () "default")) => "default")
) ;let

#|
option%or-else
链式操作option对象，如果当前option为空则返回备选option。

语法
----
(option%or-else default . args)

参数
----
default : option
当当前option为空时返回的备选option对象。
args : any
可选的额外参数，用于链式操作。

返回值
-----
如果当前option非空，返回当前option；如果当前option为空，返回备选option。

说明
----
提供链式操作option对象的能力，支持多个备选option的链式调用。

边界条件
--------
- 非空option：返回当前option
- 空option：返回备选option
- 参数类型检查：default必须是option类型

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
|#

;;; 测试option%or-else方法
(let ((opt1 (option 42))
      (opt2 (option 0))
      (opt3 (none)))
  (check ((opt1 :or-else opt2) :get) => 42)
  (check ((opt3 :or-else opt1) :get) => 42)
  (check ((opt3 :or-else opt2) :get) => 0)
) ;let


#|
option%defined?
检查option对象是否包含值（非空）。

语法
----
(option%defined?)

参数
----
无参数。

返回值
-----
布尔值：
- #t 表示option包含值（非空）
- #f 表示option为空

说明
----
用于检查option对象是否包含实际的值，而不是空的option。

边界条件
--------
- 非空option：返回#t
- 空option：返回#f
- 包含不同数据类型的值：都返回#t

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
|#

;;; 测试option%defined?方法
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '(1 2 3)))  ; 包含非空列表的option
      (opt4 (none)))
  (check (opt1 :defined?) => #t)
  (check (opt2 :defined?) => #t)
  (check (opt3 :defined?) => #t)  ; 包含列表的option返回#t
  (check (opt4 :defined?) => #f)
) ;let

#|
option%empty?
检查option对象是否为空（不包含值）。

语法
----
(option%empty?)

参数
----
无参数。

返回值
-----
布尔值：
- #t 表示option为空
- #f 表示option包含值

说明
----
用于检查option对象是否为空，即不包含任何值。这是option%defined?的互补方法。

边界条件
--------
- 空option：返回#t
- 非空option：返回#f
- 包含空列表的option：返回#t（空列表被视为空值）
- 包含非空列表的option：返回#f

性能特征
--------
- 时间复杂度：O(1)
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
|#

;;; 测试option%empty?方法
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '(1 2 3)))  ; 包含非空列表的option
      (opt4 (option '()))       ; 包含空列表的option（被视为空option）
      (opt5 (none)))
  (check (opt1 :empty?) => #f)
  (check (opt2 :empty?) => #f)
  (check (opt3 :empty?) => #f)  ; 包含非空列表的option返回#f
  (check (opt4 :empty?) => #t)  ; 包含空列表的option返回#t（被视为空option）
  (check (opt5 :empty?) => #t)
) ;let

#|
option%forall
对option中的值应用谓词函数，如果option为空则返回#f。

语法
----
(option%forall f)

参数
----
f : procedure
接受单个参数的谓词函数，返回布尔值。

返回值
-----
布尔值：
- #f 表示option为空，或者谓词函数对值返回#f
- #t 表示option非空且谓词函数对值返回#t

说明
----
对option对象中的值应用谓词函数进行验证。如果option为空，直接返回#f；
如果option非空，则返回谓词函数应用于包装值的结果。

边界条件
--------
- 空option：返回#f
- 非空option：返回 (f value) 的结果
- 谓词函数返回#t：返回#t
- 谓词函数返回#f：返回#f
- 包含不同类型的值：都能正确处理

性能特征
--------
- 时间复杂度：O(1)，如果option为空；O(f) 如果option非空，其中f是谓词函数的复杂度
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
- 与option%exists方法互补
|#

;;; 测试option%forall方法
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '(1 2 3)))  ; 包含列表的option
      (opt4 (none)))
  ;; 测试空option返回#f
  (check (opt4 :forall (lambda (x) #t)) => #f)
  (check (opt4 :forall (lambda (x) #f)) => #f)

  ;; 测试谓词函数返回#t的情况
  (check (opt1 :forall (lambda (x) (= x 42))) => #t)
  (check (opt2 :forall (lambda (x) (string? x))) => #t)
  (check (opt3 :forall (lambda (x) (list? x))) => #t)

  ;; 测试谓词函数返回#f的情况
  (check (opt1 :forall (lambda (x) (= x 0))) => #f)
  (check (opt2 :forall (lambda (x) (number? x))) => #f)
  (check (opt3 :forall (lambda (x) (null? x))) => #f)

  ;; 测试不同类型的值
  (check (opt1 :forall (lambda (x) (> x 40))) => #t)
  (check (opt2 :forall (lambda (x) (string=? x "hello"))) => #t)
) ;let

#|
option%exists
检查option中是否存在满足谓词函数条件的值，如果option为空则返回#f。

语法
----
(option%exists f)

参数
----
f : procedure
接受单个参数的谓词函数，返回布尔值。

返回值
-----
布尔值：
- #f 表示option为空，或者谓词函数对值返回#f
- #t 表示option非空且谓词函数对值返回#t

说明
----
检查option对象中的值是否满足谓词函数的条件。如果option为空，直接返回#f；
如果option非空，则返回谓词函数应用于包装值的结果。

虽然option%exists和option%forall在行为上相同（因为option最多只能包含一个值），
但在语义上有所不同：
- forall："所有"（但option只有一个值，所以就是检查这个值）
- exists："存在"（但option只有一个值，所以也是检查这个值）

边界条件
--------
- 空option：返回#f
- 非空option：返回 (f value) 的结果
- 谓词函数返回#t：返回#t
- 谓词函数返回#f：返回#f
- 包含不同类型的值：都能正确处理

性能特征
--------
- 时间复杂度：O(1)，如果option为空；O(f) 如果option非空，其中f是谓词函数的复杂度
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
- 与option%forall方法互补
|#

;;; 测试option%exists方法
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '(1 2 3)))  ; 包含列表的option
      (opt4 (option #\a))       ; 包含字符的option
      (opt5 (option #t))        ; 包含布尔值的option
      (opt6 (none)))
  ;; 测试空option返回#f
  (check (opt6 :exists (lambda (x) #t)) => #f)
  (check (opt6 :exists (lambda (x) #f)) => #f)

  ;; 测试谓词函数返回#t的情况
  (check (opt1 :exists (lambda (x) (= x 42))) => #t)
  (check (opt2 :exists (lambda (x) (string? x))) => #t)
  (check (opt3 :exists (lambda (x) (list? x))) => #t)
  (check (opt4 :exists (lambda (x) (char? x))) => #t)
  (check (opt5 :exists (lambda (x) (boolean? x))) => #t)

  ;; 测试谓词函数返回#f的情况
  (check (opt1 :exists (lambda (x) (= x 0))) => #f)
  (check (opt2 :exists (lambda (x) (number? x))) => #f)
  (check (opt3 :exists (lambda (x) (null? x))) => #f)
  (check (opt4 :exists (lambda (x) (number? x))) => #f)
  (check (opt5 :exists (lambda (x) (char? x))) => #f)

  ;; 测试不同类型的值
  (check (opt1 :exists (lambda (x) (> x 40))) => #t)
  (check (opt2 :exists (lambda (x) (string=? x "hello"))) => #t)
  (check (opt3 :exists (lambda (x) (= (length x) 3))) => #t)
  (check (opt4 :exists (lambda (x) (char=? x #\a))) => #t)
  (check (opt5 :exists (lambda (x) x)) => #t)
) ;let

#|
option%contains
检查option中是否包含指定的元素，如果option为空则返回#f。

语法
----
(option%contains elem)

参数
----
elem : any
要检查是否包含的元素，可以是任意值。

返回值
-----
布尔值：
- #f 表示option为空，或者option中的值与elem不相等
- #t 表示option非空且option中的值与elem相等

说明
----
检查option对象中是否包含指定的元素。如果option为空，直接返回#f；
如果option非空，则使用equal?函数比较option中的值与elem是否相等。

边界条件
--------
- 空option：返回#f
- 非空option：返回 (equal? value elem) 的结果
- 包含不同类型的值：都能正确处理
- 相等性比较：使用equal?函数进行比较

性能特征
--------
- 时间复杂度：O(1)，如果option为空；O(e) 如果option非空，其中e是equal?函数的复杂度
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
- 使用equal?函数进行相等性比较
|#

;;; 测试option%contains方法
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '(1 2 3)))  ; 包含列表的option
      (opt4 (option #\a))       ; 包含字符的option
      (opt5 (option #t))        ; 包含布尔值的option
      (opt6 (option 'symbol))   ; 包含符号的option
      (opt7 (none)))
  ;; 测试空option返回#f
  (check (opt7 :contains 42) => #f)
  (check (opt7 :contains "hello") => #f)
  (check (opt7 :contains '(1 2 3)) => #f)
  (check (opt7 :contains #\a) => #f)
  (check (opt7 :contains #t) => #f)
  (check (opt7 :contains 'symbol) => #f)

  ;; 测试包含指定元素返回#t的情况
  (check (opt1 :contains 42) => #t)
  (check (opt2 :contains "hello") => #t)
  (check (opt3 :contains '(1 2 3)) => #t)
  (check (opt4 :contains #\a) => #t)
  (check (opt5 :contains #t) => #t)
  (check (opt6 :contains 'symbol) => #t)

  ;; 测试不包含指定元素返回#f的情况
  (check (opt1 :contains 0) => #f)
  (check (opt1 :contains "42") => #f)
  (check (opt2 :contains "world") => #f)
  (check (opt2 :contains 42) => #f)
  (check (opt3 :contains '(1 2)) => #f)
  (check (opt3 :contains '(1 2 3 4)) => #f)
  (check (opt4 :contains #\b) => #f)
  (check (opt4 :contains "a") => #f)
  (check (opt5 :contains #f) => #f)
  (check (opt5 :contains 1) => #f)
  (check (opt6 :contains 'other-symbol) => #f)
  (check (opt6 :contains "symbol") => #f)

  ;; 测试相等性比较（使用equal?）
  (check (opt1 :contains 42) => #t)  ; 数字相等
  (check (opt2 :contains "hello") => #t)  ; 字符串相等
  (check (opt3 :contains '(1 2 3)) => #t)  ; 列表相等
  (check (opt4 :contains #\a) => #t)  ; 字符相等
  (check (opt5 :contains #t) => #t)  ; 布尔值相等
  (check (opt6 :contains 'symbol) => #t)  ; 符号相等
) ;let

#|
option%for-each
对option中的值应用函数（副作用操作），如果option为空则不执行任何操作。

语法
----
(option%for-each f)

参数
----
f : procedure
接受单个参数的函数，通常用于执行副作用操作。

返回值
-----
未定义。此方法主要用于副作用操作，不返回有意义的值。

说明
----
对option对象中的值应用函数进行副作用操作。如果option为空，不执行任何操作；
如果option非空，则调用函数 `(f value)` 对包装的值进行处理。

边界条件
--------
- 空option：不执行任何操作
- 非空option：调用函数 `(f value)`
- 函数参数：必须接受单个参数
- 返回值：通常忽略，主要用于副作用

性能特征
--------
- 时间复杂度：O(1)，如果option为空；O(f) 如果option非空，其中f是函数的复杂度
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
- 与option%map方法互补（map用于转换，for-each用于副作用）
|#

;;; 测试option%for-each方法
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '(1 2 3)))  ; 包含列表的option
      (opt4 (option #\a))       ; 包含字符的option
      (opt5 (option #t))        ; 包含布尔值的option
      (opt6 (none)))
  ;; 测试空option不执行函数
  (let ((executed #f))
    (opt6 :for-each (lambda (x) (set! executed #t)))
    (check executed => #f)
  ) ;let

  ;; 测试非空option执行函数并传递正确的值
  (let ((result '()))
    (opt1 :for-each (lambda (x) (set! result (cons x result))))
    (check result => '(42))
  ) ;let

  (let ((result '()))
    (opt2 :for-each (lambda (x) (set! result (cons x result))))
    (check result => '("hello"))
  ) ;let

  (let ((result '()))
    (opt3 :for-each (lambda (x) (set! result (cons x result))))
    (check result => '((1 2 3)))
  ) ;let

  (let ((result '()))
    (opt4 :for-each (lambda (x) (set! result (cons x result))))
    (check result => '(#\a))
  ) ;let

  (let ((result '()))
    (opt5 :for-each (lambda (x) (set! result (cons x result))))
    (check result => '(#t))
  ) ;let

  ;; 测试副作用操作（修改外部变量）
  (let ((counter 0))
    (opt1 :for-each (lambda (x) (set! counter (+ counter x))))
    (check counter => 42)
  ) ;let

  (let ((message ""))
    (opt2 :for-each (lambda (x) (set! message (string-append message x))))
    (check message => "hello")
  ) ;let

  ;; 测试多次调用同一个option
  (let ((sum 0))
    (opt1 :for-each (lambda (x) (set! sum (+ sum x))))
    (opt1 :for-each (lambda (x) (set! sum (+ sum x))))
    (check sum => 84)
  ) ;let

  ;; 测试不同类型的值都能正确处理
  (let ((results '()))
    (opt1 :for-each (lambda (x) (set! results (cons (number? x) results))))
    (opt2 :for-each (lambda (x) (set! results (cons (string? x) results))))
    (opt3 :for-each (lambda (x) (set! results (cons (list? x) results))))
    (opt4 :for-each (lambda (x) (set! results (cons (char? x) results))))
    (opt5 :for-each (lambda (x) (set! results (cons (boolean? x) results))))
    (check results => '(#t #t #t #t #t))
  ) ;let
) ;let

#|
option%map
对option中的值应用映射函数，返回包含转换结果的新option对象。

语法
----
(option%map f)

参数
----
f : procedure
接受单个参数的映射函数，返回转换后的值。

返回值
-----
新的option对象：
- 如果原option为空：返回空的option
- 如果原option非空：返回包含 `(f value)` 结果的新option

说明
----
对option对象中的值应用映射函数进行转换，返回包含转换结果的新option对象。
这是函数式编程中常见的转换操作，允许对option中的值进行链式处理。

边界条件
--------
- 空option：返回空的option
- 非空option：返回包含 `(f value)` 结果的新option
- 函数参数：必须接受单个参数
- 返回值：新的option对象，原option保持不变

性能特征
--------
- 时间复杂度：O(1)，如果option为空；O(f) 如果option非空，其中f是映射函数的复杂度
- 空间复杂度：O(1)，如果option为空；O(1) + 新值存储，如果option非空

兼容性
------
- 适用于所有option实例
- 与option%for-each方法互补（map用于转换并返回新option，for-each用于副作用）
|#

;;; 测试option%map方法
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '(1 2 3)))  ; 包含列表的option
      (opt4 (option #\a))       ; 包含字符的option
      (opt5 (option #t))        ; 包含布尔值的option
      (opt6 (option 'symbol))   ; 包含符号的option
      (opt7 (none)))
  ;; 测试空option返回空option
  (check ((opt7 :map (lambda (x) (+ x 1))) :empty?) => #t)
  (check ((opt7 :map (lambda (x) (string-append x "!"))) :empty?) => #t)
  (check ((opt7 :map (lambda (x) (cons 0 x))) :empty?) => #t)
  (check ((opt7 :map (lambda (x) (char-upcase x))) :empty?) => #t)
  (check ((opt7 :map (lambda (x) (not x))) :empty?) => #t)
  (check ((opt7 :map (lambda (x) (symbol->string x))) :empty?) => #t)

  ;; 测试数值转换
  (check ((opt1 :map (lambda (x) (+ x 1))) :get) => 43)
  (check ((opt1 :map (lambda (x) (* x 2))) :get) => 84)
  (check ((opt1 :map (lambda (x) (- x))) :get) => -42)
  (check ((opt1 :map (lambda (x) (number->string x))) :get) => "42")

  ;; 测试字符串操作
  (check ((opt2 :map (lambda (x) (string-append x "!"))) :get) => "hello!")
  (check ((opt2 :map (lambda (x) (string-upcase x))) :get) => "HELLO")
  (check ((opt2 :map (lambda (x) (string-length x))) :get) => 5)
  (check ((opt2 :map (lambda (x) (string->list x))) :get) => '(#\h #\e #\l #\l #\o))

  ;; 测试列表操作
  (check ((opt3 :map (lambda (x) (length x))) :get) => 3)
  (check ((opt3 :map (lambda (x) (cons 0 x))) :get) => '(0 1 2 3))
  (check ((opt3 :map (lambda (x) (map (lambda (y) (+ y 1)) x))) :get) => '(2 3 4))
  (check ((opt3 :map (lambda (x) (list->string (map (lambda (y) (integer->char (+ y 48))) x)))) :get) => "123")

  ;; 测试字符操作
  (check ((opt4 :map (lambda (x) (char-upcase x))) :get) => #\A)
  (check ((opt4 :map (lambda (x) (char->integer x))) :get) => 97)
  (check ((opt4 :map (lambda (x) (integer->char (+ (char->integer x) 1)))) :get) => #\b)
  (check ((opt4 :map (lambda (x) (string x))) :get) => "a")

  ;; 测试布尔值操作
  (check ((opt5 :map (lambda (x) (not x))) :get) => #f)
  (check ((opt5 :map (lambda (x) (if x "true" "false"))) :get) => "true")
  (check ((opt5 :map (lambda (x) (number->string (if x 1 0)))) :get) => "1")

  ;; 测试符号操作
  (check ((opt6 :map (lambda (x) (symbol->string x))) :get) => "symbol")
  (check ((opt6 :map (lambda (x) (string->symbol (string-append "new-" (symbol->string x))))) :get) => 'new-symbol)
  (check ((opt6 :map (lambda (x) (eq? x 'symbol))) :get) => #t)

  ;; 测试链式map操作
  (check ((opt1 :map (lambda (x) (+ x 1)) :map (lambda (x) (* x 2))) :get) => 86)
  (check ((opt2 :map (lambda (x) (string-append x "!")) :map string-length) :get) => 6)
  (check ((opt3 :map length :map (lambda (x) (+ x 1))) :get) => 4)

  ;; 测试原option保持不变
  (let ((original-value (opt1 :get)))
    (opt1 :map (lambda (x) (+ x 100)))
    (check (opt1 :get) => original-value)
  ) ;let

  (let ((original-value (opt2 :get)))
    (opt2 :map (lambda (x) (string-append x "-modified")))
    (check (opt2 :get) => original-value)
  ) ;let

  ;; 测试不同类型之间的转换
  (check ((opt1 :map (lambda (x) (string-append "number-" (number->string x)))) :get) => "number-42")
  (check ((opt2 :map (lambda (x) (string->symbol x))) :get) => 'hello)
  (check ((opt4 :map (lambda (x) (string x))) :map string-length :get) => 1)

  ;; 测试边界情况：映射函数返回不同类型
  (check ((opt1 :map (lambda (x) (if (> x 0) "positive" "negative"))) :get) => "positive")
  (check ((opt1 :map (lambda (x) (if (= x 42) #t #f))) :get) => #t)
  (check ((opt1 :map (lambda (x) (list x x x))) :get) => '(42 42 42))
) ;let


#|
option%flat-map
对option中的值应用扁平映射函数，返回扁平化后的option对象。

语法
----
(option%flat-map f)

参数
----
f : procedure
接受单个参数并返回option对象的函数。

返回值
-----
新的option对象：
- 如果原option为空：返回空的option
- 如果原option非空：返回 `(f value)` 的结果

说明
----
对option对象中的值应用扁平映射函数进行转换，返回扁平化后的option对象。
与option%map方法不同，flat-map的映射函数直接返回option对象，而不是普通值。
这使得flat-map可以处理嵌套的option结构，避免option的option这种情况。

边界条件
--------
- 空option：返回空的option
- 非空option：返回 `(f value)` 的结果
- 函数参数：必须接受单个参数并返回option对象
- 返回值：新的option对象，原option保持不变

性能特征
--------
- 时间复杂度：O(1)，如果option为空；O(f) 如果option非空，其中f是映射函数的复杂度
- 空间复杂度：O(1)，如果option为空；O(1) + 新option存储，如果option非空

兼容性
------
- 适用于所有option实例
- 与option%map方法互补（map用于普通映射，flat-map用于扁平映射）
|#

;;; 测试option%flat-map方法
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '(1 2 3)))  ; 包含列表的option
      (opt4 (option #\a))       ; 包含字符的option
      (opt5 (option #t))        ; 包含布尔值的option
      (opt6 (option 'symbol))   ; 包含符号的option
      (opt7 (none)))
  ;; 测试空option返回空option
  (check ((opt7 :flat-map (lambda (x) (option (+ x 1)))) :empty?) => #t)
  (check ((opt7 :flat-map (lambda (x) (option (string-append x "!")))) :empty?) => #t)
  (check ((opt7 :flat-map (lambda (x) (option (cons 0 x)))) :empty?) => #t)
  (check ((opt7 :flat-map (lambda (x) (option (char-upcase x)))) :empty?) => #t)
  (check ((opt7 :flat-map (lambda (x) (option (not x)))) :empty?) => #t)
  (check ((opt7 :flat-map (lambda (x) (option (symbol->string x)))) :empty?) => #t)

  ;; 测试数值转换（映射函数返回非空option）
  (check ((opt1 :flat-map (lambda (x) (option (+ x 1)))) :get) => 43)
  (check ((opt1 :flat-map (lambda (x) (option (* x 2)))) :get) => 84)
  (check ((opt1 :flat-map (lambda (x) (option (- x)))) :get) => -42)
  (check ((opt1 :flat-map (lambda (x) (option (number->string x)))) :get) => "42")

  ;; 测试字符串操作（映射函数返回非空option）
  (check ((opt2 :flat-map (lambda (x) (option (string-append x "!")))) :get) => "hello!")
  (check ((opt2 :flat-map (lambda (x) (option (string-upcase x)))) :get) => "HELLO")
  (check ((opt2 :flat-map (lambda (x) (option (string-length x)))) :get) => 5)
  (check ((opt2 :flat-map (lambda (x) (option (string->list x)))) :get) => '(#\h #\e #\l #\l #\o))

  ;; 测试列表操作（映射函数返回非空option）
  (check ((opt3 :flat-map (lambda (x) (option (length x)))) :get) => 3)
  (check ((opt3 :flat-map (lambda (x) (option (cons 0 x)))) :get) => '(0 1 2 3))
  (check ((opt3 :flat-map (lambda (x) (option (map (lambda (y) (+ y 1)) x)))) :get) => '(2 3 4))

  ;; 测试字符操作（映射函数返回非空option）
  (check ((opt4 :flat-map (lambda (x) (option (char-upcase x)))) :get) => #\A)
  (check ((opt4 :flat-map (lambda (x) (option (char->integer x)))) :get) => 97)
  (check ((opt4 :flat-map (lambda (x) (option (integer->char (+ (char->integer x) 1))))) :get) => #\b)
  (check ((opt4 :flat-map (lambda (x) (option (string x)))) :get) => "a")

  ;; 测试布尔值操作（映射函数返回非空option）
  (check ((opt5 :flat-map (lambda (x) (option (not x)))) :get) => #f)
  (check ((opt5 :flat-map (lambda (x) (option (if x "true" "false")))) :get) => "true")
  (check ((opt5 :flat-map (lambda (x) (option (number->string (if x 1 0))))) :get) => "1")

  ;; 测试符号操作（映射函数返回非空option）
  (check ((opt6 :flat-map (lambda (x) (option (symbol->string x)))) :get) => "symbol")
  (check ((opt6 :flat-map (lambda (x) (option (string->symbol (string-append "new-" (symbol->string x)))))) :get) => 'new-symbol)
  (check ((opt6 :flat-map (lambda (x) (option (eq? x 'symbol)))) :get) => #t)

  ;; 测试映射函数返回空option的情况
  (check ((opt1 :flat-map (lambda (x) (none))) :empty?) => #t)
  (check ((opt2 :flat-map (lambda (x) (none))) :empty?) => #t)
  (check ((opt3 :flat-map (lambda (x) (none))) :empty?) => #t)
  (check ((opt4 :flat-map (lambda (x) (none))) :empty?) => #t)
  (check ((opt5 :flat-map (lambda (x) (none))) :empty?) => #t)
  (check ((opt6 :flat-map (lambda (x) (none))) :empty?) => #t)

  ;; 测试链式flat-map操作
  (check ((opt1 :flat-map (lambda (x) (option (+ x 1))) :flat-map (lambda (x) (option (* x 2)))) :get) => 86)
  (check ((opt2 :flat-map (lambda (x) (option (string-append x "!"))) :flat-map (lambda (x) (option (string-length x)))) :get) => 6)
  (check ((opt3 :flat-map (lambda (x) (option (length x))) :flat-map (lambda (x) (option (+ x 1)))) :get) => 4)

  ;; 测试原option保持不变
  (let ((original-value (opt1 :get)))
    (opt1 :flat-map (lambda (x) (option (+ x 100))))
    (check (opt1 :get) => original-value)
  ) ;let

  (let ((original-value (opt2 :get)))
    (opt2 :flat-map (lambda (x) (option (string-append x "-modified"))))
    (check (opt2 :get) => original-value)
  ) ;let

  ;; 测试不同类型之间的转换
  (check ((opt1 :flat-map (lambda (x) (option (string-append "number-" (number->string x))))) :get) => "number-42")
  (check ((opt2 :flat-map (lambda (x) (option (string->symbol x)))) :get) => 'hello)
  (check ((opt4 :flat-map (lambda (x) (option (string x))) :flat-map (lambda (x) (option (string-length x)))) :get) => 1)

  ;; 测试边界情况：映射函数返回不同类型
  (check ((opt1 :flat-map (lambda (x) (option (if (> x 0) "positive" "negative")))) :get) => "positive")
  (check ((opt1 :flat-map (lambda (x) (option (if (= x 42) #t #f)))) :get) => #t)
  (check ((opt1 :flat-map (lambda (x) (option (list x x x)))) :get) => '(42 42 42))
) ;let

#|
option%equals
比较两个option对象是否相等。

语法
----
(option%equals other)

参数
----
other : option
要比较的另一个option对象。

返回值
-----
布尔值：
- #t 表示两个option对象相等
- #f 表示两个option对象不相等

说明
----
比较当前option对象与另一个option对象是否相等。两个option对象相等当且仅当：
- 两者都是空的option，或者
- 两者都包含值且使用equal?函数比较值相等

边界条件
--------
- 两个空option：返回#t
- 两个非空option且值相等：返回#t
- 一个空option一个非空option：返回#f
- 两个非空option但值不相等：返回#f
- 参数类型检查：other必须是option类型

性能特征
--------
- 时间复杂度：O(1)，如果option为空；O(e) 如果option非空，其中e是equal?函数的复杂度
- 空间复杂度：O(1)

兼容性
------
- 适用于所有option实例
- 使用equal?函数进行值比较
|#

;;; 测试option%equals方法
(let ((opt1 (option 42))
      (opt2 (option 42))
      (opt3 (option "hello"))
      (opt4 (option "hello"))
      (opt5 (option '(1 2 3)))
      (opt6 (option '(1 2 3)))
      (opt7 (none))
      (opt8 (none)))
  ;; 测试相同值的option相等
  (check (opt1 :equals opt2) => #t)
  (check (opt3 :equals opt4) => #t)
  (check (opt5 :equals opt6) => #t)

  ;; 测试不同值的option不相等
  (check (opt1 :equals opt3) => #f)
  (check (opt1 :equals opt5) => #f)
  (check (opt3 :equals opt5) => #f)

  ;; 测试空option相等
  (check (opt7 :equals opt8) => #t)

  ;; 测试空option与非空option不相等
  (check (opt1 :equals opt7) => #f)
  (check (opt7 :equals opt1) => #f)

  ;; 测试自反性
  (check (opt1 :equals opt1) => #t)
  (check (opt7 :equals opt7) => #t)
) ;let

#|
option%filter
对option中的值应用过滤函数，如果值满足条件则返回原option，否则返回空option。

语法
----
(option%filter pred)

参数
----
pred : procedure
接受单个参数的谓词函数，返回布尔值。

返回值
-----
新的option对象：
- 如果原option为空：返回空的option
- 如果原option非空且 `(pred value)` 返回#t：返回原option
- 如果原option非空且 `(pred value)` 返回#f：返回空的option

说明
----
对option对象中的值应用过滤函数进行筛选。如果option为空，直接返回空的option；
如果option非空，则根据谓词函数的返回值决定是否保留原option中的值。

边界条件
--------
- 空option：返回空的option
- 非空option且谓词函数返回#t：返回原option
- 非空option且谓词函数返回#f：返回空的option
- 包含不同类型的值：都能正确处理
- 谓词函数必须接受单个参数并返回布尔值

性能特征
--------
- 时间复杂度：O(1)，如果option为空；O(p) 如果option非空，其中p是谓词函数的复杂度
- 空间复杂度：O(1)，如果option为空；O(1) 如果option非空

兼容性
------
- 适用于所有option实例
- 与option%map和option%flat-map方法配合使用，支持链式操作
|#

;;; 测试option%filter方法
(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (option '(1 2 3)))  ; 包含列表的option
      (opt4 (option #\a))       ; 包含字符的option
      (opt5 (option #t))        ; 包含布尔值的option
      (opt6 (option 'symbol))   ; 包含符号的option
      (opt7 (none)))
  ;; 测试空option返回空option
  (check ((opt7 :filter (lambda (x) #t)) :empty?) => #t)
  (check ((opt7 :filter (lambda (x) #f)) :empty?) => #t)
  (check ((opt7 :filter (lambda (x) (> x 0))) :empty?) => #t)
  (check ((opt7 :filter (lambda (x) (string? x))) :empty?) => #t)
  (check ((opt7 :filter (lambda (x) (list? x))) :empty?) => #t)
  (check ((opt7 :filter (lambda (x) (char? x))) :empty?) => #t)
  (check ((opt7 :filter (lambda (x) (boolean? x))) :empty?) => #t)
  (check ((opt7 :filter (lambda (x) (symbol? x))) :empty?) => #t)

  ;; 测试谓词函数返回#t时返回原option
  (check ((opt1 :filter (lambda (x) (= x 42))) :get) => 42)
  (check ((opt2 :filter (lambda (x) (string? x))) :get) => "hello")
  (check ((opt3 :filter (lambda (x) (list? x))) :get) => '(1 2 3))
  (check ((opt4 :filter (lambda (x) (char? x))) :get) => #\a)
  (check ((opt5 :filter (lambda (x) (boolean? x))) :get) => #t)
  (check ((opt6 :filter (lambda (x) (symbol? x))) :get) => 'symbol)

  ;; 测试谓词函数返回#f时返回空option
  (check ((opt1 :filter (lambda (x) (= x 0))) :empty?) => #t)
  (check ((opt2 :filter (lambda (x) (number? x))) :empty?) => #t)
  (check ((opt3 :filter (lambda (x) (null? x))) :empty?) => #t)
  (check ((opt4 :filter (lambda (x) (number? x))) :empty?) => #t)
  (check ((opt5 :filter (lambda (x) (char? x))) :empty?) => #t)
  (check ((opt6 :filter (lambda (x) (string? x))) :empty?) => #t)

  ;; 测试数值比较条件
  (check ((opt1 :filter (lambda (x) (> x 40))) :get) => 42)
  (check ((opt1 :filter (lambda (x) (< x 50))) :get) => 42)
  (check ((opt1 :filter (lambda (x) (even? x))) :get) => 42)
  (check ((opt1 :filter (lambda (x) (> x 100))) :empty?) => #t)
  (check ((opt1 :filter (lambda (x) (< x 0))) :empty?) => #t)
  (check ((opt1 :filter (lambda (x) (odd? x))) :empty?) => #t)

  ;; 测试字符串匹配条件
  (check ((opt2 :filter (lambda (x) (string=? x "hello"))) :get) => "hello")
  (check ((opt2 :filter (lambda (x) (>= (string-length x) 3))) :get) => "hello")
  (check ((opt2 :filter (lambda (x) (string=? x "world"))) :empty?) => #t)
  (check ((opt2 :filter (lambda (x) (< (string-length x) 3))) :empty?) => #t)

  ;; 测试列表条件
  (check ((opt3 :filter (lambda (x) (= (length x) 3))) :get) => '(1 2 3))
  (check ((opt3 :filter (lambda (x) (member 2 x))) :get) => '(1 2 3))
  (check ((opt3 :filter (lambda (x) (not (null? x)))) :get) => '(1 2 3))
  (check ((opt3 :filter (lambda (x) (= (length x) 0))) :empty?) => #t)
  (check ((opt3 :filter (lambda (x) (member 4 x))) :empty?) => #t)

  ;; 测试字符条件
  (check ((opt4 :filter (lambda (x) (char=? x #\a))) :get) => #\a)
  (check ((opt4 :filter (lambda (x) (char<? x #\z))) :get) => #\a)
  (check ((opt4 :filter (lambda (x) (char>? x #\A))) :get) => #\a)
  (check ((opt4 :filter (lambda (x) (char=? x #\b))) :empty?) => #t)
  (check ((opt4 :filter (lambda (x) (char>? x #\z))) :empty?) => #t)

  ;; 测试布尔值条件
  (check ((opt5 :filter (lambda (x) x)) :get) => #t)
  (check ((opt5 :filter (lambda (x) (not x))) :empty?) => #t)

  ;; 测试符号条件
  (check ((opt6 :filter (lambda (x) (eq? x 'symbol))) :get) => 'symbol)
  (check ((opt6 :filter (lambda (x) (symbol? x))) :get) => 'symbol)
  (check ((opt6 :filter (lambda (x) (eq? x 'other-symbol))) :empty?) => #t)

  ;; 测试链式filter操作
  (check ((opt1 :filter (lambda (x) (> x 0)) :filter (lambda (x) (< x 100))) :get) => 42)
  (check ((opt2 :filter (lambda (x) (string? x)) :filter (lambda (x) (> (string-length x) 3))) :get) => "hello")
  (check ((opt3 :filter (lambda (x) (list? x)) :filter (lambda (x) (= (length x) 3))) :get) => '(1 2 3))

  ;; 测试filter与map的链式操作
  (check ((opt1 :filter (lambda (x) (> x 0)) :map (lambda (x) (+ x 1))) :get) => 43)
  (check ((opt2 :filter (lambda (x) (string? x)) :map string-length) :get) => 5)
  (check ((opt3 :filter (lambda (x) (list? x)) :map length) :get) => 3)

  ;; 测试原option保持不变
  (let ((original-value (opt1 :get)))
    (opt1 :filter (lambda (x) (> x 0)))
    (check (opt1 :get) => original-value)
  ) ;let

  (let ((original-value (opt2 :get)))
    (opt2 :filter (lambda (x) (string? x)))
    (check (opt2 :get) => original-value)
  ) ;let

  ;; 测试边界情况：复杂谓词函数
  (check ((opt1 :filter (lambda (x) (and (number? x) (> x 0) (< x 100)))) :get) => 42)
  (check ((opt2 :filter (lambda (x) (and (string? x) (>= (string-length x) 3)))) :get) => "hello")
  (check ((opt3 :filter (lambda (x) (and (list? x) (apply + x) (= (apply + x) 6)))) :get) => '(1 2 3))

  ;; 测试不同类型值的组合条件
  (check ((opt1 :filter (lambda (x) (or (= x 42) (= x 0)))) :get) => 42)
  (check ((opt2 :filter (lambda (x) (or (string=? x "hello") (string=? x "world")))) :get) => "hello")
  (check ((opt3 :filter (lambda (x) (or (null? x) (= (length x) 3)))) :get) => '(1 2 3))
) ;let


(check-report)
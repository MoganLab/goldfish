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
        (liii rich-either)
        (liii option)
        (liii lang)
        (liii error)
) ;import

(check-set-mode! 'report-failed)
(define either rich-either)
;; ==========================================
;; 1. 基础构造与类型判断测试
;; ==========================================

#|
left / right
构造函数：创建 Rich Either 实例。

语法
----
(left value)
(right value)

描述
----
- `left`: 创建一个表示失败或错误的 Either 实例。
- `right`: 创建一个表示成功或有效值的 Either 实例。
|#

#|
:left? / :right?
成员方法：判断 Either 的类型。

语法
----
(obj :left?)
(obj :right?)

返回值
------
boolean
|#

#|
:get
成员方法：提取内部值。

语法
----
(obj :get)

描述
----
无论对象是 Left 还是 Right，直接返回其内部存储的值。
|#
;; 测试 left 和 right 构造函数
(check-true ((left "error") :left?))
(check-false ((left "error") :right?))

(check-true ((right "success") :right?))
(check-false ((right "success") :left?))

;; 测试 get 方法
(check ((left "error") :get) => "error")
(check ((right 42) :get) => 42)

;; ==========================================
;; 2. or-else 和 get-or-else 测试
;; ==========================================

#|
:or-else
成员方法：对象级别的备选方案。

语法
----
(obj :or-else alternative)

参数
----
alternative : rich-either
    当 obj 为 Left 时返回的备用 Either 对象。

返回值
------
rich-either
    - 如果 obj 为 Right，返回 obj 自身。
    - 如果 obj 为 Left，返回 alternative。
|#

#|
:get-or-else
成员方法：值级别的安全提取。

语法
----
(obj :get-or-else default)

参数
----
default : any | procedure
    当 obj 为 Left 时返回的默认值，或者一个无参函数（Thunk）。

返回值
------
any
    - 如果 obj 为 Right，返回内部值。
    - 如果 obj 为 Left，且 default 是函数，返回 (default) 的结果；否则返回 default。
|#
;; 测试 %or-else
(let ((right-val (right 1))
      (left-val (left 0))
      (backup (right 2)))
  ;; Right 返回自身
  (check ((right-val :or-else backup) :get) => 1)
  ;; Left 返回备选方案
  (check ((left-val :or-else backup) :get) => 2)
) ;let

;; 测试 %get-or-else
(check ((right 42) :get-or-else 0) => 42)
(check ((left "error") :get-or-else 0) => 0)
;; 测试函数作为默认值
(check ((left "error") :get-or-else (lambda () 99)) => 99)

;; ==========================================
;; 3. filter-or-else 测试
;; ==========================================

#|
:filter-or-else
成员方法：条件过滤。

语法
----
(obj :filter-or-else predicate zero)

参数
----
predicate : procedure (any -> boolean)
    用于测试 Right 值的谓词函数。
zero : any
    当过滤失败（即谓词返回 false）时，用于构建新 Left 的值。

描述
----
- 如果 obj 是 Right 且 (predicate value) 为真：返回 obj 自身。
- 如果 obj 是 Right 且 (predicate value) 为假：返回 (left zero)。
- 如果 obj 是 Left：返回 obj 自身。
|#
;; Right 且满足条件时返回自身
(let ((r (right 10)))
  (check ((r :filter-or-else (lambda (x) (> x 5)) 0) :get) => 10)
) ;let

;; Right 但不满足条件时返回 left
(let ((r (right 3)))
  (check-true ((r :filter-or-else (lambda (x) (> x 5)) 0) :left?))
  (check ((r :filter-or-else (lambda (x) (> x 5)) 0) :get) => 0)
) ;let

;; Left 时返回自身
(let ((l (left "error")))
  (check ((l :filter-or-else (lambda (x) #t) 0) :get) => "error")
) ;let

;; ==========================================
;; 4. contains 测试
;; ==========================================

#|
:contains
成员方法：检查是否包含特定值。

语法
----
(obj :contains target)

描述
----
仅当 obj 是 Right 类型，且其内部值与 target 相等（使用 class=? 比较）时，返回 #t。
Left 类型总是返回 #f。
|#
(check-true ((right 42) :contains 42))
(check-false ((right 42) :contains 43))
(check-false ((left "error") :contains "error"))


;; ==========================================
;; 5. for-each 测试
;; ==========================================

#|
:for-each
成员方法：副作用遍历。

语法
----
(obj :for-each proc)

描述
----
如果 obj 是 Right，则对其值执行 proc。
如果 obj 是 Left，不执行任何操作。
|#
(let ((counter 0)
      (right-val (right 5))
      (left-val (left "error")))
  ;; Right 执行副作用
  (begin
    (right-val :for-each (lambda (x) (set! counter (+ counter x))))
    (check counter => 5)
  ) ;begin
  ;; Left 不执行副作用
  (begin
    (left-val :for-each (lambda (x) (set! counter (+ counter 10))))
    (check counter => 5)
  ) ;begin
) ;let

;; ==========================================
;; 6. to-option 测试
;; ==========================================

#|
:to-option
成员方法：类型转换。

语法
----
(obj :to-option)

返回值
------
option
    - Right 值转换为 (option value)。
    - Left 值转换为 (none)。
|#
;; Right 转换为 defined option
(let ((opt ((right 42) :to-option)))
  (check-true (opt :defined?))
  (check (opt :get) => 42)
) ;let

;; Left 转换为 empty option
(let ((opt ((left "error") :to-option)))
  (check-true (opt :empty?))
) ;let

;; ==========================================
;; 7. map 测试
;; ==========================================

#|
:map
成员方法：Functor 映射。

语法
----
(obj :map func . args)

描述
----
如果 obj 是 Right，应用 func 到其值上，并返回包装了新值的 Right。
如果 obj 是 Left，直接返回自身。
支持链式调用参数 args。
|#
;; 对 Right 应用 map
(let ((result ((right 5) :map (lambda (x) (* x 2)))))
  (check-true (result :right?))
  (check (result :get) => 10)
) ;let

;; 对 Left 应用 map 返回自身
(let ((l (left "error")))
  (check ((l :map (lambda (x) (string-append "Mapped: " x))) :get) => "error")
) ;let

;; ==========================================
;; 8. flat-map 测试
;; ==========================================

#|
:flat-map
成员方法：Monad 绑定。

语法
----
(obj :flat-map func . args)

描述
----
如果 obj 是 Right，应用 func（必须返回 Either）到其值上，并返回该结果。
如果 obj 是 Left，直接返回自身。
|#
;; 对 Right 应用 flat-map
(let ((result ((right 5) :flat-map (lambda (x) (right (* x 2))))))
  (check-true (result :right?))
  (check (result :get) => 10)
) ;let

;; 对 Left 应用 flat-map 返回自身
(let ((l (left "error")))
  (check ((l :flat-map (lambda (x) (right (string-length x)))) :get) => "error")
) ;let

;; ==========================================
;; 9. forall 和 exists 测试
;; ==========================================

#|
:forall
成员方法：全称量词检查。

语法
----
(obj :forall predicate)

描述
----
- 如果 obj 是 Right，返回 (predicate value)。
- 如果 obj 是 Left，返回 #t (真空真)。
|#

#|
:exists
成员方法：存在量词检查。

语法
----
(obj :exists predicate)

描述
----
- 如果 obj 是 Right，返回 (predicate value)。
- 如果 obj 是 Left，返回 #f。
|#
;; forall: Right 且满足条件时为真
(check-true ((right 10) :forall (lambda (x) (> x 5))))
(check-false ((right 3) :forall (lambda (x) (> x 5))))
;; forall: Left 总是为真
(check-true ((left "error") :forall (lambda (x) #f)))

;; exists: Right 且满足条件时为真
(check-true ((right 10) :exists (lambda (x) (> x 5))))
(check-false ((right 3) :exists (lambda (x) (> x 5))))
;; exists: Left 总是为假
(check-false ((left "error") :exists (lambda (x) #t)))

;; ==========================================
;; 10. 类型兼容性测试
;; ==========================================

;; 测试 either 是 rich-either 的别名
(check-true (either :is-type-of (left "test")))
(check-true (either :is-type-of (right "test")))

;; ==========================================
;; 11. 错误处理测试
;; ==========================================

#|
错误处理
检查非法参数是否能正确抛出 type-error。
|#

;; 测试 %or-else 参数类型检查
(check-catch 'type-error ((right 1) :or-else "not-an-either"))

;; 测试 %filter-or-else 参数类型检查
(check-catch 'type-error ((right 1) :filter-or-else "not-a-procedure" 0))

;; 测试 %forall 参数类型检查
(check-catch 'type-error ((right 1) :forall "not-a-procedure"))

;; 测试 %exists 参数类型检查
(check-catch 'type-error ((right 1) :exists "not-a-procedure"))

;; ==========================================
;; 12. 综合流程测试
;; ==========================================

;; 测试链式操作
(let* ((val1 (right 10))
       (val2 (val1 :map (lambda (x) (+ x 5)))) ;; Right 15
       (val3 (val2 :flat-map (lambda (x) (right (* x 2)))))) ;; Right 30
  (check-true (val3 :right?))
  (check (val3 :get) => 30)
) ;let*

;; 测试错误处理流程
(let* ((error-val (left "network error"))
       ;; 下面的 map 不应执行，因为输入已经是 Left
       (mapped-error (error-val :map (lambda (x) (string-append "Error: " x)))))
  (check-true (mapped-error :left?))
  (check (mapped-error :get) => "network error")
) ;let*

(check-report)
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
        (liii error)
        (liii either)
) ;import

(check-set-mode! 'report-failed)


;; ==========================================
;; 1. 基础构造与提取测试
;; ==========================================

#|
from-left
创建 Left 值（通常代表错误或异常情况）。
|#
(check (to-left (from-left "error message")) => "error message")
(check (to-left (from-left 42)) => 42)
(check (to-left (from-left '())) => '())

#|
from-right
创建 Right 值（通常代表成功或有效数据）。
|#
(check (to-right (from-right "success data")) => "success data")
(check (to-right (from-right 100)) => 100)
(check (to-right (from-right '(1 2 3))) => '(1 2 3))


;; ==========================================
;; 2. 谓词测试
;; ==========================================

#|
either-left? / either-right?
类型判断函数。
|#
(let ((left-val (from-left "error"))
      (right-val (from-right "success")))
  (check-true (either-left? left-val))
  (check-false (either-right? left-val))
  (check-true (either-right? right-val))
  (check-false (either-left? right-val))
) ;let

;; 边界情况测试：非 Pair 不是 Either
(check-false (either-left? '()))
(check-false (either-right? '()))
(check-false (either-left? "string"))


;; ==========================================
;; 3. 高阶函数操作测试 (Map / For-Each)
;; ==========================================

#|
either-map
Functor 映射操作。
|#
(let ((left-val (from-left "error"))
      (right-val (from-right 5)))
  ;; 对左值应用 map 应该返回原值
  (check (to-left (either-map (lambda (x) (* x 2)) left-val)) => "error")
  ;; 对右值应用 map 应该应用函数
  (let ((result (either-map (lambda (x) (* x 2)) right-val)))
    (check-true (either-right? result))
    (check (to-right result) => 10)
  ) ;let
) ;let

#|
either-for-each
副作用遍历操作。
|#
(let ((counter 0)
      (left-val (from-left "error"))
      (right-val (from-right 5)))
  ;; 对左值应用 for-each 不执行
  (either-for-each (lambda (x) (set! counter (+ counter x))) left-val)
  (check counter => 0)
  ;; 对右值应用 for-each 执行
  (either-for-each (lambda (x) (set! counter (+ counter x))) right-val)
  (check counter => 5)
) ;let


;; ==========================================
;; 4. 实用函数测试 (Utility Functions)
;; ==========================================

#|
either-get-or-else
简单获取值或默认值。
|#
(check (either-get-or-else (from-right 42) 0) => 42)
(check (either-get-or-else (from-left "error") 0) => 0)

#|
either-or-else
Either 级别的备选方案。
|#
(let ((main (from-right 1))
      (backup (from-right 2))
      (fail (from-left 0)))
  (check (to-right (either-or-else main backup)) => 1)
  (check (to-right (either-or-else fail backup)) => 2)
) ;let


;; ==========================================
;; 5. 逻辑判断与过滤测试
;; ==========================================

#|
either-filter-or-else
条件过滤。

语法
----
(either-filter-or-else pred zero either)

描述
----
- 如果是 Right 且满足 pred -> 保持 Right
- 如果是 Right 且不满足 pred -> 变为 Left(zero)
- 如果是 Left -> 保持 Left
|#
(let ((r10 (from-right 10))   ; 偶数
      (r11 (from-right 11))   ; 奇数
      (l (from-left "orig"))) ; 原始错误

  ;; 1. Right 且满足条件 -> 保持原样
  (check (to-right (either-filter-or-else even? "err" r10)) => 10)
  
  ;; 2. Right 但不满足条件 -> 变为 Left("err")
  (let ((res (either-filter-or-else even? "Must be even" r11)))
    (check-true (either-left? res))
    (check (to-left res) => "Must be even")
  ) ;let
  
  ;; 3. Left -> 保持原样 (忽略条件)
  (let ((res-l (either-filter-or-else even? "Must be even" l)))
    (check-true (either-left? res-l))
    (check (to-left res-l) => "orig")
  ) ;let
) ;let

#|
either-contains
包含判断。

语法
----
(either-contains either x)
|#
(check-true (either-contains (from-right 10) 10))
(check-false (either-contains (from-right 11) 10)) ; 值不同
(check-false (either-contains (from-left 10) 10))  ; 状态不对

#|
either-every
全称量词 (空真性测试)。

语法
----
(either-every pred either)

描述
----
Right 必须满足 pred。
Left 总是返回 #t。
|#
(check-true (either-every even? (from-right 10)))
(check-false (either-every even? (from-right 11)))
(check-true (either-every even? (from-left "error"))) ; Left 总是 #t

#|
either-any
存在量词。

语法
----
(either-any pred either)

描述
----
Right 必须满足 pred。
Left 总是返回 #f。
|#
(check-true (either-any even? (from-right 10)))
(check-false (either-any even? (from-right 11)))
(check-false (either-any even? (from-left "error"))) ; Left 总是 #f


;; ==========================================
;; 6. 综合流程测试
;; ==========================================

;; 测试 Map 的连续使用
(let* ((val1 (from-right 10))
       (val2 (either-map (lambda (x) (+ x 5)) val1))     ;; Right 15
       (val3 (either-map (lambda (x) (* x 2)) val2)))    ;; Right 30
  (check-true (either-right? val3))
  (check (to-right val3) => 30)
) ;let*

;; 测试错误处理流程 (验证短路特性)
(let* ((error-val (from-left "network error"))
       ;; 下面的 map 不应执行，因为输入已经是 Left
       (mapped-error (either-map (lambda (x) (string-append "Error: " x)) error-val)))
  (check-true (either-left? mapped-error))
  (check (to-left mapped-error) => "network error")
) ;let*



;; ==========================================
;; 7. 异常与边界测试 (Check-Catch)
;; ==========================================

;; ------------------------------------------
;; A. 测试 check-either 类型守卫
;; 预期：所有函数接收非 Either 类型时，抛出 'type-error
;; ------------------------------------------

;; 提取函数防卫
(check-catch 'type-error (to-left "not-either"))
(check-catch 'type-error (to-left 123))
(check-catch 'type-error (to-right '()))

;; 高阶函数防卫
(check-catch 'type-error (either-map (lambda (x) x) "not-either"))
(check-catch 'type-error (either-for-each (lambda (x) x) "not-either"))

;; 逻辑函数防卫
(check-catch 'type-error (either-filter-or-else even? 0 "not-either"))
(check-catch 'type-error (either-contains "not-either" 1))
(check-catch 'type-error (either-every even? "not-either"))
(check-catch 'type-error (either-any even? "not-either"))

;; 实用函数防卫
(check-catch 'type-error (either-get-or-else "not-either" 0))
(check-catch 'type-error (either-or-else "not-either" (from-right 1)))

;; ------------------------------------------
;; B. 测试参数类型检查 (procedure?)
;; 预期：传入非函数参数时，抛出 'type-error
;; ------------------------------------------

(define valid-right (from-right 10))

(check-catch 'type-error (either-map "not-a-proc" valid-right))
(check-catch 'type-error (either-for-each "not-a-proc" valid-right))
(check-catch 'type-error (either-filter-or-else "not-a-proc" 0 valid-right))
(check-catch 'type-error (either-every "not-a-proc" valid-right))
(check-catch 'type-error (either-any "not-a-proc" valid-right))

;; ------------------------------------------
;; C. 测试逻辑错误 (Value Error)
;; 预期：违反 Either 语义的操作，抛出 'value-error
;; ------------------------------------------

;; 试图从 Right 中提取 Left
(check-catch 'value-error (to-left (from-right "I am Right")))

;; 试图从 Left 中提取 Right
(check-catch 'value-error (to-right (from-left "I am Left")))

(check-report)
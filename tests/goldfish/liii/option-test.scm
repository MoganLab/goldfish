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
) ;import

(check-set-mode! 'report-failed)

;; option?
;; 判断值是否为 option 类型。
;;
;; 语法
;; ----
;; (option? x)
;;
;; 参数
;; ----
;; x : any
;; 要判断的值。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #t 表示是 option 类型
;; - #f 表示不是 option 类型
;;
;; 说明
;; ----
;; 通过检查值的内部表示（cdr 是否为 'N 或 'S）来判断是否为 option。

(let ((opt1 (option 42))
      (opt2 (none)))
  (check (option? opt1) => #t)
  (check (option? opt2) => #t)
  (check (option? 42) => #f)
  (check (option? '()) => #f)
  (check (option? '(1 . 2)) => #f)
  (check (option? "hello") => #f)
) ;let

;; none
;; 创建空的 option 对象。
;;
;; 语法
;; ----
;; (none)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; 空的 option 对象，内部表示为 (cons #f 'N)。
;;
;; 说明
;; ----
;; 用于表示缺失值或空值。

;; option
;; 创建包含值的 option 对象。
;;
;; 语法
;; ----
;; (option value)
;;
;; 参数
;; ----
;; value : any
;; 要包装的值。
;;
;; 返回值
;; -----
;; 包含值的 option 对象，内部表示为 (cons value 'S)。

(let ((opt1 (option 42))
      (opt2 (option "hello"))
      (opt3 (none)))
  (check (option-defined? opt1) => #t)
  (check (option-defined? opt2) => #t)
  (check (option-empty? opt3) => #t)
  (check (option-defined? opt3) => #f)
) ;let

;; option-empty?
;; 判断 option 是否为空（none）。
;;
;; 语法
;; ----
;; (option-empty? opt)
;;
;; 参数
;; ----
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #t 表示 option 为空
;; - #f 表示 option 包含值

;; option-defined?
;; 判断 option 是否包含值。
;;
;; 语法
;; ----
;; (option-defined? opt)
;;
;; 参数
;; ----
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #t 表示 option 包含值
;; - #f 表示 option 为空

(let ((opt1 (option 42))
      (opt2 (none)))
  (check (option-empty? opt1) => #f)
  (check (option-defined? opt1) => #t)
  (check (option-empty? opt2) => #t)
  (check (option-defined? opt2) => #f)
) ;let

;; option-map
;; 对 option 中的值应用映射函数。
;;
;; 语法
;; ----
;; (option-map f opt)
;;
;; 参数
;; ----
;; f : procedure
;; 接受一个参数的映射函数。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 新的 option 对象：
;; - 如果原 option 为空：返回空 option
;; - 如果原 option 非空：返回包含 (f value) 的新 option

(let ((opt1 (option 42))
      (opt2 (none)))
  (check (option-map (lambda (x) (+ x 1)) opt1) => (option 43))
  (check (option-map (lambda (x) (+ x 1)) opt2) => (none))
  (check (option-map (lambda (x) (number->string x)) opt1) => (option "42"))
) ;let

;; option-filter
;; 根据谓词函数过滤 option 中的值。
;;
;; 语法
;; ----
;; (option-filter pred opt)
;;
;; 参数
;; ----
;; pred : procedure
;; 接受一个参数的谓词函数，返回布尔值。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; option 对象：
;; - 如果原 option 为空：返回空 option
;; - 如果值满足条件：返回原 option
;; - 如果值不满足条件：返回空 option

(let ((opt1 (option 42))
      (opt2 (none))
      (opt3 (option -5)))
  (check (option-filter (lambda (x) (> x 0)) opt1) => (option 42))
  (check (option-filter (lambda (x) (> x 100)) opt1) => (none))
  (check (option-filter (lambda (x) (> x 0)) opt2) => (none))
  (check (option-filter (lambda (x) (> x 0)) opt3) => (none))
) ;let

;; option-flat-map
;; 对 option 中的值应用返回 option 的映射函数。
;;
;; 语法
;; ----
;; (option-flat-map f opt)
;;
;; 参数
;; ----
;; f : procedure
;; 接受一个参数并返回 option 的函数。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; option 对象：
;; - 如果原 option 为空：返回空 option
;; - 如果原 option 非空：返回 (f value)

(let ((opt1 (option 42))
      (opt2 (none)))
  (check (option-flat-map (lambda (x) (option (+ x 1))) opt1) => (option 43))
  (check (option-flat-map (lambda (x) (option (+ x 1))) opt2) => (none))
  (check (option-flat-map (lambda (x) (none)) opt1) => (none))
) ;let

;; option-for-each
;; 对 option 中的值执行副作用操作。
;;
;; 语法
;; ----
;; (option-for-each f opt)
;;
;; 参数
;; ----
;; f : procedure
;; 接受一个参数的副作用函数。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 无（未定义）。
;;
;; 说明
;; ----
;; 如果 option 为空，则不执行任何操作。

(let ((opt1 (option 42))
      (opt2 (none))
      (result '()))
  (option-for-each (lambda (x) (set! result (cons x result))) opt1)
  (check result => '(42))
  (set! result '())
  (option-for-each (lambda (x) (set! result (cons x result))) opt2)
  (check result => '())
) ;let

;; option-get
;; 获取 option 中的值，如果为空则报错。
;;
;; 语法
;; ----
;; (option-get opt)
;;
;; 参数
;; ----
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; option 中的值。
;;
;; 异常
;; ----
;; 如果 option 为空，抛出错误。

(let ((opt1 (option 42)))
  (check (option-get opt1) => 42)
) ;let

;; option-get-or-else
;; 获取 option 中的值，如果为空则返回默认值。
;;
;; 语法
;; ----
;; (option-get-or-else default opt)
;;
;; 参数
;; ----
;; default : any or procedure
;; 默认值或返回默认值的函数。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; option 中的值（如果非空）或默认值（如果为空）。

(let ((opt1 (option 42))
      (opt2 (none)))
  (check (option-get-or-else 0 opt1) => 42)
  (check (option-get-or-else 0 opt2) => 0)
  (check (option-get-or-else (lambda () "default") opt2) => "default")
) ;let

;; option-or-else
;; 如果当前 option 为空，则返回备选的 option。
;;
;; 语法
;; ----
;; (option-or-else alt opt)
;;
;; 参数
;; ----
;; alt : option
;; 备选 option。
;; opt : option
;; 当前 option 对象。
;;
;; 返回值
;; -----
;; 当前 option（如果非空）或备选 option（如果当前为空）。

(let ((opt1 (option 42))
      (opt2 (option 0))
      (opt3 (none)))
  (check (option-or-else opt2 opt1) => (option 42))
  (check (option-or-else opt1 opt3) => (option 42))
  (check (option-or-else opt2 opt3) => (option 0))
  (check (option-or-else opt1 opt1) => (option 42))
) ;let

;; option=?
;; 比较两个 option 是否相等。
;;
;; 语法
;; ----
;; (option=? opt1 opt2)
;;
;; 参数
;; ----
;; opt1 : option
;; 第一个 option 对象。
;; opt2 : option
;; 第二个 option 对象。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #t 如果两个 option 都为空，或都包含相等的值（使用 equal? 比较）
;; - #f 否则

(let ((opt1 (option 42))
      (opt2 (option 42))
      (opt3 (option "hello"))
      (opt4 (none))
      (opt5 (none)))
  (check (option=? opt1 opt2) => #t)
  (check (option=? opt1 opt3) => #f)
  (check (option=? opt4 opt5) => #t)
  (check (option=? opt1 opt4) => #f)
) ;let

;; option-every
;; 检查 option 中的值是否满足谓词函数（全称量词）。
;;
;; 语法
;; ----
;; (option-every pred opt)
;;
;; 参数
;; ----
;; pred : procedure
;; 接受一个参数的谓词函数，返回布尔值。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #f 如果 option 为空
;; - (pred value) 的结果如果 option 非空

(let ((opt1 (option 42))
      (opt2 (none))
      (opt3 (option -5)))
  (check (option-every (lambda (x) (> x 0)) opt1) => #t)
  (check (option-every (lambda (x) (> x 0)) opt2) => #f)
  (check (option-every (lambda (x) (> x 0)) opt3) => #f)
  (check (option-every (lambda (x) (number? x)) opt1) => #t)
) ;let

;; option-any
;; 检查 option 中的值是否满足谓词函数（存在量词）。
;;
;; 语法
;; ----
;; (option-any pred opt)
;;
;; 参数
;; ----
;; pred : procedure
;; 接受一个参数的谓词函数，返回布尔值。
;; opt : option
;; option 对象。
;;
;; 返回值
;; -----
;; 布尔值：
;; - #f 如果 option 为空
;; - (pred value) 的结果如果 option 非空

(let ((opt1 (option 42))
      (opt2 (none))
      (opt3 (option -5)))
  (check (option-any (lambda (x) (> x 0)) opt1) => #t)
  (check (option-any (lambda (x) (> x 0)) opt2) => #f)
  (check (option-any (lambda (x) (> x 0)) opt3) => #f)
  (check (option-any (lambda (x) (number? x)) opt1) => #t)
  (check (option-any (lambda (x) (string? x)) opt1) => #f)
) ;let

(check-report)

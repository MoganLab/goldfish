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
        (liii enum)
        (srfi srfi-1)
        (srfi srfi-128)
) ;import

(check-set-mode! 'report-failed)

;;; 测试数据

(define color-names
  '(red tangerine orange yellow green cyan blue violet)
) ;define

(define color (make-enum-type color-names))

(define color-red (enum-name->enum color 'red))
(define color-tangerine (enum-name->enum color 'tangerine))
(define color-blue (enum-name->enum color 'blue))
(define color-green (enum-name->enum color 'green))
(define color-set (enum-type->enum-set color))

(define reddish (list->enum-set
                  color
                  (map (lambda (name)
                         (enum-name->enum color name))
                       (take color-names 3))
                  ) ;map
) ;define

(define ~reddish (list->enum-set
                   color
                   (map (lambda (ord)
                          (enum-name->enum color ord))
                        (drop color-names 3))
                   ) ;map
) ;define

(define empty-colors (enum-empty-set color))

(define pizza-descriptions
  '((margherita "tomato and mozzarella")
    (funghi     "mushrooms")
    (bianca     "ricotta and mozzarella")
    (chicago    "deep-dish")
    (hawaiian   "pineapple and ham"))
) ;define

(define pizza-names (map car pizza-descriptions))
(define pizza (make-enum-type pizza-descriptions))
(define pizza-chicago (enum-name->enum pizza 'chicago))
(define pizza-bianca (enum-name->enum pizza 'bianca))

;;; 工具函数

(define (constantly obj)
  (lambda _ obj)
) ;define

(define always (constantly #t))
(define never (constantly #f))

(define (fresh-sets proc eset1 eset2)
  (proc (enum-set-copy eset1) (enum-set-copy eset2))
) ;define

;;;; 谓词

;; enum-type?
;; 判断对象是否为 enum-type。
;;
;; 语法
;; ----
;; (enum-type? obj)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-type? color) => #t)
(check (enum-type? color-red) => #f)
(check (enum-type? 'not-a-type) => #f)

;; enum?
;; 判断对象是否为 enum。
;;
;; 语法
;; ----
;; (enum? obj)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum? color-red) => #t)
(check (enum? color) => #f)
(check (enum? 'z) => #f)

;; enum-type-contains?
;; 判断 enum 是否属于指定的 enum-type。
;;
;; 语法
;; ----
;; (enum-type-contains? enum-type enum)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-type-contains? color (enum-name->enum color 'red)) => #t)
(check (enum-type-contains? pizza (enum-name->enum color 'red)) => #f)

;;;; Enum 类型构造函数

;; make-enum-type
;; 创建一个新的 enum-type。
;;
;; 语法
;; ----
;; (make-enum-type list)
;;
;; 参数
;; ----
;; list : 列表，元素可以是符号或 (符号 值) 对
;;
;; 返回值
;; ------
;; enum-type

(check (enum-type? (make-enum-type '(a b c))) => #t)
(check (enum-type? (make-enum-type '((a 1) (b 2)))) => #t)

;;;; Enum 访问器

;; enum-type
;; 获取 enum 所属的 enum-type。
;;
;; 语法
;; ----
;; (enum-type enum)
;;
;; 返回值
;; ------
;; enum-type

(check (eqv? color (enum-type (enum-name->enum color 'red))) => #t)

;; enum-name
;; 获取 enum 的名称（符号）。
;;
;; 语法
;; ----
;; (enum-name enum)
;;
;; 返回值
;; ------
;; 符号

(check (enum-name (enum-name->enum color 'red)) => 'red)
(check (enum-name (enum-ordinal->enum color 0)) => 'red)

;; enum-ordinal
;; 获取 enum 的序数（从 0 开始）。
;;
;; 语法
;; ----
;; (enum-ordinal enum)
;;
;; 返回值
;; ------
;; 精确整数

(check (enum-ordinal (enum-name->enum color 'red)) => 0)
(check (enum-ordinal (enum-name->enum color 'blue)) => 6)
(check (enum-ordinal (enum-ordinal->enum color 0)) => 0)

;; enum-value
;; 获取 enum 的值。
;;
;; 语法
;; ----
;; (enum-value enum)
;;
;; 返回值
;; ------
;; 任意对象（如果定义时未指定值，则返回序数）

(check (enum-value (enum-name->enum pizza 'funghi)) => "mushrooms")
(check (enum-value (enum-name->enum color 'blue)) => 6)

;;;; Enum 查找器

;; enum-name->enum
;; 通过名称从 enum-type 中获取对应的 enum 对象。
;;
;; 语法
;; ----
;; (enum-name->enum enum-type symbol)
;;
;; 返回值
;; ------
;; enum 对象，如果未找到则返回 #f

(check (enum-name (enum-name->enum color 'green)) => 'green)
(check (enum-name->enum color 'mushroom) => #f)

;; enum-ordinal->enum
;; 通过序数从 enum-type 中获取对应的 enum 对象。
;;
;; 语法
;; ----
;; (enum-ordinal->enum enum-type exact-integer)
;;
;; 返回值
;; ------
;; enum 对象，如果序数超出范围则返回 #f

(check (enum-name (enum-ordinal->enum color 3)) => 'yellow)
(check (enum-ordinal->enum color 10) => #f)

;; enum-name->ordinal
;; 通过名称获取 enum 的序数。
;;
;; 语法
;; ----
;; (enum-name->ordinal enum-type symbol)
;;
;; 返回值
;; ------
;; 精确整数，如果未找到则报错

(check (enum-name->ordinal color 'red) => 0)
(check (enum-name->ordinal color 'blue) => 6)

;; enum-name->value
;; 通过名称获取 enum 的值。
;;
;; 语法
;; ----
;; (enum-name->value enum-type symbol)
;;
;; 返回值
;; ------
;; 任意对象，如果未找到则报错

(check (enum-name->value pizza 'funghi) => "mushrooms")
(check (enum-name->value color 'blue) => 6)

;; enum-ordinal->name
;; 通过序数获取 enum 的名称。
;;
;; 语法
;; ----
;; (enum-ordinal->name enum-type exact-integer)
;;
;; 返回值
;; ------
;; 符号，如果序数无效则报错

(check (enum-ordinal->name color 0) => 'red)
(check (enum-ordinal->name pizza 3) => 'chicago)

;; enum-ordinal->value
;; 通过序数获取 enum 的值。
;;
;; 语法
;; ----
;; (enum-ordinal->value enum-type exact-integer)
;;
;; 返回值
;; ------
;; 任意对象，如果序数无效则报错

(check (enum-ordinal->value pizza 1) => "mushrooms")

;;;; Enum 类型访问器

;; enum-type-size
;; 获取 enum-type 中 enum 的数量。
;;
;; 语法
;; ----
;; (enum-type-size enum-type)
;;
;; 返回值
;; ------
;; 精确整数

(check (enum-type-size color) => (length color-names))
(check (enum-type-size pizza) => (length pizza-names))

;; enum-min
;; 获取 enum-type 中最小的 enum（序数为 0）。
;;
;; 语法
;; ----
;; (enum-min enum-type)
;;
;; 返回值
;; ------
;; enum 对象

(check (enum-name (enum-min color)) => 'red)
(check (enum-name (enum-min pizza)) => 'margherita)

;; enum-max
;; 获取 enum-type 中最大的 enum。
;;
;; 语法
;; ----
;; (enum-max enum-type)
;;
;; 返回值
;; ------
;; enum 对象

(check (enum-name (enum-max color)) => 'violet)
(check (enum-name (enum-max pizza)) => 'hawaiian)

;; enum-type-enums
;; 获取 enum-type 中所有 enum 的列表，按序数排序。
;;
;; 语法
;; ----
;; (enum-type-enums enum-type)
;;
;; 返回值
;; ------
;; enum 对象列表

(check (enum-type-size color) => (length (enum-type-enums color)))
(check color-names => (map enum-name (enum-type-enums color)))
(check (iota (enum-type-size color))
       => (map enum-ordinal (enum-type-enums color))
) ;check

;; enum-type-names
;; 获取 enum-type 中所有 enum 名称的列表，按序数排序。
;;
;; 语法
;; ----
;; (enum-type-names enum-type)
;;
;; 返回值
;; ------
;; 符号列表

(check (enum-type-names color) => color-names)
(check (enum-type-names pizza) => pizza-names)

;; enum-type-values
;; 获取 enum-type 中所有 enum 值的列表，按序数排序。
;;
;; 语法
;; ----
;; (enum-type-values enum-type)
;;
;; 返回值
;; ------
;; 列表

(check (map cadr pizza-descriptions) => (enum-type-values pizza))
(check (iota (enum-type-size color)) => (enum-type-values color))

;;;; Enum 比较谓词

;; enum=?
;; 判断多个 enum 是否相等（同一对象）。
;;
;; 语法
;; ----
;; (enum=? enum1 enum2 ...)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum=? color-red (enum-ordinal->enum color 0)) => #t)
(check (enum=? color-red color-tangerine) => #f)
(check (enum=? color-red color-red color-red) => #t)
(check (enum=? color-red color-red color-tangerine) => #f)

;; enum<?
;; 判断 enum 的序数是否严格递增。
;;
;; 语法
;; ----
;; (enum<? enum1 enum2 ...)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum<? color-red color-tangerine) => #t)
(check (enum<? color-tangerine color-tangerine) => #f)
(check (enum<? color-tangerine color-red) => #f)
(check (enum<? color-red color-green color-blue) => #t)
(check (enum<? color-red color-blue color-blue) => #f)

;; enum>?
;; 判断 enum 的序数是否严格递减。
;;
;; 语法
;; ----
;; (enum>? enum1 enum2 ...)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum>? color-blue color-green color-red) => #t)
(check (enum>? color-blue color-red color-red) => #f)
(check (enum>? color-red color-tangerine) => #f)

;; enum<=?
;; 判断 enum 的序数是否非递减。
;;
;; 语法
;; ----
;; (enum<=? enum1 enum2 ...)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum<=? color-red color-tangerine) => #t)
(check (enum<=? color-tangerine color-tangerine) => #t)
(check (enum<=? color-tangerine color-red) => #f)
(check (enum<=? color-red color-blue color-blue) => #t)
(check (enum<=? color-blue color-blue color-red) => #f)

;; enum>=?
;; 判断 enum 的序数是否非递增。
;;
;; 语法
;; ----
;; (enum>=? enum1 enum2 ...)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum>=? color-blue color-red color-red) => #t)
(check (enum>=? color-blue color-red color-blue) => #f)
(check (enum>=? color-tangerine color-red) => #t)

;;;; Enum 操作

;; enum-next
;; 获取同一 enum-type 中序数下一个的 enum。
;;
;; 语法
;; ----
;; (enum-next enum)
;;
;; 返回值
;; ------
;; enum 对象或 #f（如果已经是最后一个）

(check (enum=? (enum-next color-red) color-tangerine) => #t)
(check (enum=? (enum-next pizza-bianca) pizza-chicago) => #t)
(check (enum-next (enum-max color)) => #f)

;; enum-prev
;; 获取同一 enum-type 中序数前一个的 enum。
;;
;; 语法
;; ----
;; (enum-prev enum)
;;
;; 返回值
;; ------
;; enum 对象或 #f（如果已经是第一个）

(check (enum=? (enum-prev color-tangerine) color-red) => #t)
(check (enum=? (enum-prev pizza-chicago) pizza-bianca) => #t)
(check (enum-prev (enum-min color)) => #f)

;;;; 比较器

;; make-enum-comparator
;; 创建一个 SRFI-128 比较器，用于比较同一 enum-type 的 enum。
;;
;; 语法
;; ----
;; (make-enum-comparator enum-type)
;;
;; 返回值
;; ------
;; 比较器对象

(define pizza-comparator (make-enum-comparator pizza))

(check (comparator? pizza-comparator) => #t)
(check (comparator-ordered? pizza-comparator) => #t)
(check (comparator-hashable? pizza-comparator) => #t)
(check (=? pizza-comparator pizza-chicago (enum-name->enum pizza 'chicago)) => #t)
(check (=? pizza-comparator pizza-bianca pizza-chicago) => #f)
(check (<? pizza-comparator pizza-bianca pizza-chicago) => #t)
(check (<? pizza-comparator pizza-bianca pizza-bianca) => #f)
(check (>? pizza-comparator pizza-chicago pizza-bianca) => #t)
(check (<=? pizza-comparator pizza-bianca pizza-chicago) => #t)
(check (>=? pizza-comparator pizza-chicago pizza-bianca) => #t)

;;;; Enum 集合构造函数

;; enum-empty-set
;; 创建一个空的 enum-set。
;;
;; 语法
;; ----
;; (enum-empty-set enum-type)
;;
;; 返回值
;; ------
;; enum-set

(check (enum-set-empty? (enum-empty-set pizza)) => #t)
(check (enum-set-empty? empty-colors) => #t)
(check (enum-set-empty? color-set) => #f)

;; enum-type->enum-set
;; 创建一个包含 enum-type 中所有 enum 的 enum-set。
;;
;; 语法
;; ----
;; (enum-type->enum-set enum-type)
;;
;; 返回值
;; ------
;; enum-set

(check (enum-set-contains? color-set color-blue) => #t)
(check (enum-set-contains? (enum-type->enum-set pizza) pizza-chicago) => #t)

;; enum-set
;; 创建一个包含指定 enum 的 enum-set。
;;
;; 语法
;; ----
;; (enum-set enum-type enum ...)
;;
;; 返回值
;; ------
;; enum-set

(check (enum-set-contains? (enum-set color color-red color-blue) color-red) => #t)
(check (enum-set-contains? (enum-set color color-red color-blue) color-tangerine)
       =>
       #f
) ;check

;; list->enum-set
;; 从 enum 列表创建 enum-set。
;;
;; 语法
;; ----
;; (list->enum-set enum-type list)
;;
;; 返回值
;; ------
;; enum-set

(check (enum-set-contains? (list->enum-set pizza (list pizza-chicago pizza-bianca))
                           pizza-chicago)
       =>
       #t
) ;check

;; enum-set-projection
;; 将一个 enum-set 投影到另一个 enum-type。
;;
;; 语法
;; ----
;; (enum-set-projection enum-type-or-set enum-set)
;;
;; 返回值
;; ------
;; 新的 enum-set

(check (enum-set=? (enum-set-projection color reddish) reddish) => #t)

;; enum-set-copy
;; 复制一个 enum-set。
;;
;; 语法
;; ----
;; (enum-set-copy enum-set)
;;
;; 返回值
;; ------
;; 新的 enum-set

(check (eqv? color-set (enum-set-copy color-set)) => #f)
(check (enum-set=? color-set (enum-set-copy color-set)) => #t)

;;;; Enum 集合谓词

;; enum-set?
;; 判断对象是否为 enum-set。
;;
;; 语法
;; ----
;; (enum-set? obj)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set? color-set) => #t)
(check (enum-set? color) => #f)

;; enum-set-contains?
;; 判断 enum 是否是 enum-set 的成员。
;;
;; 语法
;; ----
;; (enum-set-contains? enum-set enum)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set-contains? color-set color-blue) => #t)
(check (enum-set-contains? (enum-set-delete! (enum-set-copy color-set) color-blue)
                           color-blue)
       =>
       #f
) ;check

;; enum-set-empty?
;; 判断 enum-set 是否为空。
;;
;; 语法
;; ----
;; (enum-set-empty? enum-set)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set-empty? color-set) => #f)
(check (enum-set-empty? (enum-set-delete-all! (enum-set-copy color-set)
                                              (enum-type-enums color)))
       =>
       #t
) ;check

;; enum-set-disjoint?
;; 判断两个 enum-set 是否不相交。
;;
;; 语法
;; ----
;; (enum-set-disjoint? enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set-disjoint? color-set empty-colors) => #t)
(check (enum-set-disjoint? color-set reddish) => #f)
(check (enum-set-disjoint? reddish ~reddish) => #t)

;; enum-set=?
;; 判断两个 enum-set 是否相等。
;;
;; 语法
;; ----
;; (enum-set=? enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set=? color-set (enum-set-copy color-set)) => #t)
(check (enum-set=? color-set empty-colors) => #f)

;; enum-set<?
;; 判断 enum-set1 是否是 enum-set2 的真子集。
;;
;; 语法
;; ----
;; (enum-set<? enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set<? reddish color-set) => #t)
(check (enum-set<? color-set reddish) => #f)
(check (enum-set<? color-set color-set) => #f)

;; enum-set>?
;; 判断 enum-set1 是否是 enum-set2 的真超集。
;;
;; 语法
;; ----
;; (enum-set>? enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set>? color-set reddish) => #t)
(check (enum-set>? reddish color-set) => #f)
(check (enum-set>? color-set color-set) => #f)

;; enum-set<=?
;; 判断 enum-set1 是否是 enum-set2 的子集。
;;
;; 语法
;; ----
;; (enum-set<=? enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set<=? reddish color-set) => #t)
(check (enum-set<=? color-set reddish) => #f)
(check (enum-set<=? color-set color-set) => #t)

;; enum-set>=?
;; 判断 enum-set1 是否是 enum-set2 的超集。
;;
;; 语法
;; ----
;; (enum-set>=? enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set>=? color-set reddish) => #t)
(check (enum-set>=? reddish color-set) => #f)
(check (enum-set>=? color-set color-set) => #t)

;; enum-set-subset?
;; 判断 enum-set1 的名称集合是否是 enum-set2 名称集合的子集。
;;
;; 语法
;; ----
;; (enum-set-subset? enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set-subset? reddish color-set) => #t)
(check (enum-set-subset? color-set reddish) => #f)
(check (enum-set-subset? reddish reddish) => #t)

;; enum-set-any?
;; 判断 enum-set 中是否存在满足谓词的 enum。
;;
;; 语法
;; ----
;; (enum-set-any? pred enum-set)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set-any? (lambda (e) (eq? 'green (enum-name e))) color-set) => #t)
(check (enum-set-any? (lambda (e) (eq? 'mauve (enum-name e))) color-set) => #f)
(check (enum-set-any? never empty-colors) => #f)

;; enum-set-every?
;; 判断 enum-set 中是否所有 enum 都满足谓词。
;;
;; 语法
;; ----
;; (enum-set-every? pred enum-set)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set-every? (lambda (e) (< (enum-ordinal e) 10)) color-set) => #t)
(check (enum-set-every? never empty-colors) => #t)

;;;; Enum 集合访问器

;; enum-set-type
;; 获取 enum-set 所属的 enum-type。
;;
;; 语法
;; ----
;; (enum-set-type enum-set)
;;
;; 返回值
;; ------
;; enum-type

(check (eqv? (enum-set-type color-set) color) => #t)
(check (eqv? (enum-set-type (enum-type->enum-set pizza)) pizza) => #t)

;;;; Enum 集合修改器

;; enum-set-adjoin
;; 返回包含原 enum-set 所有成员和指定 enum 的新 enum-set。
;;
;; 语法
;; ----
;; (enum-set-adjoin enum-set enum ...)
;;
;; 返回值
;; ------
;; 新的 enum-set

(let ((reddish+green (enum-set-adjoin reddish color-green)))
  (check (enum-set<? reddish reddish+green) => #t)
  (check (enum-set-contains? reddish+green color-green) => #t)
) ;let

;; enum-set-adjoin!
;; 线性更新版本，可能修改原 enum-set。
;;
;; 语法
;; ----
;; (enum-set-adjoin! enum-set enum ...)
;;
;; 返回值
;; ------
;; enum-set

(let ((reddish+green (enum-set-adjoin! (enum-set-copy reddish) color-green)))
  (check (enum-set<? reddish reddish+green) => #t)
  (check (enum-set-contains? reddish+green color-green) => #t)
) ;let

;; enum-set-delete
;; 返回移除指定 enum 的新 enum-set。
;;
;; 语法
;; ----
;; (enum-set-delete enum-set enum ...)
;;
;; 返回值
;; ------
;; 新的 enum-set

(let ((reddish* (enum-set-delete reddish color-tangerine)))
  (check (enum-set<? reddish* reddish) => #t)
  (check (enum-set-contains? reddish* color-tangerine) => #f)
) ;let

;; enum-set-delete!
;; 线性更新版本。
;;
;; 语法
;; ----
;; (enum-set-delete! enum-set enum ...)
;;
;; 返回值
;; ------
;; enum-set

(let ((reddish* (enum-set-delete! (enum-set-copy reddish) color-tangerine)))
  (check (enum-set<? reddish* reddish) => #t)
  (check (enum-set-contains? reddish* color-tangerine) => #f)
) ;let

;; enum-set-delete-all
;; 返回移除列表中所有 enum 的新 enum-set。
;;
;; 语法
;; ----
;; (enum-set-delete-all enum-set list)
;;
;; 返回值
;; ------
;; 新的 enum-set

(let ((reddish* (enum-set-delete-all reddish (list color-tangerine))))
  (check (enum-set<? reddish* reddish) => #t)
) ;let

;; enum-set-delete-all!
;; 线性更新版本。
;;
;; 语法
;; ----
;; (enum-set-delete-all! enum-set list)
;;
;; 返回值
;; ------
;; enum-set

(let ((reddish** (enum-set-delete-all! (enum-set-copy reddish)
                                       (list color-tangerine))))
  (check (enum-set<? reddish** reddish) => #t)
) ;let

;;;; Enum 集合操作

;; enum-set-size
;; 获取 enum-set 中 enum 的数量。
;;
;; 语法
;; ----
;; (enum-set-size enum-set)
;;
;; 返回值
;; ------
;; 精确整数

(check (enum-set-size color-set) => (length color-names))
(check (enum-set-size empty-colors) => 0)

;; enum-set->enum-list
;; 将 enum-set 转换为 enum 列表，按序数排序。
;;
;; 语法
;; ----
;; (enum-set->enum-list enum-set)
;;
;; 返回值
;; ------
;; enum 列表

(check (enum-type-enums color) => (enum-set->enum-list color-set))
(check (null? (enum-set->enum-list empty-colors)) => #t)

;; enum-set->list
;; 将 enum-set 转换为名称列表，按序数排序。
;;
;; 语法
;; ----
;; (enum-set->list enum-set)
;;
;; 返回值
;; ------
;; 符号列表

(check color-names => (enum-set->list color-set))
(check (map car pizza-descriptions) => (enum-set->list (enum-type->enum-set pizza)))

;; enum-set-map->list
;; 对 enum-set 中每个 enum 应用函数，返回结果列表。
;;
;; 语法
;; ----
;; (enum-set-map->list proc enum-set)
;;
;; 返回值
;; ------
;; 列表

(check color-names => (enum-set-map->list enum-name color-set))
(check (null? (enum-set-map->list enum-name empty-colors)) => #t)

;; enum-set-count
;; 统计 enum-set 中满足谓词的 enum 数量。
;;
;; 语法
;; ----
;; (enum-set-count pred enum-set)
;;
;; 返回值
;; ------
;; 精确整数

(check (enum-set-count (lambda (e) (enum=? e color-blue)) color-set) => 1)
(check (enum-set-count (lambda (e) (enum=? e color-blue)) reddish) => 0)

;; enum-set-filter
;; 返回满足谓词的所有 enum 组成的新 enum-set。
;;
;; 语法
;; ----
;; (enum-set-filter pred enum-set)
;;
;; 返回值
;; ------
;; 新的 enum-set

(check (enum-set<? (enum-set-filter (lambda (e) (enum=? e color-red)) color-set)
                    color-set)
       =>
       #t
) ;check
(check (enum-set=? (enum-set-filter always color-set) color-set) => #t)
(check (enum-set-empty? (enum-set-filter never color-set)) => #t)

;; enum-set-filter!
;; 线性更新版本。
;;
;; 语法
;; ----
;; (enum-set-filter! pred enum-set)
;;
;; 返回值
;; ------
;; enum-set

(let ((filtered (enum-set-filter! (lambda (e) (enum=? e color-red))
                                  (enum-set-copy color-set))))
  (check (enum-set-size filtered) => 1)
  (check (enum-set-contains? filtered color-red) => #t)
) ;let

;; enum-set-remove
;; 返回不满足谓词的所有 enum 组成的新 enum-set。
;;
;; 语法
;; ----
;; (enum-set-remove pred enum-set)
;;
;; 返回值
;; ------
;; 新的 enum-set

(check (enum-set=? (enum-set-remove never color-set) color-set) => #t)
(check (enum-set-empty? (enum-set-remove always color-set)) => #t)

;; enum-set-remove!
;; 线性更新版本。
;;
;; 语法
;; ----
;; (enum-set-remove! pred enum-set)
;;
;; 返回值
;; ------
;; enum-set

(let ((removed (enum-set-remove! (lambda (e) (enum=? e color-red))
                                 (enum-set-copy color-set))))
  (check (enum-set-contains? removed color-red) => #f)
) ;let

;; enum-set-for-each
;; 对 enum-set 中每个 enum 应用过程。
;;
;; 语法
;; ----
;; (enum-set-for-each proc enum-set)
;;
;; 返回值
;; ------
;; 未指定

(check
  (let ((n 0))
    (enum-set-for-each (lambda (_) (set! n (+ n 1))) color-set)
    n
  ) ;let
  =>
  (length color-names)
) ;check

;; enum-set-fold
;; 对 enum-set 中每个 enum 进行折叠操作。
;;
;; 语法
;; ----
;; (enum-set-fold proc nil enum-set)
;;
;; 返回值
;; ------
;; 折叠结果

(check (enum-set-fold (lambda (enum lis) (cons (enum-name enum) lis))
                      '()
                      color-set)
       =>
       (reverse color-names)
) ;check

;;;; Enum 集合逻辑操作

;; enum-set-union
;; 返回两个 enum-set 的并集。
;;
;; 语法
;; ----
;; (enum-set-union enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 新的 enum-set

(check (enum-set=? color-set (enum-set-union reddish ~reddish)) => #t)

;; enum-set-intersection
;; 返回两个 enum-set 的交集。
;;
;; 语法
;; ----
;; (enum-set-intersection enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 新的 enum-set

(check (enum-set-empty? (enum-set-intersection reddish ~reddish)) => #t)

;; enum-set-difference
;; 返回在 enum-set1 中但不在 enum-set2 中的 enum 组成的新 enum-set。
;;
;; 语法
;; ----
;; (enum-set-difference enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 新的 enum-set

(check (enum-set=? ~reddish (enum-set-difference color-set reddish)) => #t)

;; enum-set-xor
;; 返回两个 enum-set 的对称差集。
;;
;; 语法
;; ----
;; (enum-set-xor enum-set1 enum-set2)
;;
;; 返回值
;; ------
;; 新的 enum-set

(check (enum-set=? color-set (enum-set-xor reddish ~reddish)) => #t)
(check (enum-set-empty? (enum-set-xor reddish reddish)) => #t)

;; 线性更新版本

(check (enum-set=? color-set (fresh-sets enum-set-union! reddish ~reddish)) => #t)
(check (enum-set-empty? (fresh-sets enum-set-intersection! reddish ~reddish)) =>
       #t
) ;check
(check (enum-set=? ~reddish (fresh-sets enum-set-difference! color-set reddish))
       =>
       #t
) ;check
(check (enum-set=? color-set (fresh-sets enum-set-xor! reddish ~reddish)) => #t)

;; enum-set-complement
;; 返回 enum-set 的补集（相对于其 enum-type 的全集）。
;;
;; 语法
;; ----
;; (enum-set-complement enum-set)
;;
;; 返回值
;; ------
;; 新的 enum-set

(check (enum-set-empty? (enum-set-complement color-set)) => #t)
(check (enum-set=? (enum-set-complement reddish) ~reddish) => #t)

;; enum-set-complement!
;; 线性更新版本。
;;
;; 语法
;; ----
;; (enum-set-complement! enum-set)
;;
;; 返回值
;; ------
;; enum-set

(check (enum-set-empty? (enum-set-complement! (enum-set-copy color-set))) => #t)
(check (enum-set=? (enum-set-complement! (enum-set-copy reddish)) ~reddish) =>
       #t
) ;check

;;;; R6RS 兼容性过程

;; make-enumeration
;; 创建 enum-type 和包含所有 enum 的 enum-set。
;;
;; 语法
;; ----
;; (make-enumeration symbol-list)
;;
;; 返回值
;; ------
;; enum-set

(let* ((ds '(red yellow green))
       (us-traffic-light (make-enumeration ds))
       (light-type (enum-set-type us-traffic-light)))
  (check (enum-set-every? (lambda (e) (eqv? (enum-name e) (enum-value e)))
                          us-traffic-light)
         =>
         #t
  ) ;check
) ;let*

;; enum-set-universe
;; 获取包含 enum-type 所有 enum 的 enum-set。
;;
;; 语法
;; ----
;; (enum-set-universe enum-set)
;;
;; 返回值
;; ------
;; enum-set

(check (enum-set=? color-set (enum-set-universe reddish)) => #t)

;; enum-set-constructor
;; 返回一个构造 enum-set 的过程。
;;
;; 语法
;; ----
;; (enum-set-constructor enum-set)
;;
;; 返回值
;; ------
;; 过程

(let ((color-con (enum-set-constructor reddish)))
  (check (eqv? (enum-set-type (color-con '(green))) color) => #t)
  (check (enum-set=? (color-con color-names) color-set) => #t)
) ;let

;; enum-set-indexer
;; 返回一个将名称映射到序数的过程。
;;
;; 语法
;; ----
;; (enum-set-indexer enum-set)
;;
;; 返回值
;; ------
;; 过程

(let ((idx (enum-set-indexer reddish)))
  (check (idx 'red) => 0)
  (check (idx 'green) => 4)
  (check (idx 'margherita) => #f)
) ;let

;; enum-set-member?
;; 判断符号是否是 enum-set 的成员名称。
;;
;; 语法
;; ----
;; (enum-set-member? symbol enum-set)
;;
;; 返回值
;; ------
;; 布尔值

(check (enum-set-member? 'red reddish) => #t)
(check (enum-set-member? 'blue reddish) => #f)

(check-report)

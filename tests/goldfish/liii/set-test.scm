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

(import (scheme base)
        (scheme char)
        (liii check)
        (liii set)
        (srfi srfi-128)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; --- Data Setup ---
(define s-empty (set))
(define comp (set-element-comparator s-empty))
(define s-1 (set 1))
(define s-1-2 (set 1 2))
(define s-1-2-3 (set 1 2 3))
(define s-2-3-4 (set 2 3 4))
(define s-4-5 (set 4 5))

#|
set?
检查对象是否为 set。

语法
----
(set? obj)

参数
----
obj : any
要检查的对象。

返回值
-----
如果 obj 是 set，返回 #t；否则返回 #f。
|#
(check-true (set? s-empty))
(check-true (set? s-1))
(check-false (set? "not a set"))
(check-false (set? '()))
(check-false (set? #(1 2 3)))


#|
set
创建一个新的 set。

语法
----
(set element ...)

参数
----
element ... : any
初始元素。

返回值
-----
返回包含指定元素的 set。
|#
(check-true (set? (set 1 2 3)))
(check-true (set-contains? (set 1) 1))

#|
list->set
将列表转换为 set。

语法
----
(list->set list)

参数
----

list : list
要转换的列表。

返回值
-----
返回包含列表中所有元素的新 set（使用默认比较器，重复元素会被去重）。
|#
(define s-list-1 (list->set '(1 2 3)))
(check-true (set? s-list-1))
(check-true (eq? (set-element-comparator s-list-1) comp))
(check-true (set=? s-1-2-3 s-list-1))
(check-false (eq? s-1-2-3 s-list-1))

(define s-list-empty (list->set '()))
(check-true (set=? s-empty s-list-empty))
(check (set-size s-list-empty) => 0)

;; Duplicates in list should be handled
(define s-list-dup (list->set '(1 2 2 1)))
(check-true (set=? s-1-2 s-list-dup))
(check (set-size s-list-dup) => 2)


#|
list->set!
将列表元素并入 set（可变操作）。

语法
----
(list->set! set list)

参数
----
set : set
目标 set。

list : list
要并入的元素列表。

返回值
------
返回修改后的 set（与传入的 set 是同一个对象）。
|#

;; 测试 list->set! 基本行为
(define s-list-merge (set 1 2))
(define s-list-merge-result (list->set! s-list-merge '(2 3 4)))
(check-true (eq? s-list-merge-result s-list-merge))
(check (set-size s-list-merge) => 4)
(check-true (set-contains? s-list-merge 1))
(check-true (set-contains? s-list-merge 2))
(check-true (set-contains? s-list-merge 3))
(check-true (set-contains? s-list-merge 4))

;; 测试空列表
(define s-list-empty (set 1 2))
(list->set! s-list-empty '())
(check (set-size s-list-empty) => 2)

;; 测试类型错误
(check-catch 'type-error (list->set! "not a set" '(1 2)))



#|
set-copy
复制一个 set。

语法
----
(set-copy set)

参数
----
set : set
要复制的 set。

返回值
-----
返回一个新的 set，包含原 set 的所有元素，且比较器相同。

异常
----
如果参数不是 set，抛出 error。
|#
(let ((copy (set-copy s-1-2)))
  (check-true (set=? s-1-2 copy))
  (check-false (eq? s-1-2 copy)) ; Ensure new instance
) ;let
(check-true (set-empty? (set-copy s-empty)))
(check-catch 'type-error (set-copy "not a set"))

#|
set-unfold
使用 unfold 模式创建 set。

语法
----
(set-unfold stop? mapper successor seed comparator)

参数
----
stop? : procedure
停止谓词。接收当前种子，返回布尔值。

mapper : procedure
映射函数。接收当前种子，返回要添加到 set 的元素。

successor : procedure
后继函数。接收当前种子，返回下一个种子。

seed : any
初始种子值。

comparator : comparator
元素比较器。

返回值
-----
返回生成的 set。
|#
;; Create set {0, 1, 2, ..., 9}
(define s-10 (set-unfold 
               (lambda (x) (= x 10)) 
               (lambda (x) x) 
               (lambda (x) (+ x 1)) 
               0 
               comp)
) ;define
(check-true (set-contains? s-10 0))
(check-true (set-contains? s-10 9))
(check-false (set-contains? s-10 10))

#|
set-contains?
检查 set 是否包含指定元素。

语法
----
(set-contains? set element)

参数
----
set : set
目标 set。

element : any
要检查的元素。

返回值
-----
如果 set 包含 element，返回 #t；否则返回 #f。

异常
----
如果参数不是 set，抛出 error。
|#
(check-true (set-contains? s-1 1))
(check-false (set-contains? s-1 2))
(check-false (set-contains? s-empty 1))
(check-catch 'type-error (set-contains? "not a set" 1))

#|
set-empty?
检查 set 是否为空。

语法
----
(set-empty? set)

参数
----
set : set
要检查的 set。

返回值
-----
如果 set 为空，返回 #t；否则返回 #f。

异常
----
如果参数不是 set，抛出 error。
|#
(check-true (set-empty? s-empty))
(check-false (set-empty? s-1))
(check-catch 'type-error (set-empty? "not a set"))

#|
set-disjoint?
检查两个 set 是否不相交（没有共同元素）。

语法
----
(set-disjoint? set1 set2)

参数
----
set1, set2 : set
要检查的 set。

返回值
-----
如果两个 set 没有共同元素，返回 #t；否则返回 #f。

异常
----
如果任一参数不是 set，抛出 error。
如果两个 set 的比较器不同，抛出 value-error。
|#
(check-true (set-disjoint? s-1-2-3 s-4-5))
(check-false (set-disjoint? s-1-2-3 s-2-3-4)) ; share 2, 3
(check-true (set-disjoint? s-empty s-1))
(check-true (set-disjoint? s-1 s-empty))
(check-true (set-disjoint? s-empty s-empty))
(check-catch 'type-error (set-disjoint? "not a set" s-1))
(check-catch 'type-error (set-disjoint? s-1 "not a set"))
;; Note: Comparator mismatch test is at the end of the file, but we should verify it here too or move it.
(define str-comp (make-comparator string? string=? string<? string-hash))
(define s-str (list->set-with-comparator str-comp '("apple" "banana")))
(check-catch 'value-error (set-disjoint? s-1 s-str))

#|
set=?
检查两个或多个 set 是否相等。

语法
----
(set=? set1 set2 ...)

参数
----
set1, set2, ... : set
要比较的 set。

返回值
-----
如果所有 set 都包含相同的元素，返回 #t；否则返回 #f。
注意：比较器必须相同。

异常
----
如果任一参数不是 set，抛出 error。
如果 set 的比较器不同，抛出 value-error。
|#
(check-true (set=? s-empty s-empty))
(check-true (set=? s-1 s-1))
(check-true (set=? s-1 (set 1)))
(check-false (set=? s-1 s-empty))
(check-false (set=? s-1 s-1-2))
;; Multiple arguments
(check-true (set=? s-1 (set 1) (list->set '(1))))
(check-false (set=? s-1 s-1 s-empty))
(check-catch 'type-error (set=? "not a set" s-1))
(check-catch 'value-error (set=? s-1 s-str))

#|
set<=?
检查一个 set 是否为另一个 set 的子集。

语法
----
(set<=? set1 set2 ...)

参数
----
set1, set2, ... : set
要检查的 set。

返回值
-----
如果每个 set 都是其后一个 set 的子集，返回 #t；否则返回 #f。

异常
----
如果任一参数不是 set，抛出 error。
如果 set 的比较器不同，抛出 value-error。
|#
(check-true (set<=? s-empty s-1))
(check-true (set<=? s-1 s-1-2))
(check-true (set<=? s-1-2 s-1-2-3))
(check-true (set<=? s-1-2 s-1-2))
(check-false (set<=? s-1-2 s-1))
;; Chain
(check-true (set<=? s-empty s-1 s-1-2 s-1-2-3))
(check-false (set<=? s-empty s-1-2 s-1)) ; Broken chain
(check-catch 'type-error (set<=? "not a set" s-1))
(check-catch 'value-error (set<=? s-1 s-str))

#|
set<?
检查一个 set 是否为另一个 set 的真子集。

语法
----
(set<? set1 set2 ...)

参数
----
set1, set2, ... : set
要检查的 set。

返回值
-----
如果每个 set 都是其后一个 set 的真子集，返回 #t；否则返回 #f。

异常
----
如果任一参数不是 set，抛出 error。
如果 set 的比较器不同，抛出 value-error。
|#
(check-true (set<? s-empty s-1))
(check-true (set<? s-1 s-1-2))
(check-false (set<? s-1 s-1))
(check-false (set<? s-1-2 s-1))
;; Chain
(check-true (set<? s-empty s-1 s-1-2))
(check-catch 'type-error (set<? "not a set" s-1))
(check-catch 'value-error (set<? s-1 s-str))

#|
set>=?
检查一个 set 是否为另一个 set 的超集。

语法
----
(set>=? set1 set2 ...)

参数
----
set1, set2, ... : set
要检查的 set。

返回值
-----
如果每个 set 都是其后一个 set 的超集，返回 #t；否则返回 #f。

异常
----
如果任一参数不是 set，抛出 error。
如果 set 的比较器不同，抛出 value-error。
|#
(check-true (set>=? s-1 s-empty))
(check-true (set>=? s-1-2 s-1))
(check-true (set>=? s-1 s-1))
(check-false (set>=? s-1 s-1-2))
;; Chain
(check-true (set>=? s-1-2-3 s-1-2 s-1 s-empty))
(check-catch 'type-error (set>=? "not a set" s-1))
(check-catch 'value-error (set>=? s-1 s-str))

#|
set>?
检查一个 set 是否为另一个 set 的真超集。

语法
----
(set>? set1 set2 ...)

参数
----
set1, set2, ... : set
要检查的 set。

返回值
-----
如果每个 set 都是其后一个 set 的真超集，返回 #t；否则返回 #f。

异常
----
如果任一参数不是 set，抛出 error。
如果 set 的比较器不同，抛出 value-error。
|#
(check-true (set>? s-1 s-empty))
(check-true (set>? s-1-2 s-1))
(check-false (set>? s-1 s-1))
(check-false (set>? s-1 s-1-2))
;; Chain
(check-true (set>? s-1-2 s-1 s-empty))
(check-catch 'type-error (set>? "not a set" s-1))
(check-catch 'value-error (set>? s-1 s-str))

;; --- Different Data Types ---
(define s-sym (set 'a 'b 'c))
(check-true (set-contains? s-sym 'a))
(check-false (set-contains? s-sym 'd))
(check-true (set=? s-sym (list->set '(c b a))))

;(define str-comp (make-comparator string? string=? string<? string-hash))
(define s-str (list->set '("apple" "banana")))
(check-true (set-contains? s-str "apple"))
(check-false (set-contains? s-str "pear"))

;; --- Large Set Test ---
(define (range n)
  (let loop ((i 0) (acc '()))
    (if (= i n) (reverse acc)
        (loop (+ i 1) (cons i acc))
    ) ;if
  ) ;let
) ;define

(define big-n 1000000)
(define big-list (range big-n))
(define s-big (list->set big-list))
;; Check basic existence
(check-true (set-contains? s-big 0))
(check-true (set-contains? s-big (- big-n 1)))
(check-false (set-contains? s-big big-n))
;; Check subset logic on large sets
(define s-big-minus-one (set-copy s-big))
;; (We can't easily remove elements with current API, so let's build a smaller one)
(define s-small-big (list->set (range (- big-n 1))))
(check-true (set<? s-small-big s-big))
(check-true (set=? s-big (list->set big-list)))

#|
set-size
获取 set 中元素的数量。

语法
----
(set-size set)

参数
----
set : set
要获取大小的 set。

返回值
-----
返回 set 中元素的数量（整数）。

异常
----
如果参数不是 set，抛出 error。
|#
(check (set-size s-empty) => 0)
(check (set-size s-1) => 1)
(check (set-size s-1-2) => 2)
(check (set-size s-1-2-3) => 3)
(check (set-size s-2-3-4) => 3)
(check (set-size s-4-5) => 2)
(check (set-size s-big) => big-n)
(check (set-size s-small-big) => (- big-n 1))
(check-catch 'type-error (set-size "not a set"))

#|
set-any?
检查 set 中是否有元素满足谓词。

语法
----
(set-any? predicate set)

参数
----
predicate : procedure
一个接受一个参数并返回布尔值的函数。

set : set
要检查的 set。

返回值
------
如果 set 中至少有一个元素满足 predicate，返回 #t；否则返回 #f。

注意
----
与 SRFI 1 的 any 函数不同，此函数不返回满足谓词的元素，只返回布尔值。

异常
----
如果 set 参数不是 set，抛出 error。
|#

;; 测试 set-any? 函数
(check-false (set-any? (lambda (x) (> x 10)) s-empty))
(check-false (set-any? (lambda (x) (> x 10)) s-1))
(check-false (set-any? (lambda (x) (> x 10)) s-1-2))
(check-false (set-any? (lambda (x) (> x 10)) s-1-2-3))

(check-true (set-any? (lambda (x) (= x 1)) s-1))
(check-true (set-any? (lambda (x) (= x 1)) s-1-2))
(check-true (set-any? (lambda (x) (= x 1)) s-1-2-3))
(check-true (set-any? (lambda (x) (= x 2)) s-1-2))
(check-true (set-any? (lambda (x) (= x 3)) s-1-2-3))

;; 测试多个元素满足谓词的情况
(check-true (set-any? (lambda (x) (> x 0)) s-1-2-3))
(check-true (set-any? (lambda (x) (< x 10)) s-1-2-3))

;; 测试边界情况
(check-true (set-any? (lambda (x) (even? x)) s-1-2))
(check-false (set-any? (lambda (x) (even? x)) s-1))
(check-true (set-any? (lambda (x) (odd? x)) s-1))
(check-true (set-any? (lambda (x) (odd? x)) s-1-2))

;; 测试类型错误
(check-catch 'type-error (set-any? (lambda (x) #t) "not a set"))

#|
set-every?
检查 set 中是否所有元素都满足谓词。

语法
----
(set-every? predicate set)

参数
----
predicate : procedure
一个接受一个参数并返回布尔值的函数。

set : set
要检查的 set。

返回值
------
如果 set 中所有元素都满足 predicate，返回 #t；否则返回 #f。

注意
----
与 SRFI 1 的 every 函数不同，此函数不返回满足谓词的元素，只返回布尔值。
空 set 返回 #t。

异常
----
如果 set 参数不是 set，抛出 error。
|#

;; 测试 set-every? 函数
(check-true (set-every? (lambda (x) (> x 0)) s-empty))
(check-true (set-every? (lambda (x) (> x 0)) s-1))
(check-true (set-every? (lambda (x) (> x 0)) s-1-2))
(check-true (set-every? (lambda (x) (> x 0)) s-1-2-3))

(check-false (set-every? (lambda (x) (> x 1)) s-1))
(check-false (set-every? (lambda (x) (> x 1)) s-1-2))
(check-false (set-every? (lambda (x) (> x 1)) s-1-2-3))

(check-true (set-every? (lambda (x) (number? x)) s-1-2-3))

;; 测试边界情况
(check-true (set-every? (lambda (x) (odd? x)) s-1))
(check-false (set-every? (lambda (x) (odd? x)) s-1-2)) ; 2 is even
(check-false (set-every? (lambda (x) (even? x)) s-1-2)) ; 1 is odd

;; 测试类型错误
(check-catch 'type-error (set-every? (lambda (x) #t) "not a set"))

#|
set-find
查找 set 中满足谓词的元素。

语法
----
(set-find predicate set failure)

参数
----
predicate : procedure
一个接受一个参数并返回布尔值的函数。

set : set
要检查的 set。

failure : procedure
一个无参函数，当没有找到满足谓词的元素时调用。

返回值
------
如果找到满足 predicate 的元素，返回该元素；否则返回 failure 的调用结果。

注意
----
如果有多个元素满足谓词，返回其中任意一个。

异常
----
如果 set 参数不是 set，抛出 error。
|#

;; 测试 set-find 函数
(check (set-find (lambda (x) (= x 1)) s-1 (lambda () 'not-found)) => 1)
(check (set-find (lambda (x) (= x 1)) s-1-2 (lambda () 'not-found)) => 1)
(check (set-find (lambda (x) (= x 2)) s-1-2 (lambda () 'not-found)) => 2)

(check (set-find (lambda (x) (> x 10)) s-1 (lambda () 'not-found)) => 'not-found)
(check (set-find (lambda (x) (> x 10)) s-empty (lambda () 'not-found)) => 'not-found)

;; 测试多个元素满足谓词的情况（返回任意一个）
(let ((res (set-find (lambda (x) (> x 0)) s-1-2 (lambda () 'not-found))))
  (check-true (or (= res 1) (= res 2)))
) ;let

;; 测试类型错误
(check-catch 'type-error (set-find (lambda (x) #t) "not a set" (lambda () #f)))

#|
set-count
计算 set 中满足谓词的元素个数。

语法
----
(set-count predicate set)

参数
----
predicate : procedure
一个接受一个参数并返回布尔值的函数。

set : set
要检查的 set。

返回值
------
返回满足 predicate 的元素个数（精确整数）。

异常
----
如果 set 参数不是 set，抛出 error。
|#

;; 测试 set-count 函数
(check (set-count (lambda (x) (> x 0)) s-empty) => 0)
(check (set-count (lambda (x) (> x 0)) s-1) => 1)
(check (set-count (lambda (x) (> x 0)) s-1-2) => 2)
(check (set-count (lambda (x) (> x 0)) s-1-2-3) => 3)

(check (set-count (lambda (x) (> x 1)) s-1) => 0)
(check (set-count (lambda (x) (> x 1)) s-1-2) => 1)
(check (set-count (lambda (x) (> x 1)) s-1-2-3) => 2)

(check (set-count (lambda (x) (even? x)) s-1-2-3) => 1) ; only 2
(check (set-count (lambda (x) (odd? x)) s-1-2-3) => 2)  ; 1 and 3

;; 测试类型错误
(check-catch 'type-error (set-count (lambda (x) #t) "not a set"))

#|
set-member
查找 set 中与指定元素相等的元素。

语法
----
(set-member set element default)

参数
----
set : set
要检查的 set。

element : any
要查找的元素。

default : any
如果 element 不在 set 中，返回的值。

返回值
------
如果 element 在 set 中，返回 set 中存储的那个元素（可能与 element 并不是同一个对象，但比较结果相等）。
如果 element 不在 set 中，返回 default。

异常
----
如果 set 参数不是 set，抛出 error。
|#

;; 测试 set-member 函数
(check (set-member s-1 1 'not-found) => 1)
(check (set-member s-1 2 'not-found) => 'not-found)
(check (set-member s-empty 1 'not-found) => 'not-found)

;; 测试通过比较器相等但对象不同的情况
;; 构造一个大小写不敏感的字符串集合
(define (my-string-ci-hash s)
  (string-hash (string-map char-downcase s))
) ;define
(define string-ci-comparator (make-comparator string? string-ci=? string-ci<? my-string-ci-hash))
(define s-str-ci (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))

(check (set-contains? s-str-ci "apple") => #t)
;; set-member 应该返回集合中存储的 "Apple"，而不是查询用的 "apple"
(check (set-member s-str-ci "apple" 'not-found) => "Apple")
(check (set-member s-str-ci "banana" 'not-found) => "Banana")
(check (set-member s-str-ci "pear" 'not-found) => 'not-found)

;; 测试类型错误
(check-catch 'type-error (set-member "not a set" 1 'default))

#|
set-search!
在 set 中搜索指定元素，并通过 continuation 决定更新方式（可变操作）。

语法
----
(set-search! set element failure success)

参数
----
set : set
目标 set。

element : any
要搜索的元素。

failure : procedure
当元素不存在时调用，接收两个 continuation：insert 与 ignore。

success : procedure
当元素存在时调用，接收 matching-element、update 与 remove。

返回值
------
返回两个值：可能更新后的 set 和 obj。

注意
----
continuation 的效果：
(insert obj) 插入 element。
(ignore obj) 不修改 set。
(update new-element obj) 用 new-element 替换匹配元素。
(remove obj) 移除匹配元素。
|#

;; 测试 set-search! 插入
(define s-search-1 (set 1 2))
(call-with-values
  (lambda ()
    (set-search! s-search-1 3
      (lambda (insert ignore)
        (insert 'payload)
      ) ;lambda
      (lambda (found update remove)
        (error "unexpected success")
      ) ;lambda
    ) ;set-search!
  ) ;lambda
  (lambda (result obj)
    (check-true (eq? result s-search-1))
    (check (set-size s-search-1) => 3)
    (check-true (set-contains? s-search-1 3))
    (check-false (set-contains? s-search-1 'payload))
    (check obj => 'payload)
  ) ;lambda
) ;call-with-values

;; 测试 set-search! 忽略
(define s-search-2 (set 1 2))
(call-with-values
  (lambda ()
    (set-search! s-search-2 3
      (lambda (insert ignore)
        (ignore 'ignored)
      ) ;lambda
      (lambda (found update remove)
        (error "unexpected success")
      ) ;lambda
    ) ;set-search!
  ) ;lambda
  (lambda (result obj)
    (check-true (eq? result s-search-2))
    (check (set-size s-search-2) => 2)
    (check-false (set-contains? s-search-2 3))
    (check obj => 'ignored)
  ) ;lambda
) ;call-with-values

;; 测试 set-search! 更新（equals 但 not eq?）
(define s-search-ci (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(call-with-values
  (lambda ()
    (set-search! s-search-ci "apple"
      (lambda (insert ignore)
        (error "unexpected failure")
      ) ;lambda
      (lambda (found update remove)
        (check found => "Apple")
        (update "apple" 'updated)
      ) ;lambda
    ) ;set-search!
  ) ;lambda
  (lambda (result obj)
    (check-true (eq? result s-search-ci))
    (check (set-size s-search-ci) => 2)
    (check (set-member s-search-ci "apple" 'not-found) => "apple")
    (check obj => 'updated)
  ) ;lambda
) ;call-with-values

;; 测试 set-search! 删除
(define s-search-3 (set 1 2 3))
(call-with-values
  (lambda ()
    (set-search! s-search-3 2
      (lambda (insert ignore)
        (error "unexpected failure")
      ) ;lambda
      (lambda (found update remove)
        (remove 'removed)
      ) ;lambda
    ) ;set-search!
  ) ;lambda
  (lambda (result obj)
    (check-true (eq? result s-search-3))
    (check (set-size s-search-3) => 2)
    (check-false (set-contains? s-search-3 2))
    (check obj => 'removed)
  ) ;lambda
) ;call-with-values

;; 测试类型错误
(check-catch 'type-error
  (set-search! "not a set" 1
    (lambda (insert ignore) (ignore 'x))
    (lambda (found update remove) (remove 'x))
  ) ;set-search!
) ;check-catch

#|
set-map
对 set 中的每个元素应用 proc，并返回一个新 set。

语法
----
(set-map comparator proc set)

参数
----
comparator : comparator
结果 set 的比较器。

proc : procedure
映射函数。

set : set
源 set。

返回值
------
返回新的 set，其中元素为 proc 的映射结果。

注意
----
如果映射后出现等价元素（基于 comparator），重复元素会被去重。
|#

;; 测试 set-map 基本映射
(define s-map-1 (set 1 2 3))
(define s-map-2 (set-map comp (lambda (x) (+ x 10)) s-map-1))
(check-true (set? s-map-2))
(check-true (eq? (set-element-comparator s-map-2) comp))
(check (set-size s-map-2) => 3)
(check-true (set-contains? s-map-2 11))
(check-true (set-contains? s-map-2 12))
(check-true (set-contains? s-map-2 13))
(check-true (set=? s-map-1 (set 1 2 3))) ; 原 set 不变

;; 测试 set-map 去重行为
(define s-map-dup (set 1 2 3 4 5))
(define s-map-dup-result (set-map comp (lambda (x) (quotient x 2)) s-map-dup))
(check (set-size s-map-dup-result) => 3)
(check-true (set-contains? s-map-dup-result 0))
(check-true (set-contains? s-map-dup-result 1))
(check-true (set-contains? s-map-dup-result 2))

;; 测试 set-map 使用不同 comparator
(define s-map-sym (list->set-with-comparator (make-eq-comparator) '(foo bar baz)))
(define s-map-str (set-map string-ci-comparator symbol->string s-map-sym))
(check (set-member s-map-str "FOO" 'not-found) => "foo")
(check (set-member s-map-str "BAR" 'not-found) => "bar")
(check (set-member s-map-str "BAZ" 'not-found) => "baz")

;; 测试类型错误
(check-catch 'type-error (set-map comp (lambda (x) x) "not a set"))

#|
set-for-each
对 set 中的每个元素应用 proc，忽略返回值。

语法
----
(set-for-each proc set)

参数
----
proc : procedure
要应用的函数。

set : set
目标 set。

返回值
------
返回值未指定。
|#

;; 测试 set-for-each 基本行为
(define s-foreach (set 1 2 3))
(define foreach-collected '())
(set-for-each (lambda (x) (set! foreach-collected (cons x foreach-collected))) s-foreach)
(check-true (set=? (list->set foreach-collected) s-foreach))

;; 测试空集合不触发调用
(define foreach-count 0)
(set-for-each (lambda (x) (set! foreach-count (+ foreach-count 1))) s-empty)
(check (set-size s-empty) => 0)
(check foreach-count => 0)

;; 测试类型错误
(check-catch 'type-error (set-for-each (lambda (x) x) "not a set"))

#|
set-fold
对 set 中的每个元素应用 proc，累积结果并返回。

语法
----
(set-fold proc nil set)

参数
----
proc : procedure
接收元素与累积值。

nil : any
初始累积值。

set : set
目标 set。

返回值
------
返回最后一次调用的结果，若 set 为空则返回 nil。
|#

;; 测试 set-fold 求和
(check (set-fold (lambda (x acc) (+ x acc)) 0 s-1-2-3) => 6)
(check (set-fold (lambda (x acc) (+ x acc)) 0 s-empty) => 0)

;; 测试 set-fold 累积为列表（顺序不保证）
(define fold-list (set-fold (lambda (x acc) (cons x acc)) '() s-1-2))
(check-true (set=? (list->set fold-list) s-1-2))

;; 测试类型错误
(check-catch 'type-error (set-fold (lambda (x acc) acc) '() "not a set"))

#|
set-filter
返回一个新的 set，仅包含满足 predicate 的元素。

语法
----
(set-filter predicate set)

参数
----
predicate : procedure
筛选条件。

set : set
源 set。

返回值
------
返回新的 set，比较器与原 set 相同。
|#

;; 测试 set-filter 基本筛选
(define s-filter-1 (set 1 2 3 4))
(define s-filter-2 (set-filter even? s-filter-1))
(check-true (set? s-filter-2))
(check-true (eq? (set-element-comparator s-filter-2) (set-element-comparator s-filter-1)))
(check (set-size s-filter-2) => 2)
(check-true (set-contains? s-filter-2 2))
(check-true (set-contains? s-filter-2 4))
(check-true (set-contains? s-filter-1 1)) ; 原 set 不变
(check-true (set-contains? s-filter-1 3))

;; 测试空集合
(define s-filter-empty (set-filter even? s-empty))
(check (set-size s-filter-empty) => 0)

;; 测试类型错误
(check-catch 'type-error (set-filter even? "not a set"))

#|
set-filter!
可变筛选，返回仅包含满足 predicate 的元素的 set。

语法
----
(set-filter! predicate set)

参数
----
predicate : procedure
筛选条件。

set : set
目标 set。

返回值
------
返回修改后的 set（与传入的 set 是同一个对象）。
|#

;; 测试 set-filter! 基本行为
(define s-filter-mut (set 1 2 3 4))
(define s-filter-mut-result (set-filter! odd? s-filter-mut))
(check-true (eq? s-filter-mut-result s-filter-mut))
(check (set-size s-filter-mut) => 2)
(check-true (set-contains? s-filter-mut 1))
(check-true (set-contains? s-filter-mut 3))
(check-false (set-contains? s-filter-mut 2))
(check-false (set-contains? s-filter-mut 4))

;; 测试空集合
(define s-filter-mut-empty (set-copy s-empty))
(set-filter! even? s-filter-mut-empty)
(check (set-size s-filter-mut-empty) => 0)

;; 测试类型错误
(check-catch 'type-error (set-filter! even? "not a set"))

#|
set-remove
返回一个新的 set，仅包含不满足 predicate 的元素。

语法
----
(set-remove predicate set)

参数
----
predicate : procedure
筛选条件。

set : set
源 set。

返回值
------
返回新的 set，比较器与原 set 相同。
|#

;; 测试 set-remove 基本筛选
(define s-remove-1 (set 1 2 3 4))
(define s-remove-2 (set-remove even? s-remove-1))
(check-true (set? s-remove-2))
(check-true (eq? (set-element-comparator s-remove-2) (set-element-comparator s-remove-1)))
(check (set-size s-remove-2) => 2)
(check-true (set-contains? s-remove-2 1))
(check-true (set-contains? s-remove-2 3))
(check-false (set-contains? s-remove-2 2))
(check-false (set-contains? s-remove-2 4))
(check-true (set-contains? s-remove-1 2)) ; 原 set 不变
(check-true (set-contains? s-remove-1 4))

;; 测试空集合
(define s-remove-empty (set-remove even? s-empty))
(check (set-size s-remove-empty) => 0)

;; 测试类型错误
(check-catch 'type-error (set-remove even? "not a set"))

#|
set-remove!
可变筛选，返回仅包含不满足 predicate 的元素的 set。

语法
----
(set-remove! predicate set)

参数
----
predicate : procedure
筛选条件。

set : set
目标 set。

返回值
------
返回修改后的 set（与传入的 set 是同一个对象）。
|#

;; 测试 set-remove! 基本行为
(define s-remove-mut (set 1 2 3 4))
(define s-remove-mut-result (set-remove! even? s-remove-mut))
(check-true (eq? s-remove-mut-result s-remove-mut))
(check (set-size s-remove-mut) => 2)
(check-true (set-contains? s-remove-mut 1))
(check-true (set-contains? s-remove-mut 3))
(check-false (set-contains? s-remove-mut 2))
(check-false (set-contains? s-remove-mut 4))

;; 测试空集合
(define s-remove-mut-empty (set-copy s-empty))
(set-remove! even? s-remove-mut-empty)
(check (set-size s-remove-mut-empty) => 0)

;; 测试类型错误
(check-catch 'type-error (set-remove! even? "not a set"))

#|
set-partition
将 set 划分为满足 predicate 与不满足 predicate 的两个新 set。

语法
----
(set-partition predicate set)

参数
----
predicate : procedure
划分条件。

set : set
源 set。

返回值
------
返回两个值：满足 predicate 的新 set 与不满足 predicate 的新 set。
|#

;; 测试 set-partition 基本行为
(define s-partition-1 (set 1 2 3 4))
(call-with-values
  (lambda () (set-partition even? s-partition-1))
  (lambda (yes no)
    (check-true (set? yes))
    (check-true (set? no))
    (check-true (eq? (set-element-comparator yes) (set-element-comparator s-partition-1)))
    (check-true (eq? (set-element-comparator no) (set-element-comparator s-partition-1)))
    (check (set-size yes) => 2)
    (check (set-size no) => 2)
    (check-true (set-contains? yes 2))
    (check-true (set-contains? yes 4))
    (check-true (set-contains? no 1))
    (check-true (set-contains? no 3))
    (check-true (set-contains? s-partition-1 2)) ; 原 set 不变
    (check-true (set-contains? s-partition-1 4))
  ) ;lambda
) ;call-with-values

;; 测试空集合
(call-with-values
  (lambda () (set-partition even? s-empty))
  (lambda (yes no)
    (check (set-size yes) => 0)
    (check (set-size no) => 0)
  ) ;lambda
) ;call-with-values

;; 测试类型错误
(check-catch 'type-error (set-partition even? "not a set"))

#|
set-partition!
可变划分，返回满足 predicate 的 set（原 set）与不满足 predicate 的新 set。

语法
----
(set-partition! predicate set)

参数
----
predicate : procedure
划分条件。

set : set
目标 set。

返回值
------
返回两个值：修改后的 set 与不满足 predicate 的新 set。
|#

;; 测试 set-partition! 基本行为
(define s-partition-mut (set 1 2 3 4))
(call-with-values
  (lambda () (set-partition! even? s-partition-mut))
  (lambda (yes no)
    (check-true (eq? yes s-partition-mut))
    (check (set-size yes) => 2)
    (check (set-size no) => 2)
    (check-true (set-contains? yes 2))
    (check-true (set-contains? yes 4))
    (check-true (set-contains? no 1))
    (check-true (set-contains? no 3))
  ) ;lambda
) ;call-with-values

;; 测试空集合
(define s-partition-empty (set-copy s-empty))
(call-with-values
  (lambda () (set-partition! even? s-partition-empty))
  (lambda (yes no)
    (check (set-size yes) => 0)
    (check (set-size no) => 0)
  ) ;lambda
) ;call-with-values

;; 测试类型错误
(check-catch 'type-error (set-partition! even? "not a set"))

#|
set-union
返回多个 set 的并集。

语法
----
(set-union set1 set2 ...)

参数
----
set1, set2 ... : set
参与并集的 set。

返回值
------
返回新的 set，元素来自它们首次出现的 set。
|#

;; 测试 set-union 基本行为
(define s-union-1 (set-union s-1-2-3 s-2-3-4))
(check (set-size s-union-1) => 4)
(check-true (set-contains? s-union-1 1))
(check-true (set-contains? s-union-1 2))
(check-true (set-contains? s-union-1 3))
(check-true (set-contains? s-union-1 4))
(check-true (eq? (set-element-comparator s-union-1) comp))

;; 测试多集合并集
(define s-union-2 (set-union s-1 s-2-3-4 s-4-5))
(check (set-size s-union-2) => 5)
(check-true (set-contains? s-union-2 1))
(check-true (set-contains? s-union-2 5))

;; 测试元素来源（使用大小写不敏感比较器）
(define s-union-ci-1 (list->set-with-comparator string-ci-comparator '("Apple")))
(define s-union-ci-2 (list->set-with-comparator string-ci-comparator '("apple" "Banana")))
(define s-union-ci (set-union s-union-ci-1 s-union-ci-2))
(check (set-member s-union-ci "apple" 'not-found) => "Apple")
(check (set-member s-union-ci "banana" 'not-found) => "Banana")

;; 测试类型与比较器错误
(check-catch 'type-error (set-union "not a set" s-1))
(check-catch 'value-error (set-union s-1 s-str-ci))

#|
set-intersection
返回多个 set 的交集。

语法
----
(set-intersection set1 set2 ...)

参数
----
set1, set2 ... : set
参与交集的 set。

返回值
------
返回新的 set，元素来自第一个 set。
|#

;; 测试 set-intersection 基本行为
(define s-inter-1 (set-intersection s-1-2-3 s-2-3-4))
(check (set-size s-inter-1) => 2)
(check-true (set-contains? s-inter-1 2))
(check-true (set-contains? s-inter-1 3))

;; 测试多集合交集
(define s-inter-2 (set-intersection s-1-2-3 s-2-3-4 (set 2 3)))
(check (set-size s-inter-2) => 2)
(check-true (set-contains? s-inter-2 2))
(check-true (set-contains? s-inter-2 3))

;; 测试元素来源（使用大小写不敏感比较器）
(define s-inter-ci-1 (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(define s-inter-ci-2 (list->set-with-comparator string-ci-comparator '("apple" "Pear")))
(define s-inter-ci (set-intersection s-inter-ci-1 s-inter-ci-2))
(check (set-member s-inter-ci "apple" 'not-found) => "Apple")
(check (set-size s-inter-ci) => 1)

;; 测试类型与比较器错误
(check-catch 'type-error (set-intersection "not a set" s-1))
(check-catch 'value-error (set-intersection s-1 s-str-ci))

#|
set-difference
返回第一个 set 与其余 set 的差集。

语法
----
(set-difference set1 set2 ...)

参数
----
set1, set2 ... : set
参与差集的 set。

返回值
------
返回新的 set，元素来自第一个 set。
|#

;; 测试 set-difference 基本行为
(define s-diff-1 (set-difference s-1-2-3 s-2-3-4))
(check (set-size s-diff-1) => 1)
(check-true (set-contains? s-diff-1 1))
(check-false (set-contains? s-diff-1 2))
(check-false (set-contains? s-diff-1 3))

;; 测试多集合差集
(define s-diff-2 (set-difference s-1-2-3 s-2-3-4 s-4-5))
(check (set-size s-diff-2) => 1)
(check-true (set-contains? s-diff-2 1))

;; 测试元素来源（使用大小写不敏感比较器）
(define s-diff-ci-1 (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(define s-diff-ci-2 (list->set-with-comparator string-ci-comparator '("apple")))
(define s-diff-ci (set-difference s-diff-ci-1 s-diff-ci-2))
(check (set-size s-diff-ci) => 1)
(check (set-member s-diff-ci "banana" 'not-found) => "Banana")

;; 测试类型与比较器错误
(check-catch 'type-error (set-difference "not a set" s-1))
(check-catch 'value-error (set-difference s-1 s-str-ci))

#|
set-xor
返回两个 set 的对称差集。

语法
----
(set-xor set1 set2)

参数
----
set1, set2 : set
参与对称差集的 set。

返回值
------
返回新的 set。
|#

;; 测试 set-xor 基本行为
(define s-xor-1 (set-xor s-1-2-3 s-2-3-4))
(check (set-size s-xor-1) => 2)
(check-true (set-contains? s-xor-1 1))
(check-true (set-contains? s-xor-1 4))

;; 测试元素来源（使用大小写不敏感比较器）
(define s-xor-ci (set-xor s-union-ci-1 s-union-ci-2))
(check (set-size s-xor-ci) => 1)
(check (set-member s-xor-ci "banana" 'not-found) => "Banana")
(check-true (set-contains? s-xor-ci "banana"))
(check-false (set-contains? s-xor-ci "apple"))

;; 测试类型与比较器错误
(check-catch 'type-error (set-xor "not a set" s-1))
(check-catch 'value-error (set-xor s-1 s-str-ci))

#|
set-union!
将多个 set 并入 set1（可变操作）。

语法
----
(set-union! set1 set2 ...)

参数
----
set1, set2 ... : set
参与并集的 set。

返回值
------
返回修改后的 set1，元素来自它们首次出现的 set。
|#

;; 测试 set-union! 基本行为
(define s-union!-1 (set 1 2 3))
(define s-union!-result (set-union! s-union!-1 s-2-3-4 s-4-5))
(check-true (eq? s-union!-result s-union!-1))
(check (set-size s-union!-1) => 5)
(check-true (set-contains? s-union!-1 1))
(check-true (set-contains? s-union!-1 5))

;; 测试元素来源（使用大小写不敏感比较器）
(define s-union!-ci-1 (list->set-with-comparator string-ci-comparator '("Apple")))
(define s-union!-ci-2 (list->set-with-comparator string-ci-comparator '("apple" "Banana")))
(define s-union!-ci (set-union! s-union!-ci-1 s-union!-ci-2))
(check-true (eq? s-union!-ci s-union!-ci-1))
(check (set-member s-union!-ci "apple" 'not-found) => "Apple")
(check (set-member s-union!-ci "banana" 'not-found) => "Banana")

;; 测试类型与比较器错误
(check-catch 'type-error (set-union! "not a set" s-1))
(check-catch 'value-error (set-union! s-1 s-str-ci))

#|
set-intersection!
就地更新 set1，使其成为多个 set 的交集。

语法
----
(set-intersection! set1 set2 ...)

参数
----
set1, set2 ... : set
参与交集的 set。

返回值
------
返回修改后的 set1，元素来自第一个 set。
|#

;; 测试 set-intersection! 基本行为
(define s-inter!-1 (set 1 2 3 4))
(define s-inter!-result (set-intersection! s-inter!-1 s-2-3-4))
(check-true (eq? s-inter!-result s-inter!-1))
(check (set-size s-inter!-1) => 3)
(check-true (set-contains? s-inter!-1 2))
(check-true (set-contains? s-inter!-1 3))
(check-true (set-contains? s-inter!-1 4))

;; 测试多集合交集
(define s-inter!-2 (set 1 2 3 4))
(set-intersection! s-inter!-2 s-2-3-4 (set 2 3))
(check (set-size s-inter!-2) => 2)
(check-true (set-contains? s-inter!-2 2))
(check-true (set-contains? s-inter!-2 3))

;; 测试元素来源（使用大小写不敏感比较器）
(define s-inter!-ci-1 (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(define s-inter!-ci-2 (list->set-with-comparator string-ci-comparator '("apple" "Pear")))
(set-intersection! s-inter!-ci-1 s-inter!-ci-2)
(check (set-member s-inter!-ci-1 "apple" 'not-found) => "Apple")
(check (set-size s-inter!-ci-1) => 1)

;; 测试类型与比较器错误
(check-catch 'type-error (set-intersection! "not a set" s-1))
(check-catch 'value-error (set-intersection! s-1 s-str-ci))

#|
set-difference!
就地更新 set1，使其成为与其余 set 的差集。

语法
----
(set-difference! set1 set2 ...)

参数
----
set1, set2 ... : set
参与差集的 set。

返回值
------
返回修改后的 set1，元素来自第一个 set。
|#

;; 测试 set-difference! 基本行为
(define s-diff!-1 (set 1 2 3 4))
(define s-diff!-result (set-difference! s-diff!-1 s-2-3-4))
(check-true (eq? s-diff!-result s-diff!-1))
(check (set-size s-diff!-1) => 1)
(check-true (set-contains? s-diff!-1 1))
(check-false (set-contains? s-diff!-1 2))

;; 测试多集合差集
(define s-diff!-2 (set 1 2 3 4 5))
(set-difference! s-diff!-2 s-2-3-4 s-4-5)
(check (set-size s-diff!-2) => 1)
(check-true (set-contains? s-diff!-2 1))

;; 测试元素来源（使用大小写不敏感比较器）
(define s-diff!-ci-1 (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(define s-diff!-ci-2 (list->set-with-comparator string-ci-comparator '("apple")))
(set-difference! s-diff!-ci-1 s-diff!-ci-2)
(check (set-size s-diff!-ci-1) => 1)
(check (set-member s-diff!-ci-1 "banana" 'not-found) => "Banana")

;; 测试类型与比较器错误
(check-catch 'type-error (set-difference! "not a set" s-1))
(check-catch 'value-error (set-difference! s-1 s-str-ci))

#|
set-xor!
就地更新 set1，使其成为与 set2 的对称差集。

语法
----
(set-xor! set1 set2)

参数
----
set1, set2 : set
参与对称差集的 set。

返回值
------
返回修改后的 set1。
|#

;; 测试 set-xor! 基本行为
(define s-xor!-1 (set 1 2 3))
(define s-xor!-result (set-xor! s-xor!-1 s-2-3-4))
(check-true (eq? s-xor!-result s-xor!-1))
(check (set-size s-xor!-1) => 2)
(check-true (set-contains? s-xor!-1 1))
(check-true (set-contains? s-xor!-1 4))

;; 测试元素来源（使用大小写不敏感比较器）
(define s-xor!-ci-1 (list->set-with-comparator string-ci-comparator '("Apple")))
(define s-xor!-ci-2 (list->set-with-comparator string-ci-comparator '("apple" "Banana")))
(set-xor! s-xor!-ci-1 s-xor!-ci-2)
(check (set-size s-xor!-ci-1) => 1)
(check (set-member s-xor!-ci-1 "banana" 'not-found) => "Banana")
(check-false (set-contains? s-xor!-ci-1 "apple"))

;; 测试类型与比较器错误
(check-catch 'type-error (set-xor! "not a set" s-1))
(check-catch 'value-error (set-xor! s-1 s-str-ci))

#|
set->list
将 set 转换为列表（顺序未指定）。

语法
----
(set->list set)

参数
----
set : set
源 set。

返回值
------
返回包含 set 元素的新列表。
|#

;; 测试 set->list 基本行为
(define s-to-list (set 1 2 3))
(define l-to-list (set->list s-to-list))
(check (length l-to-list) => 3)
(check-true (set=? (list->set l-to-list) s-to-list))

;; 测试类型错误
(check-catch 'type-error (set->list "not a set"))


#|
set-adjoin
返回一个新的 set，包含原 set 的所有元素以及新增的元素。

语法
----
(set-adjoin set element ...)

参数
----
set : set
初始 set。

element ... : any
要添加的元素。

返回值
------
返回一个新的 set。

注意
----
此函数不修改原 set。
|#

;; 测试 set-adjoin 函数
(define s-adjoin-1 (set-adjoin s-empty 1))
(check (set-size s-adjoin-1) => 1)
(check-true (set-contains? s-adjoin-1 1))
(check-true (set-empty? s-empty)) ; 原 set 不变

(define s-adjoin-2 (set-adjoin s-1 2 3))
(check (set-size s-adjoin-2) => 3)
(check-true (set-contains? s-adjoin-2 1))
(check-true (set-contains? s-adjoin-2 2))
(check-true (set-contains? s-adjoin-2 3))

;; 测试添加已存在的元素
(define s-adjoin-3 (set-adjoin s-1 1))
(check (set-size s-adjoin-3) => 1)
(check-true (set-contains? s-adjoin-3 1))

;; 测试类型错误
(check-catch 'type-error (set-adjoin "not a set" 1))

#|
set-adjoin!
向 set 中添加一个或多个元素（可变操作）。

语法
----
(set-adjoin! set element ...)

参数
----
set : set
目标 set。

element ... : any
要添加的元素。

返回值
------
返回修改后的 set（与传入的 set 是同一个对象）。

注意
----
此函数会修改原 set。
|#

;; 测试 set-adjoin! 函数
(define s-mut (set-copy s-empty))
(set-adjoin! s-mut 1)
(check (set-size s-mut) => 1)
(check-true (set-contains? s-mut 1))

(set-adjoin! s-mut 2 3)
(check (set-size s-mut) => 3)
(check-true (set-contains? s-mut 1))
(check-true (set-contains? s-mut 2))
(check-true (set-contains? s-mut 3))

;; 测试添加已存在的元素
(set-adjoin! s-mut 1)
(check (set-size s-mut) => 3)

;; 测试类型错误
(check-catch 'type-error (set-adjoin! "not a set" 1))

#|
set-replace
返回一个新的 set，其中指定的元素被替换。

语法
----
(set-replace set element)

参数
----
set : set
初始 set。

element : any
用来替换的元素。

返回值
------
返回一个新的 set。
如果 set 中包含与 element 相等的元素（根据比较器），则该元素被 element 替换（对于 equals 但 not eq? 的情况很有用）。
如果 set 中不包含 element，则返回原 set（不做任何修改）。

注意
----
此函数不修改原 set。
|#

;; 测试 set-replace 函数
(define s-replace-1 (set-replace s-1 1))
(check (set-size s-replace-1) => 1)
(check-true (set-contains? s-replace-1 1))
(check-true (set=? s-replace-1 s-1)) ; 内容相同
(check-false (eq? s-replace-1 s-1)) ; 但应该是新分配的 set (因为确实发生了替换逻辑)

(define s-replace-2 (set-replace s-1 2))
(check-true (eq? s-replace-2 s-1)) ; 不包含 2，返回原 set

;; 测试替换 equals 但 not eq? 的元素
(define s-str-ci-2 (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(check (set-member s-str-ci-2 "apple" 'not-found) => "Apple")

(define s-replace-3 (set-replace s-str-ci-2 "apple"))
(check (set-member s-replace-3 "apple" 'not-found) => "apple") ; 应该被替换为 "apple"
(check-false (eq? s-replace-3 s-str-ci-2)) ; 应该是新 set

;; 测试类型错误
(check-catch 'type-error (set-replace "not a set" 1))

#|
set-replace!
修改 set，将其中指定的元素替换（可变操作）。

语法
----
(set-replace! set element)

参数
----
set : set
目标 set。

element : any
用来替换的元素。

返回值
------
返回修改后的 set（与传入的 set 是同一个对象）。

注意
----
此函数会修改原 set。
|#

;; 测试 set-replace! 函数
(define s-mut-replace (set-copy s-1))
(set-replace! s-mut-replace 1)
(check (set-size s-mut-replace) => 1)
(check-true (set-contains? s-mut-replace 1))

;; 测试替换 equals 但 not eq? 的元素
(define s-str-ci-mut (list->set-with-comparator string-ci-comparator '("Apple" "Banana")))
(check (set-member s-str-ci-mut "apple" 'not-found) => "Apple")

(set-replace! s-str-ci-mut "apple")
(check (set-member s-str-ci-mut "apple" 'not-found) => "apple") ; 应该被替换为 "apple"

;; 测试不存在的元素
(set-replace! s-str-ci-mut "Pear")
(check (set-size s-str-ci-mut) => 2) ; 不变

;; 测试类型错误
(check-catch 'type-error (set-replace! "not a set" 1))

#|
set-delete
返回一个新的 set，其中指定的元素被移除。

语法
----
(set-delete set element ...)

参数
----
set : set
初始 set。

element ... : any
要移除的元素。

返回值
------
返回一个新的 set。
如果元素不存在，则忽略。

注意
----
此函数不修改原 set。
|#

;; 测试 set-delete 函数
(define s-del-1 (set-delete s-1-2-3 1))
(check (set-size s-del-1) => 2)
(check-false (set-contains? s-del-1 1))
(check-true (set-contains? s-del-1 2))
(check-true (set-contains? s-del-1 3))

(define s-del-2 (set-delete s-1-2-3 4)) ; 移除不存在的元素
(check (set-size s-del-2) => 3)
(check-true (set=? s-del-2 s-1-2-3))

(define s-del-3 (set-delete s-1-2-3 1 2))
(check (set-size s-del-3) => 1)
(check-false (set-contains? s-del-3 1))
(check-false (set-contains? s-del-3 2))
(check-true (set-contains? s-del-3 3))

#|
set-delete!
从 set 中移除指定的元素（可变操作）。

语法
----
(set-delete! set element ...)

参数
----
set : set
目标 set。

element ... : any
要移除的元素。

返回值
------
返回修改后的 set（与传入的 set 是同一个对象）。

注意
----
此函数会修改原 set。
|#

;; 测试 set-delete! 函数
(define s-mut-del (set-copy s-1-2-3))
(set-delete! s-mut-del 1)
(check (set-size s-mut-del) => 2)
(check-false (set-contains? s-mut-del 1))

(set-delete! s-mut-del 2 3)
(check (set-size s-mut-del) => 0)
(check-true (set-empty? s-mut-del))

#|
set-delete-all
返回一个新的 set，其中指定列表中的元素被移除。

语法
----
(set-delete-all set element-list)

参数
----
set : set
初始 set。

element-list : list
要移除的元素列表。

返回值
------
返回一个新的 set。
|#

;; 测试 set-delete-all 函数
(define s-del-all (set-delete-all s-1-2-3 '(1 2)))
(check (set-size s-del-all) => 1)
(check-false (set-contains? s-del-all 1))
(check-false (set-contains? s-del-all 2))
(check-true (set-contains? s-del-all 3))

#|
set-delete-all!
从 set 中移除指定列表中的元素（可变操作）。

语法
----
(set-delete-all! set element-list)

参数
----
set : set
目标 set。

element-list : list
要移除的元素列表。

返回值
------
返回修改后的 set。
|#

;; 测试 set-delete-all! 函数
(define s-mut-del-all (set-copy s-1-2-3))
(set-delete-all! s-mut-del-all '(1 2))
(check (set-size s-mut-del-all) => 1)
(check-false (set-contains? s-mut-del-all 1))
(check-false (set-contains? s-mut-del-all 2))
(check-true (set-contains? s-mut-del-all 3))

(check-report)

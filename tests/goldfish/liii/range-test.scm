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
        (srfi srfi-196))

(check-set-mode! 'report-failed)

;; range
;; 使用指定的长度和索引器函数创建 range。
;;
;; 语法
;; ----
;; (range length indexer)
;;
;; 参数
;; ----
;; length : exact-natural
;; range 的长度，必须是非负整数。
;; indexer : procedure
;; 接受一个索引参数并返回对应位置的值的函数。
;;
;; 返回值
;; -----
;; 新创建的 range 对象。
;;
;; 说明
;; ----
;; 创建一个惰性序列，元素通过索引器函数按需计算。

(let ((r (range 10 (lambda (i) (* i 2)))))
  (check (range? r) => #t)
  (check (range-length r) => 10)
  (check (range-ref r 0) => 0)
  (check (range-ref r 5) => 10)
  (check (range-ref r 9) => 18)
) ;let

;; numeric-range
;; 创建数值范围的 range。
;;
;; 语法
;; ----
;; (numeric-range start end)
;; (numeric-range start end step)
;;
;; 参数
;; ----
;; start : real
;; 范围的起始值。
;; end : real
;; 范围的结束值（不包含）。
;; step : real (可选)
;; 步长，默认为 1。
;;
;; 返回值
;; -----
;; 包含数值序列的 range 对象。
;;
;; 说明
;; ----
;; 创建等差数列的 range。当 step 为负数时，可以创建递减序列。

(let ((r (numeric-range 0 10)))
  (check (range? r) => #t)
  (check (range-length r) => 10)
  (check (range-ref r 0) => 0)
  (check (range-ref r 9) => 9)
) ;let

(let ((r (numeric-range 10 30 2)))
  (check (range-length r) => 10)
  (check (range-ref r 0) => 10)
  (check (range-ref r 1) => 12)
  (check (range-ref r 9) => 28)
) ;let

(let ((r (numeric-range 5 0 -1)))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 5)
  (check (range-ref r 4) => 1)
) ;let

;; iota-range
;; 创建 iota 序列的 range（类似于 iota 函数）。
;;
;; 语法
;; ----
;; (iota-range len)
;; (iota-range len start)
;; (iota-range len start step)
;;
;; 参数
;; ----
;; len : exact-natural
;; 序列长度。
;; start : real (可选)
;; 起始值，默认为 0。
;; step : real (可选)
;; 步长，默认为 1。
;;
;; 返回值
;; -----
;; 包含 iota 序列的 range 对象。
;;
;; 说明
;; ----
;; 创建等差数列，类似于 (start, start+step, start+2*step, ...)。

(let ((r (iota-range 10)))
  (check (range-length r) => 10)
  (check (range-ref r 0) => 0)
  (check (range-ref r 9) => 9)
) ;let

(let ((r (iota-range 5 10)))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 10)
  (check (range-ref r 4) => 14)
) ;let

(let ((r (iota-range 5 0 2)))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 0)
  (check (range-ref r 1) => 2)
  (check (range-ref r 4) => 8)
) ;let

;; vector-range
;; 从向量创建 range。
;;
;; 语法
;; ----
;; (vector-range vec)
;;
;; 参数
;; ----
;; vec : vector
;; 源向量。
;;
;; 返回值
;; -----
;; 包含向量元素的 range 对象。
;;
;; 说明
;; ----
;; 创建的 range 与源向量共享元素。

(let ((r (vector-range #(a b c d e))))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 'a)
  (check (range-ref r 4) => 'e)
) ;let

;; string-range
;; 从字符串创建 range。
;;
;; 语法
;; ----
;; (string-range s)
;;
;; 参数
;; ----
;; s : string
;; 源字符串。
;;
;; 返回值
;; -----
;; 包含字符串字符的 range 对象。
;;
;; 说明
;; ----
;; 将字符串转换为字符向量后再创建 range。

(let ((r (string-range "hello")))
  (check (range-length r) => 5)
  (check (range-ref r 0) => #\h)
  (check (range-ref r 4) => #\o)
) ;let

;; range?
;; 判断值是否为 range 类型。
;;
;; 语法
;; ----
;; (range? x)
;;
;; 参数
;; ----
;; x : any
;; 要判断的值。
;;
;; 返回值
;; -----
;; 布尔值：#t 如果是 range，#f 否则。

;; range-length
;; 获取 range 的长度。
;;
;; 语法
;; ----
;; (range-length r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; range 的长度，非负整数。

;; range-ref
;; 获取 range 中指定索引位置的元素。
;;
;; 语法
;; ----
;; (range-ref r index)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;; index : exact-natural
;; 索引位置，从 0 开始。
;;
;; 返回值
;; -----
;; 索引位置的元素。

;; range-first
;; 获取 range 的第一个元素。
;;
;; 语法
;; ----
;; (range-first r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 第一个元素。
;;
;; 说明
;; ----
;; 等价于 (range-ref r 0)。

;; range-last
;; 获取 range 的最后一个元素。
;;
;; 语法
;; ----
;; (range-last r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 最后一个元素。
;;
;; 说明
;; ----
;; 等价于 (range-ref r (- (range-length r) 1))。

(let ((r (numeric-range 10 20)))
  (check (range-first r) => 10)
  (check (range-last r) => 19)
) ;let

;; range=?
;; 比较多个 range 是否相等。
;;
;; 语法
;; ----
;; (range=? equal r1 r2)
;; (range=? equal r1 r2 ...)
;;
;; 参数
;; ----
;; equal : procedure
;; 比较两个元素是否相等的函数。
;; r1, r2, ... : range
;; 要比较的 range 对象。
;;
;; 返回值
;; -----
;; 布尔值：#t 如果所有 range 长度相同且对应元素相等，#f 否则。

(let ((r1 (numeric-range 0 5))
      (r2 (numeric-range 0 5))
      (r3 (numeric-range 1 6)))
  (check (range=? = r1 r2) => #t)
  (check (range=? = r1 r3) => #f)
) ;let

;; subrange
;; 提取 range 的子范围。
;;
;; 语法
;; ----
;; (subrange r start end)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;; start : exact-natural
;; 起始索引（包含）。
;; end : exact-natural
;; 结束索引（不包含）。
;;
;; 返回值
;; -----
;; 新的 range 对象，包含从 start 到 end-1 的元素。

(let ((r (numeric-range 0 10)))
  (let ((s (subrange r 2 7)))
    (check (range-length s) => 5)
    (check (range-ref s 0) => 2)
    (check (range-ref s 4) => 6)
  ) ;let
) ;let

;; range-take
;; 从 range 开头提取指定数量的元素。
;;
;; 语法
;; ----
;; (range-take r count)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;; count : exact-natural
;; 要提取的元素数量。
;;
;; 返回值
;; -----
;; 新的 range 对象，包含前 count 个元素。

;; range-drop
;; 从 range 开头删除指定数量的元素。
;;
;; 语法
;; ----
;; (range-drop r count)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;; count : exact-natural
;; 要删除的元素数量。
;;
;; 返回值
;; -----
;; 新的 range 对象，不包含前 count 个元素。

(let ((r (numeric-range 0 10)))
  (let ((taken (range-take r 5)))
    (check (range-length taken) => 5)
    (check (range-ref taken 0) => 0)
    (check (range-ref taken 4) => 4)
  ) ;let
  (let ((dropped (range-drop r 5)))
    (check (range-length dropped) => 5)
    (check (range-ref dropped 0) => 5)
    (check (range-ref dropped 4) => 9)
  ) ;let
) ;let

;; range-take-right
;; 从 range 末尾提取指定数量的元素。
;;
;; 语法
;; ----
;; (range-take-right r count)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;; count : exact-natural
;; 要提取的元素数量。
;;
;; 返回值
;; -----
;; 新的 range 对象，包含后 count 个元素。

;; range-drop-right
;; 从 range 末尾删除指定数量的元素。
;;
;; 语法
;; ----
;; (range-drop-right r count)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;; count : exact-natural
;; 要删除的元素数量。
;;
;; 返回值
;; -----
;; 新的 range 对象，不包含后 count 个元素。

(let ((r (numeric-range 0 10)))
  (let ((taken (range-take-right r 3)))
    (check (range-length taken) => 3)
    (check (range-ref taken 0) => 7)
    (check (range-ref taken 2) => 9)
  ) ;let
  (let ((dropped (range-drop-right r 3)))
    (check (range-length dropped) => 7)
    (check (range-ref dropped 6) => 6)
  ) ;let
) ;let

;; range-split-at
;; 在指定位置将 range 分割为两部分。
;;
;; 语法
;; ----
;; (range-split-at r index)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;; index : exact-natural
;; 分割位置。
;;
;; 返回值
;; -----
;; 两个值：前半部分和后半部分的 range。

(let ((r (numeric-range 0 10)))
  (let-values (((left right) (range-split-at r 4)))
    (check (range-length left) => 4)
    (check (range-length right) => 6)
    (check (range-ref left 3) => 3)
    (check (range-ref right 0) => 4)
  ) ;let-values
) ;let

;; range-segment
;; 将 range 分割为固定大小的段。
;;
;; 语法
;; ----
;; (range-segment r k)
;;
;; 参数
;; ----
;; r : range
;; 源 range 对象。
;; k : exact-positive-integer
;; 每段的大小。
;;
;; 返回值
;; -----
;; range 列表，每个 range 最多包含 k 个元素。

(let ((r (numeric-range 0 10)))
  (let ((segs (range-segment r 3)))
    (check (length segs) => 4)
    (check (range-length (car segs)) => 3)
    (check (range-length (cadddr segs)) => 1)
  ) ;let
) ;let

;; range-count
;; 统计满足谓词的元素数量。
;;
;; 语法
;; ----
;; (range-count pred r)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接受一个元素返回布尔值。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 满足谓词的元素数量。

(let ((r (numeric-range 0 10)))
  (check (range-count even? r) => 5)
  (check (range-count odd? r) => 5)
  (check (range-count (lambda (x) (> x 5)) r) => 4)
) ;let

;; range-map->list
;; 将映射函数应用于 range 的每个元素，结果收集为列表。
;;
;; 语法
;; ----
;; (range-map->list proc r)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射函数，接受一个元素。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 列表，包含映射后的结果。

(let ((r (numeric-range 0 5)))
  (check (range-map->list (lambda (x) (* x 2)) r) => '(0 2 4 6 8))
  (check (range-map->list (lambda (x) (* x x)) r) => '(0 1 4 9 16))
) ;let

;; range-for-each
;; 对 range 的每个元素执行副作用操作。
;;
;; 语法
;; ----
;; (range-for-each proc r)
;;
;; 参数
;; ----
;; proc : procedure
;; 副作用函数，接受一个元素。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 无（未定义）。

(let ((r (numeric-range 0 5))
      (result '()))
  (range-for-each (lambda (x) (set! result (cons x result))) r)
  (check result => '(4 3 2 1 0))
) ;let

;; range-fold
;; 对 range 进行左折叠。
;;
;; 语法
;; ----
;; (range-fold proc nil r)
;;
;; 参数
;; ----
;; proc : procedure
;; 折叠函数，接受累加值和当前元素。
;; nil : any
;; 初始累加值。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 最终的累加值。

;; range-fold-right
;; 对 range 进行右折叠。
;;
;; 语法
;; ----
;; (range-fold-right proc nil r)
;;
;; 参数
;; ----
;; proc : procedure
;; 折叠函数，接受当前元素和累加值。
;; nil : any
;; 初始累加值。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 最终的累加值。

(let ((r (numeric-range 1 6)))
  (check (range-fold + 0 r) => 15)
  (check (range-fold * 1 r) => 120)
  (check (range-fold-right cons '() r) => '(1 2 3 4 5))
) ;let

;; range-any
;; 检查是否存在满足谓词的元素。
;;
;; 语法
;; ----
;; (range-any pred r)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 布尔值：#t 如果存在满足谓词的元素，#f 否则。

;; range-every
;; 检查是否所有元素都满足谓词。
;;
;; 语法
;; ----
;; (range-every pred r)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 布尔值：#t 如果所有元素都满足谓词，#f 否则。

(let ((r (numeric-range 0 10)))
  (check (range-any even? r) => #t)
  (check (range-any (lambda (x) (> x 8)) r) => #t)
  (check (range-any (lambda (x) (> x 10)) r) => #f)
  (check (range-every integer? r) => #t)
  (check (range-every even? r) => #f)
  (check (range-every (lambda (x) (< x 10)) r) => #t)
) ;let

;; range-filter->list
;; 过滤 range 中满足谓词的元素，结果为列表。
;;
;; 语法
;; ----
;; (range-filter->list pred r)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 列表，包含所有满足谓词的元素。

;; range-remove->list
;; 过滤 range 中不满足谓词的元素，结果为列表。
;;
;; 语法
;; ----
;; (range-remove->list pred r)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 列表，包含所有不满足谓词的元素。

(let ((r (numeric-range 0 10)))
  (check (range-filter->list even? r) => '(0 2 4 6 8))
  (check (range-filter->list odd? r) => '(1 3 5 7 9))
  (check (range-remove->list even? r) => '(1 3 5 7 9))
  (check (range-remove->list odd? r) => '(0 2 4 6 8))
) ;let

;; range-reverse
;; 反转 range 的元素顺序。
;;
;; 语法
;; ----
;; (range-reverse r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 新的 range 对象，元素顺序反转。

(let ((r (numeric-range 0 5)))
  (let ((rev (range-reverse r)))
    (check (range->list rev) => '(4 3 2 1 0))
  ) ;let
) ;let

;; range-append
;; 连接多个 range。
;;
;; 语法
;; ----
;; (range-append)
;; (range-append r)
;; (range-append r1 r2)
;; (range-append r1 r2 ...)
;;
;; 参数
;; ----
;; r, r1, r2, ... : range
;; 要连接的 range 对象。
;;
;; 返回值
;; -----
;; 新的 range 对象，包含所有输入 range 的元素。

(let ((r1 (numeric-range 0 3))
      (r2 (numeric-range 3 6)))
  (let ((appended (range-append r1 r2)))
    (check (range-length appended) => 6)
    (check (range->list appended) => '(0 1 2 3 4 5))
  ) ;let
) ;let

(let ((r1 (numeric-range 0 2))
      (r2 (numeric-range 2 4))
      (r3 (numeric-range 4 6)))
  (let ((appended (range-append r1 r2 r3)))
    (check (range-length appended) => 6)
    (check (range->list appended) => '(0 1 2 3 4 5))
  ) ;let
) ;let

;; range->list
;; 将 range 转换为列表。
;;
;; 语法
;; ----
;; (range->list r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 包含 range 所有元素的列表。

(let ((r (numeric-range 0 5)))
  (check (range->list r) => '(0 1 2 3 4))
) ;let

;; range->vector
;; 将 range 转换为向量。
;;
;; 语法
;; ----
;; (range->vector r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 包含 range 所有元素的向量。

(let ((r (numeric-range 0 5)))
  (check (range->vector r) => #(0 1 2 3 4))
) ;let

;; range->string
;; 将字符 range 转换为字符串。
;;
;; 语法
;; ----
;; (range->string r)
;;
;; 参数
;; ----
;; r : range
;; 字符 range 对象。
;;
;; 返回值
;; -----
;; 包含 range 所有字符的字符串。

(let ((r (string-range "hello")))
  (check (range->string r) => "hello")
) ;let

;; vector->range
;; 将向量转换为 range（创建副本）。
;;
;; 语法
;; ----
;; (vector->range vec)
;;
;; 参数
;; ----
;; vec : vector
;; 源向量。
;;
;; 返回值
;; -----
;; 包含向量元素副本的 range 对象。

(let ((r (vector->range #(1 2 3 4 5))))
  (check (range-length r) => 5)
  (check (range->list r) => '(1 2 3 4 5))
) ;let

;; range->generator
;; 将 range 转换为生成器函数。
;;
;; 语法
;; ----
;; (range->generator r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 生成器函数，每次调用返回下一个元素，结束时返回 eof-object。

(let ((r (numeric-range 0 5))
      (result '()))
  (let ((g (range->generator r)))
    (let loop ((v (g)))
      (if (eof-object? v)
          (check result => '(0 1 2 3 4))
          (begin
            (set! result (append result (list v)))
            (loop (g)))))))

;; range-map->vector
;; 将映射函数应用于 range 的每个元素，结果收集为向量。
;;
;; 语法
;; ----
;; (range-map->vector proc r)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射函数。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 向量，包含映射后的结果。

(let ((r (numeric-range 0 5)))
  (check (range-map->vector (lambda (x) (* x 2)) r) => #(0 2 4 6 8)))

;; range-filter->vector
;; 过滤 range 中满足谓词的元素，结果为向量。
;;
;; 语法
;; ----
;; (range-filter->vector pred r)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 向量，包含所有满足谓词的元素。

;; range-remove->vector
;; 过滤 range 中不满足谓词的元素，结果为向量。
;;
;; 语法
;; ----
;; (range-remove->vector pred r)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数。
;; r : range
;; range 对象。
;;
;; 返回值
;; -----
;; 向量，包含所有不满足谓词的元素。

(let ((r (numeric-range 0 10)))
  (check (range-filter->vector even? r) => #(0 2 4 6 8))
  (check (range-remove->vector even? r) => #(1 3 5 7 9)))

(check-report)

;
; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;
; Permission is hereby granted, free of charge, to any person obtaining a
; copy of this software and associated documentation files (the
; "Software"), to deal in the Software without restriction, including
; without limitation the rights to use, copy, modify, merge, publish,
; distribute, sublicense, and/or sell copies of the Software, and to
; permit persons to whom the Software is furnished to do so, subject to
; the following conditions:
;
; The above copyright notice and this permission notice shall be included
; in all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
        (liii check)
        (liii iset)
        (only (srfi srfi-1) every any count)
) ;import

(check-set-mode! 'report-failed)

;;;; Utility

(define (init xs)
  (if (null? (cdr xs))
      '()
      (cons (car xs) (init (cdr xs)))
  ) ;if
) ;define

(define (constantly x)
  (lambda (_) x)
) ;define

(define pos-seq (iota 20 100 3))
(define neg-seq (iota 20 -100 3))
(define mixed-seq (iota 20 -10 3))
(define sparse-seq (iota 20 -10000 1003))

(define pos-set (list->iset pos-seq))
(define pos-set+ (iset-adjoin pos-set 9))
(define neg-set (list->iset neg-seq))
(define mixed-set (list->iset mixed-seq))
(define dense-set (make-range-iset 0 49))
(define sparse-set (list->iset sparse-seq))

(define all-test-sets
  (list pos-set neg-set mixed-set dense-set sparse-set)
) ;define

;;
;; iset?
;; 检查对象是否为整数集合（iset）。
;;
;; 语法
;; ----
;; (iset? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要检查的对象。
;;
;; 返回值
;; -----
;; 如果 obj 是 iset，返回 #t；否则返回 #f。
;;
(check-true (iset? (iset)))
(check-true (iset? pos-set))
(check-false (iset? '()))
(check-false (iset? "not a set"))
(check-false (iset? 42))

;;
;; iset
;; 创建一个新的整数集合。
;;
;; 语法
;; ----
;; (iset element ...)
;;
;; 参数
;; ----
;; element ... : exact-integer
;; 初始元素（可选）。
;;
;; 返回值
;; -----
;; 返回包含指定元素的新 iset。
;;
(check-true (iset? (iset 1 2 3)))
(check (iset->list (iset 2 3 5 7 11)) => '(2 3 5 7 11))
(check (iset->list (iset)) => '())

;;
;; iset-unfold
;; 使用 unfold 模式创建整数集合。
;;
;; 语法
;; ----
;; (iset-unfold stop? mapper successor seed)
;;
;; 参数
;; ----
;; stop? : procedure
;; 停止谓词。接收当前种子，返回布尔值。
;;
;; mapper : procedure
;; 映射函数。接收当前种子，返回要添加到集合的整数。
;;
;; successor : procedure
;; 后继函数。接收当前种子，返回下一个种子。
;;
;; seed : any
;; 初始种子值。
;;
;; 返回值
;; -----
;; 返回生成的 iset。
;;
(check (iset->list (iset-unfold (lambda (n) (> n 64))
                                values
                                (lambda (n) (* n 2))
                                2))
       => '(2 4 8 16 32 64)
) ;check

;;
;; make-range-iset
;; 创建一个包含整数范围的集合。
;;
;; 语法
;; ----
;; (make-range-iset start end)
;; (make-range-iset start end step)
;;
;; 参数
;; ----
;; start : exact-integer
;; 包含的起始值。
;;
;; end : exact-integer
;; 不包含的结束值。
;;
;; step : exact-integer (可选，默认为 1)
;; 步长值。可以为负数。
;;
;; 返回值
;; -----
;; 返回包含从 start 到 end（不包含）的整数序列的 iset。
;;
(check (iset->list (make-range-iset 25 30)) => '(25 26 27 28 29))
(check (iset->list (make-range-iset -10 10 6)) => '(-10 -4 2 8))
(check (iset->list (make-range-iset 10 -10 -6)) => '(-8 -2 4 10))

;;
;; iset-contains?
;; 检查集合是否包含指定元素。
;;
;; 语法
;; ----
;; (iset-contains? iset element)
;;
;; 参数
;; ----
;; iset : iset
;; 目标集合。
;;
;; element : exact-integer
;; 要检查的元素。
;;
;; 返回值
;; -----
;; 如果 iset 包含 element，返回 #t；否则返回 #f。
;;
(check-true (iset-contains? (iset 2 3 5 7 11) 5))
(check-false (iset-contains? (iset 2 3 5 7 11) 4))
(check-true (every (lambda (n) (iset-contains? pos-set n)) pos-seq))
(check-false (any (lambda (n) (iset-contains? pos-set n)) neg-seq))

;;
;; iset-empty?
;; 检查集合是否为空。
;;
;; 语法
;; ----
;; (iset-empty? iset)
;;
;; 参数
;; ----
;; iset : iset
;; 要检查的集合。
;;
;; 返回值
;; -----
;; 如果 iset 为空，返回 #t；否则返回 #f。
;;
(check-true (iset-empty? (iset)))
(check-false (iset-empty? pos-set))
(check-false (iset-empty? (iset 2 3 5 7 11)))

;;
;; iset-disjoint?
;; 检查两个集合是否不相交（没有共同元素）。
;;
;; 语法
;; ----
;; (iset-disjoint? iset1 iset2)
;;
;; 参数
;; ----
;; iset1, iset2 : iset
;; 要检查的集合。
;;
;; 返回值
;; -----
;; 如果两个集合没有共同元素，返回 #t；否则返回 #f。
;;
(check-true (iset-disjoint? (iset 1 3 5) (iset 0 2 4)))
(check-false (iset-disjoint? (iset 1 3 5) (iset 2 3 4)))
(check-true (iset-disjoint? pos-set neg-set))
(check-false (iset-disjoint? dense-set sparse-set))

;;
;; iset-member
;; 查找集合中与指定元素相等的元素。
;;
;; 语法
;; ----
;; (iset-member iset element default)
;;
;; 参数
;; ----
;; iset : iset
;; 要检查的集合。
;;
;; element : exact-integer
;; 要查找的元素。
;;
;; default : any
;; 如果 element 不在集合中，返回的值。
;;
;; 返回值
;; -----
;; 如果 element 在 iset 中，返回该元素；否则返回 default。
;;
(check (iset-member (iset 2 3 5 7 11) 7 #f) => 7)
(check (iset-member (iset 2 3 5 7 11) 4 'failure) => 'failure)

;;
;; iset-min / iset-max
;; 返回集合中的最小/最大元素。
;;
;; 语法
;; ----
;; (iset-min iset)
;; (iset-max iset)
;;
;; 参数
;; ----
;; iset : iset
;; 要查询的集合。
;;
;; 返回值
;; -----
;; 返回集合中的最小/最大整数，如果集合为空则返回 #f。
;;
(check (iset-min (iset 2 3 5 7 11)) => 2)
(check (iset-max (iset 2 3 5 7 11)) => 11)
(check (iset-max (iset)) => #f)
(check (iset-min pos-set) => (car pos-seq))
(check (iset-max pos-set) => (list-ref pos-seq (- (length pos-seq) 1)))

;;
;; iset-adjoin
;; 返回一个新集合，包含原集合的所有元素以及新增的元素。
;;
;; 语法
;; ----
;; (iset-adjoin iset element ...)
;;
;; 参数
;; ----
;; iset : iset
;; 初始集合。
;;
;; element ... : exact-integer
;; 要添加的元素。
;;
;; 返回值
;; -----
;; 返回一个新的 iset。
;;
(check (iset->list (iset-adjoin (iset 1 3 5) 0)) => '(0 1 3 5))
(check-true (iset-contains? (iset-adjoin neg-set 10) 10))

;;
;; iset-adjoin!
;; 与 iset-adjoin 相同，但可以修改并返回原集合。
;;
(check (iset->list (iset-adjoin! (iset 1 3 5) 0)) => '(0 1 3 5))

;;
;; iset-delete / iset-delete!
;; 返回一个新集合，移除指定元素。
;;
;; 语法
;; ----
;; (iset-delete iset element ...)
;; (iset-delete! iset element ...)
;;
;; 参数
;; ----
;; iset : iset
;; 初始集合。
;;
;; element ... : exact-integer
;; 要移除的元素。
;;
;; 返回值
;; -----
;; 返回一个新的 iset（或修改后的原集合）。
;;
(check (iset->list (iset-delete (iset 1 3 5) 3)) => '(1 5))
(check (iset->list (iset-delete (iset 1 2 3) 4)) => '(1 2 3))

;;
;; iset-delete-all / iset-delete-all!
;; 与 iset-delete 相同，但接受一个元素列表。
;;
;; 语法
;; ----
;; (iset-delete-all iset element-list)
;; (iset-delete-all! iset element-list)
;;
(check (iset->list (iset-delete-all (iset 2 3 5 7 11) '(3 4 5))) => '(2 7 11))

;;
;; iset-delete-min / iset-delete-max
;; 返回最小/最大元素和剩余集合。
;;
;; 语法
;; ----
;; (iset-delete-min iset)
;; (iset-delete-max iset)
;;
;; 返回值
;; -----
;; 返回两个值：最小/最大元素和包含其余元素的新集合。
;; 如果集合为空则报错。
;;
(let-values (((n set) (iset-delete-min (iset 2 3 5 7 11))))
  (check n => 2)
  (check (iset->list set) => '(3 5 7 11))
) ;let-values

(let-values (((n set) (iset-delete-max (iset 2 3 5 7 11))))
  (check n => 11)
  (check (iset->list set) => '(2 3 5 7))
) ;let-values

;;
;; iset-search
;; 在集合中搜索元素，并使用 continuation 决定更新方式。
;;
;; 语法
;; ----
;; (iset-search iset element failure success)
;;
;; 参数
;; ----
;; iset : iset
;; 目标集合。
;;
;; element : exact-integer
;; 要搜索的元素。
;;
;; failure : procedure
;; 当元素不存在时调用，接收 insert 和 ignore 两个 continuation。
;;
;; success : procedure
;; 当元素存在时调用，接收匹配元素、update 和 remove 三个 continuation。
;;
;; 返回值
;; -----
;; 返回两个值：可能更新后的集合和 obj。
;;
;; iset-search insertion
(call-with-values
  (lambda ()
    (iset-search mixed-set
                 1
                 (lambda (insert _) (insert #t))
                 (lambda (x update _) (update 1 #t))
    ) ;iset-search
  ) ;lambda
  (lambda (set _)
    (check-true (iset=? (iset-adjoin mixed-set 1) set))
  ) ;lambda
) ;call-with-values

;; iset-search ignore
(call-with-values
  (lambda ()
    (iset-search mixed-set
                 1
                 (lambda (_ ignore) (ignore #t))
                 (lambda (x _ remove) (remove #t))
    ) ;iset-search
  ) ;lambda
  (lambda (set _)
    (check-true (iset=? mixed-set set))
  ) ;lambda
) ;call-with-values

;; iset-search remove
(call-with-values
  (lambda ()
    (iset-search mixed-set
                 2
                 (lambda (_ ignore) (ignore #t))
                 (lambda (x _ remove) (remove #t))
    ) ;iset-search
  ) ;lambda
  (lambda (set _)
    (check-true (iset=? (iset-delete mixed-set 2) set))
  ) ;lambda
) ;call-with-values

;;
;; iset-size
;; 返回集合中元素的数量。
;;
;; 语法
;; ----
;; (iset-size iset)
;;
(check (iset-size (iset)) => 0)
(check (iset-size (iset 1 3 5)) => 3)
(check (iset-size pos-set) => (length pos-seq))

;;
;; iset-find
;; 查找集合中满足谓词的最小元素。
;;
;; 语法
;; ----
;; (iset-find predicate iset failure)
;;
;; 参数
;; ----
;; predicate : procedure
;; 接受一个整数并返回布尔值的函数。
;;
;; iset : iset
;; 要搜索的集合。
;;
;; failure : procedure
;; 无参函数，当没有元素满足谓词时调用。
;;
;; 返回值
;; -----
;; 返回满足谓词的最小元素，或 failure 的调用结果。
;;
(check (iset-find positive? (iset -1 1) (lambda () #f)) => 1)
(check (iset-find zero? (iset -1 1) (lambda () #f)) => #f)
(check (iset-find even? (iset 1 3 5 7 8 9 10) (lambda () #f)) => 8)

;;
;; iset-count
;; 计算集合中满足谓词的元素数量。
;;
;; 语法
;; ----
;; (iset-count predicate iset)
;;
(check (iset-count positive? (iset -2 -1 1 2)) => 2)
(check (iset-count even? (iset)) => 0)
(check (iset-count even? pos-set) => (count even? pos-seq))

;;
;; iset-any?
;; 检查集合中是否有元素满足谓词。
;;
;; 语法
;; ----
;; (iset-any? predicate iset)
;;
;; 返回值
;; -----
;; 如果至少有一个元素满足谓词，返回 #t；否则返回 #f。
;; 注意：不同于 SRFI 1 的 any，此函数不返回满足谓词的元素。
;;
(check-true (iset-any? positive? (iset -2 -1 1 2)))
(check-false (iset-any? zero? (iset -2 -1 1 2)))
(check-false (iset-any? even? (iset)))

;;
;; iset-every?
;; 检查集合中是否所有元素都满足谓词。
;;
;; 语法
;; ----
;; (iset-every? predicate iset)
;;
;; 返回值
;; -----
;; 如果所有元素都满足谓词，返回 #t；否则返回 #f。
;; 注意：空集合返回 #t。
;;
(check-true (iset-every? (lambda (x) (< x 5)) (iset -2 -1 1 2)))
(check-false (iset-every? positive? (iset -2 -1 1 2)))
(check-true (iset-every? even? (iset)))

;;
;; iset-map
;; 对集合中的每个元素应用函数，并返回一个新集合。
;;
;; 语法
;; ----
;; (iset-map proc iset)
;;
;; 参数
;; ----
;; proc : procedure
;; 接受一个整数并返回整数的函数。
;;
;; iset : iset
;; 源集合。
;;
;; 返回值
;; -----
;; 返回新的 iset，包含 proc 的结果。
;; 注意：如果 proc 返回非整数会报错；如果产生重复元素会被去重。
;;
(check-true (iset=? (iset-map (lambda (x) (* 10 x)) (iset 1 11 21))
                    (iset 10 110 210))
) ;check-true
(check (iset->list (iset-map (lambda (x) (quotient x 2)) (iset 1 2 3 4 5)))
       => '(0 1 2)
) ;check

;;
;; iset-for-each
;; 对集合中的每个元素应用函数，忽略返回值。
;;
;; 语法
;; ----
;; (iset-for-each proc iset)
;;
;; 参数
;; ----
;; proc : procedure
;; 要应用的函数。
;;
;; iset : iset
;; 目标集合。
;;
;; 注意
;; ----
;; 按递增数值顺序应用 proc。
;;
(check (let ((sum 0))
         (iset-for-each (lambda (x) (set! sum (+ sum x))) (iset 2 3 5 7 11))
         sum)
       => 28
) ;check

;;
;; iset-fold / iset-fold-right
;; 对集合进行折叠操作。
;;
;; 语法
;; ----
;; (iset-fold proc nil iset)
;; (iset-fold-right proc nil iset)
;;
;; 参数
;; ----
;; proc : procedure
;; 接受元素和累积值，返回新累积值的函数。
;;
;; nil : any
;; 初始累积值。
;;
;; iset : iset
;; 目标集合。
;;
;; 注意
;; ----
;; iset-fold 按递增顺序折叠，iset-fold-right 按递减顺序折叠。
;;
(check (iset-fold + 0 (iset 2 3 5 7 11)) => 28)
(check (iset-fold cons '() (iset 2 3 5 7 11)) => '(11 7 5 3 2))
(check (iset-fold-right cons '() (iset 2 3 5 7 11)) => '(2 3 5 7 11))

;;
;; iset-filter / iset-filter!
;; 返回仅包含满足谓词元素的新集合。
;;
;; 语法
;; ----
;; (iset-filter predicate iset)
;; (iset-filter! predicate iset)
;;
(check (iset->list (iset-filter (lambda (x) (< x 6)) (iset 2 3 5 7 11)))
       => '(2 3 5)
) ;check
(check (iset->list (iset-filter! (lambda (x) (< x 6)) (iset 2 3 5 7 11)))
       => '(2 3 5)
) ;check

;;
;; iset-remove / iset-remove!
;; 返回仅包含不满足谓词元素的新集合。
;;
;; 语法
;; ----
;; (iset-remove predicate iset)
;; (iset-remove! predicate iset)
;;
(check (iset->list (iset-remove (lambda (x) (< x 6)) (iset 2 3 5 7 11)))
       => '(7 11)
) ;check
(check (iset->list (iset-remove! (lambda (x) (< x 6)) (iset 2 3 5 7 11)))
       => '(7 11)
) ;check

;;
;; iset-partition / iset-partition!
;; 将集合划分为满足和不满足谓词的两个集合。
;;
;; 语法
;; ----
;; (iset-partition predicate iset)
;; (iset-partition! predicate iset)
;;
;; 返回值
;; -----
;; 返回两个值：满足谓词的集合和不满足谓词的集合。
;;
(let-values (((low high) (iset-partition (lambda (x) (< x 6))
                                          (iset 2 3 5 7 11))))
  (check (iset->list low) => '(2 3 5))
  (check (iset->list high) => '(7 11))
) ;let-values

;;
;; iset-copy
;; 复制一个集合。
;;
;; 语法
;; ----
;; (iset-copy iset)
;;
;; 返回值
;; -----
;; 返回包含相同元素的新 iset。
;;
(check-true (not (eqv? (iset-copy pos-set) pos-set)))
(check-true (iset=? (iset-copy pos-set) pos-set))

;;
;; iset->list
;; 将集合转换为有序列表。
;;
;; 语法
;; ----
;; (iset->list iset)
;;
;; 返回值
;; -----
;; 返回按递增顺序排列的元素列表。
;;
(check (iset->list (iset)) => '())
(check (iset->list pos-set) => pos-seq)
(check (iset->list neg-set) => neg-seq)

;;
;; list->iset
;; 将列表转换为集合。
;;
;; 语法
;; ----
;; (list->iset list)
;;
;; 参数
;; ----
;; list : list of exact-integers
;; 要转换的列表。
;;
;; 返回值
;; -----
;; 返回包含列表元素的新 iset。重复元素会被去重。
;;
(check (iset->list (list->iset '(-3 -1 0 2))) => '(-3 -1 0 2))

;;
;; list->iset!
;; 将列表元素并入集合。
;;
;; 语法
;; ----
;; (list->iset! iset list)
;;
;; 返回值
;; -----
;; 返回包含原集合和列表元素的 iset。可以修改原集合。
;;
(check (iset->list (list->iset! (iset 2 3 5) '(-3 -1 0)))
       => '(-3 -1 0 2 3 5)
) ;check

;;
;; iset=?
;; 检查两个或多个集合是否相等（包含相同元素）。
;;
;; 语法
;; ----
;; (iset=? iset1 iset2 ...)
;;
(check-true (iset=? (iset) (iset)))
(check-true (iset=? (iset 1 2 3 4) (iset 2 1 4 3)))
(check-true (iset=? (iset 1 2 3 4) (iset 2 1 4 3) (iset 3 2 1 4)))
(check-false (iset=? (iset 1 2 3 4) (iset 2 3 4)))

;;
;; iset<? / iset>? / iset<=? / iset>=?
;; 检查子集关系。
;;
;; 语法
;; ----
;; (iset<? iset1 iset2 ...)  ; 真子集
;; (iset>? iset1 iset2 ...)  ; 真超集
;; (iset<=? iset1 iset2 ...) ; 子集
;; (iset>=? iset1 iset2 ...) ; 超集
;;
(check-true (iset<? (iset) pos-set))
(check-true (iset<? pos-set pos-set+))
(check-false (iset<? pos-set pos-set))
(check-true (iset<=? (iset) pos-set))
(check-true (iset<=? pos-set pos-set))
(check-true (iset>? pos-set+ pos-set))
(check-true (iset>=? pos-set+ pos-set))
(check-true (iset>=? pos-set pos-set))

;;
;; iset-union / iset-union!
;; 返回多个集合的并集。
;;
;; 语法
;; ----
;; (iset-union iset1 iset2 ...)
;; (iset-union! iset1 iset2 ...)
;;
(check (iset->list (iset-union (iset 0 1 3) (iset 0 2 4)))
       => '(0 1 2 3 4)
) ;check
(check (iset->list (iset-union pos-set neg-set))
       => (iset->list (list->iset (append pos-seq neg-seq)))
) ;check

;;
;; iset-intersection / iset-intersection!
;; 返回多个集合的交集。
;;
;; 语法
;; ----
;; (iset-intersection iset1 iset2 ...)
;; (iset-intersection! iset1 iset2 ...)
;;
(check (iset->list (iset-intersection (iset 0 1 3 4) (iset 0 2 4)))
       => '(0 4)
) ;check
(check-true (iset-empty? (iset-intersection pos-set neg-set)))

;;
;; iset-difference / iset-difference!
;; 返回第一个集合与其余集合的差集。
;;
;; 语法
;; ----
;; (iset-difference iset1 iset2 ...)
;; (iset-difference! iset1 iset2 ...)
;;
(check (iset->list (iset-difference (iset 0 1 3 4) (iset 0 2) (iset 0 4)))
       => '(1 3)
) ;check
(check (iset->list (iset-difference pos-set neg-set))
       => pos-seq
) ;check

;;
;; iset-xor / iset-xor!
;; 返回两个集合的对称差集。
;;
;; 语法
;; ----
;; (iset-xor iset1 iset2)
;; (iset-xor! iset1 iset2)
;;
(check (iset->list (iset-xor (iset 0 1 3) (iset 0 2 4)))
       => '(1 2 3 4)
) ;check
(check (iset->list (iset-xor pos-set pos-set)) => '())

;;
;; iset-open-interval
;; 返回集合中在开区间 (low, high) 内的元素。
;;
;; 语法
;; ----
;; (iset-open-interval iset low high)
;;
;; 参数
;; ----
;; iset : iset
;; 源集合。
;;
;; low, high : exact-integer
;; 区间边界（不包含）。
;;
(check (iset->list (iset-open-interval (iset 2 3 5 7 11) 2 7))
       => '(3 5)
) ;check
(check-true (iset-empty? (iset-open-interval neg-set 0 50)))

;;
;; iset-closed-interval
;; 返回集合中在闭区间 [low, high] 内的元素。
;;
;; 语法
;; ----
;; (iset-closed-interval iset low high)
;;
;; 参数
;; ----
;; low, high : exact-integer
;; 区间边界（包含）。
;;
(check (iset->list (iset-closed-interval (iset 2 3 5 7 11) 2 7))
       => '(2 3 5 7)
) ;check

;;
;; iset-open-closed-interval
;; 返回集合中在左开右闭区间 (low, high] 内的元素。
;;
;; 语法
;; ----
;; (iset-open-closed-interval iset low high)
;;
(check (iset->list (iset-open-closed-interval (iset 2 3 5 7 11) 2 7))
       => '(3 5 7)
) ;check

;;
;; iset-closed-open-interval
;; 返回集合中在左闭右开区间 [low, high) 内的元素。
;;
;; 语法
;; ----
;; (iset-closed-open-interval iset low high)
;;
(check (iset->list (iset-closed-open-interval (iset 2 3 5 7 11) 2 7))
       => '(2 3 5)
) ;check

;;
;; isubset=
;; 返回集合中等于 k 的元素（结果最多包含一个元素）。
;;
;; 语法
;; ----
;; (isubset= iset k)
;;
(check (iset->list (isubset= pos-set 90)) => '())
(check (iset->list (isubset= pos-set 100)) => '(100))

;;
;; isubset<
;; 返回集合中小于 k 的元素。
;;
;; 语法
;; ----
;; (isubset< iset k)
;;
(check (iset->list (isubset< pos-set 109)) => '(100 103 106))
(check (iset->list (isubset< mixed-set -4)) => '(-10 -7))

;;
;; isubset<=
;; 返回集合中小于或等于 k 的元素。
;;
;; 语法
;; ----
;; (isubset<= iset k)
;;
(check (iset->list (isubset<= pos-set 109)) => '(100 103 106 109))
(check (iset->list (isubset<= mixed-set -4)) => '(-10 -7 -4))

;;
;; isubset>
;; 返回集合中大于 k 的元素。
;;
;; 语法
;; ----
;; (isubset> iset k)
;;
(check (iset->list (isubset> pos-set 148)) => '(151 154 157))
(check (iset->list (isubset> mixed-set 38)) => '(41 44 47))

;;
;; isubset>=
;; 返回集合中大于或等于 k 的元素。
;;
;; 语法
;; ----
;; (isubset>= iset k)
;;
(check (iset->list (isubset>= pos-set 148)) => '(148 151 154 157))
(check (iset->list (isubset>= mixed-set 38)) => '(38 41 44 47))

(check-report)

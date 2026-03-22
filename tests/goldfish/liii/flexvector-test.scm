;
; Copyright (C) 2020 Adam Nelson
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
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;;
;; flexvector?
;; 检查对象是否为可变长向量（flexvector）。
;;
;; 语法
;; ----
;; (flexvector? obj)
;;
;; 参数
;; ----
;; obj : any
;; 要检查的对象。
;;
;; 返回值
;; -----
;; 如果 obj 是 flexvector，返回 #t；否则返回 #f。
;;
(check-true (flexvector? (flexvector)))
(check-false (flexvector? '()))
(check-false (flexvector? "not a flexvector"))
(check-false (flexvector? 42))

;;
;; flexvector / make-flexvector
;; 创建一个新的可变长向量。
;;
;; 语法
;; ----
;; (flexvector element ...)
;; (make-flexvector size)
;; (make-flexvector size fill)
;;
;; 参数
;; ----
;; element ... : any
;; 初始元素（可选）。
;;
;; size : exact-nonnegative-integer
;; 向量的初始容量。
;;
;; fill : any
;; 填充值。
;;
;; 返回值
;; -----
;; 返回包含指定元素的新 flexvector。
;;
(check (flexvector-length (make-flexvector 3 #f)) => 3)
(check (flexvector-length (flexvector 1 2 3)) => 3)
(check (flexvector->vector (make-flexvector 3 'a)) => #(a a a))

;;
;; flexvector-length
;; 返回可变长向量的长度。
;;
;; 语法
;; ----
;; (flexvector-length fv)
;;
(check (flexvector-length (flexvector)) => 0)
(check (flexvector-length (flexvector 1 2 3)) => 3)

;;
;; flexvector-ref / flexvector-front / flexvector-back
;; 访问可变长向量中的元素。
;;
;; 语法
;; ----
;; (flexvector-ref fv index)
;; (flexvector-front fv)
;; (flexvector-back fv)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; index : exact-nonnegative-integer
;; 元素索引，从 0 开始。
;;
;; 返回值
;; -----
;; 返回指定位置的元素。
;;
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-ref fv 1) => 'b)
  (check (flexvector-front fv) => 'a)
  (check (flexvector-back fv) => 'c)
) ;let

;;
;; flexvector-set!
;; 设置可变长向量中指定位置的元素。
;;
;; 语法
;; ----
;; (flexvector-set! fv index value)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; index : exact-nonnegative-integer
;; 元素索引。
;;
;; value : any
;; 新值。
;;
;; 返回值
;; -----
;; 返回原来的值。
;;
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-set! fv 1 'd) => 'b)
  (check (flexvector-ref fv 1) => 'd)
) ;let

;;
;; flexvector-add! / flexvector-add-front! / flexvector-add-back!
;; 向可变长向量中添加元素。
;;
;; 语法
;; ----
;; (flexvector-add! fv index element ...)
;; (flexvector-add-front! fv element ...)
;; (flexvector-add-back! fv element ...)
;;
;; 参数
;; ----
;; fv : flexvector
;; 目标向量。
;;
;; index : exact-nonnegative-integer
;; 插入位置。
;;
;; element ... : any
;; 要添加的元素。
;;
;; 返回值
;; -----
;; 返回修改后的 flexvector。
;;
(let ((fv (flexvector)))
  (flexvector-add-back! fv 'a)
  (check (flexvector-length fv) => 1)
  (check (flexvector-ref fv 0) => 'a)
  (flexvector-add-front! fv 'b)
  (check (flexvector-ref fv 0) => 'b)
  (check (flexvector-ref fv 1) => 'a)
  (flexvector-add! fv 1 'c)
  (check (flexvector-ref fv 1) => 'c)
  (check (flexvector-ref fv 2) => 'a)
) ;let

;;
;; flexvector-remove! / flexvector-remove-front! / flexvector-remove-back!
;; 从可变长向量中移除元素。
;;
;; 语法
;; ----
;; (flexvector-remove! fv index)
;; (flexvector-remove-front! fv)
;; (flexvector-remove-back! fv)
;;
;; 返回值
;; -----
;; 返回被移除的元素。
;;
(let ((fv (flexvector 'a 'b 'c)))
  (check (flexvector-remove! fv 1) => 'b)
  (check (flexvector-length fv) => 2)
  (check (flexvector-ref fv 1) => 'c)
  (check (flexvector-remove-front! fv) => 'a)
  (check (flexvector-length fv) => 1)
  (check (flexvector-ref fv 0) => 'c)
  (check (flexvector-remove-back! fv) => 'c)
  (check (flexvector-empty? fv) => #t)
) ;let

;;
;; flexvector-remove-range!
;; 移除指定范围内的元素。
;;
;; 语法
;; ----
;; (flexvector-remove-range! fv start end)
;;
(let ((fv (flexvector 'a 'b 'c 'd 'e 'f)))
  (flexvector-remove-range! fv 1 4)
  (check (flexvector->list fv) => '(a e f))
) ;let

(let ((fv (flexvector 'a 'b 'c 'd 'e 'f)))
  (flexvector-remove-range! fv 1 1)
  (check (flexvector->list fv) => '(a b c d e f))
) ;let

;;
;; flexvector-clear!
;; 清空可变长向量。
;;
;; 语法
;; ----
;; (flexvector-clear! fv)
;;
(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-clear! fv)
  (check (flexvector-length fv) => 0)
  (check (flexvector-empty? fv) => #t)
) ;let

;;
;; flexvector-empty?
;; 检查可变长向量是否为空。
;;
;; 语法
;; ----
;; (flexvector-empty? fv)
;;
(check-true (flexvector-empty? (flexvector)))
(check-false (flexvector-empty? (flexvector 1 2 3)))

;;
;; flexvector=?
;; 比较两个或多个可变长向量是否相等。
;;
;; 语法
;; ----
;; (flexvector=? eq? fv1 fv2 ...)
;;
(check-true (flexvector=? eq? (flexvector 'a 'b) (flexvector 'a 'b)))
(check-false (flexvector=? eq? (flexvector 'a 'b) (flexvector 'b 'a)))
(check-false (flexvector=? = (flexvector 1 2 3 4 5) (flexvector 1 2 3 4)))
(check-true (flexvector=? eq?))
(check-true (flexvector=? eq? (flexvector 'a)))

;;
;; flexvector->vector / vector->flexvector
;; 可变长向量与向量之间的转换。
;;
;; 语法
;; ----
;; (flexvector->vector fv)
;; (flexvector->vector fv start)
;; (flexvector->vector fv start end)
;; (vector->flexvector vec)
;; (vector->flexvector vec start)
;; (vector->flexvector vec start end)
;;
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->vector fv) => #(1 2 3))
) ;let

(check (flexvector->vector (vector->flexvector #(1 2 3))) => #(1 2 3))

;;
;; flexvector->list / list->flexvector
;; 可变长向量与列表之间的转换。
;;
;; 语法
;; ----
;; (flexvector->list fv)
;; (flexvector->list fv start)
;; (flexvector->list fv start end)
;; (list->flexvector list)
;;
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->list fv) => '(1 2 3))
) ;let

(check (flexvector->list (list->flexvector '(a b c))) => '(a b c))

;;
;; reverse-flexvector->list / reverse-list->flexvector
;; 反向转换。
;;
(let ((fv (flexvector 1 2 3)))
  (check (reverse-flexvector->list fv) => '(3 2 1))
) ;let

;;
;; string->flexvector / flexvector->string
;; 字符串与可变长向量之间的转换。
;;
(check (flexvector->vector (string->flexvector "abc")) => #(#\a #\b #\c))
(check (flexvector->string (flexvector #\a #\b #\c)) => "abc")

;;
;; flexvector-copy
;; 复制可变长向量。
;;
;; 语法
;; ----
;; (flexvector-copy fv)
;; (flexvector-copy fv start)
;; (flexvector-copy fv start end)
;;
(let ((fv (flexvector 1 2 3)))
  (let ((copy (flexvector-copy fv)))
    (check (flexvector-length fv) => (flexvector-length copy))
    (check-false (eq? fv copy))
    (check (flexvector-ref copy 0) => 1)
    (flexvector-set! copy 0 'x)
    (check (flexvector-ref fv 0) => 1)
    (check (flexvector-ref copy 0) => 'x)
  ) ;let
) ;let

;;
;; flexvector-copy!
;; 将源向量的内容复制到目标向量。
;;
;; 语法
;; ----
;; (flexvector-copy! to at from)
;; (flexvector-copy! to at from start)
;; (flexvector-copy! to at from start end)
;;
(let ((to (flexvector 1 2 3 4 5))
      (from (flexvector 20 30 40)))
  (flexvector-copy! to 1 from)
  (check (flexvector->list to) => '(1 20 30 40 5))
) ;let

(let ((to (flexvector 1 2 3 4 5))
      (from (flexvector 10 20 30 40 50)))
  (flexvector-copy! to 1 from 1 4)
  (check (flexvector->list to) => '(1 20 30 40 5))
) ;let

;;
;; flexvector-reverse! / flexvector-reverse-copy
;; 反转可变长向量。
;;
;; 语法
;; ----
;; (flexvector-reverse! fv)
;; (flexvector-reverse! fv start)
;; (flexvector-reverse! fv start end)
;; (flexvector-reverse-copy fv)
;; (flexvector-reverse-copy fv start)
;; (flexvector-reverse-copy fv start end)
;;
(let ((fv (flexvector 1 2 3)))
  (let ((rev (flexvector-reverse-copy fv)))
    (check (flexvector->list rev) => '(3 2 1))
    (check (flexvector->list fv) => '(1 2 3))
  ) ;let
  (flexvector-reverse! fv)
  (check (flexvector->list fv) => '(3 2 1))
) ;let

;;
;; flexvector-reverse-copy!
;; 反向复制到目标向量。
;;
(let ((to (flexvector 1 2 3 4 5))
      (from (flexvector 20 30 40)))
  (flexvector-reverse-copy! to 1 from)
  (check (flexvector->list to) => '(1 40 30 20 5))
) ;let

;;
;; flexvector-fill!
;; 填充可变长向量。
;;
;; 语法
;; ----
;; (flexvector-fill! fv fill)
;; (flexvector-fill! fv fill start)
;; (flexvector-fill! fv fill start end)
;;
(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'x)
  (check (flexvector->list fv) => '(x x x x x))
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'y 2)
  (check (flexvector->list fv) => '(1 2 y y y))
) ;let

(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'z 1 3)
  (check (flexvector->list fv) => '(1 z z 4 5))
) ;let

;;
;; flexvector-swap!
;; 交换两个位置的元素。
;;
;; 语法
;; ----
;; (flexvector-swap! fv i j)
;;
(let ((fv (flexvector 10 20 30)))
  (flexvector-swap! fv 0 2)
  (check (flexvector->list fv) => '(30 20 10))
) ;let

;;
;; flexvector-for-each / flexvector-for-each/index
;; 遍历可变长向量。
;;
;; 语法
;; ----
;; (flexvector-for-each proc fv)
;; (flexvector-for-each proc fv1 fv2 ...)
;; (flexvector-for-each/index proc fv)
;; (flexvector-for-each/index proc fv1 fv2 ...)
;;
(let ((fv (flexvector 10 20 30))
      (res '()))
  (flexvector-for-each (lambda (x) (set! res (cons x res))) fv)
  (check res => '(30 20 10))
) ;let

(let ((fv (flexvector 10 20 30))
      (res '()))
  (flexvector-for-each/index
    (lambda (i x) (set! res (cons (+ x (* i 2)) res)))
    fv
  ) ;flexvector-for-each/index
  (check res => '(34 22 10))
) ;let

;;
;; flexvector-map / flexvector-map! / flexvector-map/index / flexvector-map/index!
;; 映射操作。
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector (flexvector-map (lambda (x) (* x 10)) fv))
         => #(100 200 300)
  ) ;check
) ;let

(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector
           (flexvector-map/index (lambda (i x) (+ x (* i 2))) fv))
         => #(10 22 34)
  ) ;check
) ;let

(let ((fv (flexvector 10 20 30)))
  (flexvector-map! (lambda (x) (* x 10)) fv)
  (check (flexvector->list fv) => '(100 200 300))
) ;let

;;
;; flexvector-fold / flexvector-fold-right
;; 折叠操作。
;;
;; 语法
;; ----
;; (flexvector-fold proc nil fv)
;; (flexvector-fold proc nil fv1 fv2 ...)
;; (flexvector-fold-right proc nil fv)
;; (flexvector-fold-right proc nil fv1 fv2 ...)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-fold (lambda (acc x) (cons x acc)) '() fv)
         => '(30 20 10)
  ) ;check
  (check (flexvector-fold-right (lambda (acc x) (cons x acc)) '() fv)
         => '(10 20 30)
  ) ;check
) ;let

;;
;; flexvector-filter / flexvector-filter! / flexvector-filter/index
;; 过滤操作。
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector
           (flexvector-filter (lambda (x) (< x 25)) fv))
         => #(10 20)
  ) ;check
) ;let

(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector
           (flexvector-filter/index (lambda (i x) (not (= i 1))) fv))
         => #(10 30)
  ) ;check
) ;let

(let ((fv (flexvector 10 20 30)))
  (flexvector-filter! (lambda (x) (< x 25)) fv)
  (check (flexvector->list fv) => '(10 20))
) ;let

;;
;; flexvector-append-map / flexvector-append-map/index
;; 映射并追加结果。
;;
(check (flexvector->vector
         (flexvector-append-map (lambda (x) (flexvector x (* x 10)))
                                (flexvector 10 20 30))
         ) ;flexvector-append-map
       => #(10 100 20 200 30 300)
) ;check

;;
;; flexvector-count
;; 统计满足条件的元素数量。
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-count (lambda (x) (< x 25)) fv) => 2)
) ;let

;;
;; flexvector-cumulate
;; 累积计算。
;;
(check (flexvector->vector
         (flexvector-cumulate + 0 (flexvector 3 1 4 1 5 9 2 5 6)))
       => #(3 4 8 9 14 23 25 30 36)
) ;check

;;
;; flexvector-index / flexvector-index-right / flexvector-skip / flexvector-skip-right
;; 查找元素索引。
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-index (lambda (x) (> x 10)) fv) => 1)
  (check (flexvector-index-right (lambda (x) (> x 10)) fv) => 2)
  (check (flexvector-skip (lambda (x) (< x 25)) fv) => 2)
) ;let

;;
;; flexvector-any / flexvector-every
;; 检查是否存在或全部满足条件的元素。
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-any (lambda (x) (= x 20)) fv) => #t)
  (check (flexvector-any (lambda (x) (= x 21)) fv) => #f)
  (check (flexvector-every (lambda (x) (< x 40)) fv) => #t)
  (check (flexvector-every (lambda (x) (< x 30)) fv) => #f)
) ;let

;;
;; flexvector-binary-search
;; 二分查找。
;;
;; 语法
;; ----
;; (flexvector-binary-search fv value cmp)
;; (flexvector-binary-search fv value cmp start)
;; (flexvector-binary-search fv value cmp start end)
;;
(let ((fv (flexvector #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j))
      (cmp (lambda (char1 char2)
             (cond ((char<? char1 char2) -1)
                   ((char=? char1 char2) 0)
                   (else 1)))
             ) ;cond
      ) ;cmp
  (check (flexvector-binary-search fv #\d cmp) => 3)
  (check (flexvector-binary-search fv #\a cmp) => 0)
  (check (flexvector-binary-search fv #\j cmp) => 9)
  (check (flexvector-binary-search fv #\k cmp) => #f)
  (check (flexvector-binary-search fv #\f cmp 2 6) => 5)
  (check (flexvector-binary-search fv #\f cmp 1 5) => #f)
) ;let

;;
;; flexvector-partition
;; 分区操作。
;;
;; 语法
;; ----
;; (flexvector-partition pred? fv)
;;
;; 返回值
;; -----
;; 返回两个值：满足谓词的 flexvector 和不满足谓词的 flexvector。
;;
(let ((fv (flexvector 10 20 30)))
  (let-values (((low high) (flexvector-partition (lambda (x) (< x 25)) fv)))
    (check (flexvector->vector low) => #(10 20))
    (check (flexvector->vector high) => #(30))
  ) ;let-values
) ;let

;;
;; flexvector-append / flexvector-concatenate / flexvector-append-subvectors
;; 连接可变长向量。
;;
(check (flexvector->vector
         (flexvector-append (flexvector 10 20) (flexvector) (flexvector 30 40)))
       => #(10 20 30 40)
) ;check

(check (flexvector->vector
         (flexvector-concatenate
           (list (flexvector 10 20) (flexvector) (flexvector 30 40)))
         ) ;flexvector-concatenate
       => #(10 20 30 40)
) ;check

(check (flexvector->vector
         (flexvector-append-subvectors
           (flexvector 'a 'b 'c 'd 'e) 0 2
           (flexvector 'f 'g 'h 'i 'j) 2 4)
         ) ;flexvector-append-subvectors
       => #(a b h i)
) ;check

;;
;; flexvector-append!
;; 破坏性连接。
;;
(let ((fv (flexvector 10 20)))
  (flexvector-append! fv (flexvector 30 40))
  (check (flexvector->vector fv) => #(10 20 30 40))
) ;let

;;
;; flexvector-unfold / flexvector-unfold-right
;; 展开操作。
;;
(check (flexvector->vector
         (flexvector-unfold (lambda (x) (> x 10))
                            (lambda (x) (* x x))
                            (lambda (x) (+ x 1))
                            1)
         ) ;flexvector-unfold
       => #(1 4 9 16 25 36 49 64 81 100)
) ;check

(check (flexvector->vector
         (flexvector-unfold-right (lambda (x) (> x 10))
                                  (lambda (x) (* x x))
                                  (lambda (x) (+ x 1))
                                  1)
         ) ;flexvector-unfold-right
       => #(100 81 64 49 36 25 16 9 4 1)
) ;check

;;
;; flexvector->generator / generator->flexvector
;; 生成器转换。
;;
(let ((gen (flexvector->generator (flexvector 'a 'b 'c))))
  (check (gen) => 'a)
  (check (gen) => 'b)
  (check (gen) => 'c)
  (check (eof-object? (gen)) => #t)
) ;let

(let ((genlist '(a b c)))
  (define (mock-generator)
    (if (pair? genlist)
      (let ((value (car genlist)))
        (set! genlist (cdr genlist))
        value
      ) ;let
      (eof-object)
    ) ;if
  ) ;define
  (check (flexvector->list (generator->flexvector mock-generator))
         => '(a b c)
  ) ;check
) ;let

(check-report)

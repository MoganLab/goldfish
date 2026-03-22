;
; Copyright (C) 2024 The Goldfish Scheme Authors
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
        (liii sort)
) ;import

(check-set-mode! 'report-failed)

(define (pair-< x y)
  (< (car x) (car y))
) ;define

(define (pair-full-< x y)
  (cond
    ((not (= (car x) (car y))) (< (car x) (car y)))
    (else (< (cdr y) (cdr x)))
  ) ;cond
) ;define

(check-false (list-sorted? < '(1 5 1 0 -1 9 2 4 3)))
(check-false (vector-sorted? < #(1 5 1 0 -1 9 2 4 3)))

(check-true (list-sorted? < (list-sort < '(1 5 1 0 -1 9 2 4 3))))
(check-true (list-sorted? < (list-stable-sort < '(1 5 1 0 -1 9 2 4 3))))
(check-true (list-sorted? pair-< (list-merge pair-< '((1 . 1) (1 . 2) (3 . 1)) '((1 . 3) (2 . 1) (3 . 2) (4 . 1)))))
(check (list-merge pair-< '((1 . 1) (1 . 2) (3 . 1)) '((1 . 3) (2 . 1) (3 . 2) (4 . 1)))
       => '((1 . 1) (1 . 2) (1 . 3) (2 . 1) (3 . 1) (3 . 2) (4 . 1))
) ;check
(check-true (list-sorted? pair-full-<
                          (list-merge pair-full-< '((1 . 2) (1 . 1) (3 . 1)) '((1 . 3) (2 . 1) (3 . 2) (4 . 1))))
) ;check-true
(check (list-merge pair-full-< '((1 . 2) (1 . 1) (3 . 1)) '((1 . 3) (2 . 1) (3 . 2) (4 . 1)))
       => '((1 . 3) (1 . 2) (1 . 1) (2 . 1) (3 . 2) (3 . 1) (4 . 1))
) ;check

(check-true (vector-sorted? < (vector-sort < #(1 5 1 0 -1 9 2 4 3))))
(check-true (vector-sorted? < (vector-stable-sort < #(1 5 1 0 -1 9 2 4 3))))
(check-true (vector-sorted? pair-<
                            (vector-merge pair-< #((1 . 1) (1 . 2) (3 . 1)) #((1 . 3) (2 . 1) (3 . 2) (4 . 1))))
) ;check-true
(check (vector-merge pair-< #((1 . 1) (1 . 2) (3 . 1)) #((1 . 3) (2 . 1) (3 . 2) (4 . 1)))
       => #((1 . 1) (1 . 2) (1 . 3) (2 . 1) (3 . 1) (3 . 2) (4 . 1))
) ;check
(check-true (vector-sorted? pair-full-<
                            (vector-merge pair-full-< #((1 . 2) (1 . 1) (3 . 1)) #((1 . 3) (2 . 1) (3 . 2) (4 . 1))))
) ;check-true
(check (vector-merge pair-full-< #((1 . 2) (1 . 1) (3 . 1)) #((1 . 3) (2 . 1) (3 . 2) (4 . 1)))
       => #((1 . 3) (1 . 2) (1 . 1) (2 . 1) (3 . 2) (3 . 1) (4 . 1))
) ;check

;
;单元测试vector-sorted?新增支持可选参数功能、list-merge!实现原地合并功能
;
(display "Testing vector-sorted?\n")
(check-true (vector-sorted? < #(1 2 3 4 5)))
(check-false (vector-sorted? < #(1 3 2 4 5)))
(check-true (vector-sorted? < #(5 1 2 3 4) 1))
(check-false (vector-sorted? < #(5 1 3 2 4) 1))
(check-true (vector-sorted? < #(5 1 2 3 4) 1 3))
(check-false (vector-sorted? < #(5 1 3 2 4) 1 4))
(check-catch "Invalid start or end parameters" (vector-sorted? < #(1 2 3 4 5) 3 2))

(display "Testing list-merge!\n")
(define lis1 '(1 3 5))
(define lis2 '(2 4 6))
(test (list-merge! < lis1 lis2) '(1 2 3 4 5 6))

(define lis3 '(1 1 3))
(define lis4 '(1 2 4))
(test (list-merge! < lis3 lis4) '(1 1 1 2 3 4))

(define lis5 '())
(define lis6 '(1 2 3))
(test (list-merge! < lis5 lis6) '(1 2 3))
(test (list-merge! < lis6 lis5) '(1 2 3))

(define lis7 '())
(define lis8 '())
(test (list-merge! < lis7 lis8) '())

;
;单元测试: 测试新增函数list-sort、list-sort!、list-stable-sort!
;
;; ================== 测试非破坏性快排 list-sort! ==================
(display "Testing list-sort\n")
(define test-list '(3 1 4 1 5 9 2 6 5))
(define sorted-list (list-sort < test-list))
(check-true (list-sorted? < sorted-list))
(check (length sorted-list) => (length test-list))
(check sorted-list => '(1 1 2 3 4 5 5 6 9))
(check (equal? test-list '(3 1 4 1 5 9 2 6 5)) => #t)  ; 确保原列表未被修改

;; ================== 测试破坏性快排 list-sort! ==================
(display "Testing list-sort!\n")
(check-true (list-sorted? < (list-sort! < '(1 5 1 0 -1 9 2 4 3))))  ;; 测试原地排序后的结果
(check-true (list-sorted? < (list-sort! < '(9 7 5 3 2 8 6 4 1))))  ;; 测试另一种顺序
(check-false (list-sorted? < '(1 5 1 0 -1 9 2 4 3)))  ;; 原始未排序的列表
(check-true (list-sorted? < (list-sort! < '())))
(check-true (list-sorted? < (list-sort! < '(42))))
(check-true (list-sorted? < (list-sort! < '(1 2 3 4 5))))
(check-true (list-sorted? < (list-sort! < '(3 1 4 1 5 9 2 6 5 3 5))))
(check-true (list-sorted? < (list-sort! < '(0 -1 2 -2 3 1))))
(check-true (list-sorted? < (list-sort! < '(5 -3 0 2 1 -1 4))))

; ================== 测试非稳定堆排序 list-stable-sort! ==================
(display "Testing list-stable-sort!\n")
(display "Testing list-stable-sort!\n")
(check-true (list-sorted? < (list-stable-sort! < '(1 5 1 0 -1 1 5 1 0 1 1 5 9 2 4 3 4 9))))
(check-true (list-sorted? < (list-stable-sort! < '(9 7 5 3 2 8 6 4 1 4 6 8 9 7 5 3 5 9 7 9))))
(check-true (list-sorted? < (list-stable-sort! < '(3 1 4 1 5 9 2 6 5 3 5 5 9 2 6 9))))  ;; 验证排序是否正确
(check-true (list-sorted? < (list-stable-sort! < '(0 -1 2 -2 0 -1 0 2 3 1 3))))  ;; 验证排序是否正确
(check-true (list-sorted? < (list-stable-sort! < '(5 -3 0 2 1 -1 2 1 2 4 5 -3 0 5))))  ;; 验证排序是否正确


(check-report)
1920
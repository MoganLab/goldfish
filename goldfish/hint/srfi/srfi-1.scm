;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; (hint srfi srfi-1) - (srfi srfi-1) 的类型接口
;;
;; 本库只提供类型签名数据，不导出 (srfi srfi-1) 的任何过程，
;; 也不导入其他 hint 库，因此可以被独立加载。
;;
;; 目前只登记 (liii alist) 实际用到的过程，以及由 (liii alist) 再导出的
;; alist-cons（实现在本库，签名只在本库声明）。

(define-library (hint srfi srfi-1)
  (import (scheme base))
  (export hint-srfi/srfi-1)
  (begin

    ;; xcons : (any? any? -> pair?)
    (define hint-xcons '((any? any? -> pair?)))
    ;; cons* : 可变参数；单参返回该值本身，多参返回点对
    (define hint-cons*
      '((any? -> any?) (any? any? -> pair?) (any? any? any? -> pair?))
    ) ;define

    ;; 谓词
    ;; circular-list? : (any? -> boolean?)
    (define hint-circular-list? '((any? -> boolean?)))

    ;; proper-list? : (any? -> boolean?)
    (define hint-proper-list? '((any? -> boolean?)))

    ;; dotted-list? : (any? -> boolean?)
    (define hint-dotted-list? '((any? -> boolean?)))

    ;; null-list? : (any? -> boolean?)
    (define hint-null-list? '((any? -> boolean?)))

    ;; 取值器
    ;; first..tenth : (pair? -> any?)，实参需至少含对应数量的元素
    (define hint-first '((pair? -> any?)))
    (define hint-second '((pair? -> any?)))
    (define hint-third '((pair? -> any?)))
    (define hint-fourth '((pair? -> any?)))
    (define hint-fifth '((pair? -> any?)))
    (define hint-sixth '((pair? -> any?)))
    (define hint-seventh '((pair? -> any?)))
    (define hint-eighth '((pair? -> any?)))
    (define hint-ninth '((pair? -> any?)))
    (define hint-tenth '((pair? -> any?)))

    ;; last : (pair? -> any?)，非空列表的末元素
    (define hint-last '((pair? -> any?)))

    ;; last-pair : (pair? -> pair?)，非空列表的末尾点对
    (define hint-last-pair '((pair? -> pair?)))

    ;; 切片
    ;; take/drop/take-right/drop-right : (list? integer? -> list?)
    ;; take/drop-right 也接受点尾列表，签名统一用 list? 表达
    (define hint-take '((list? integer? -> list?)))
    (define hint-drop '((list? integer? -> list?)))
    (define hint-take-right '((list? integer? -> list?)))
    (define hint-drop-right '((list? integer? -> list?)))

    ;; split-at : (list? integer? -> (前缀 后缀))，多返回值用 list? 占位
    (define hint-split-at '((list? integer? -> list?)))

    ;; 折叠：可变列表参数，登记 1~3 个列表的形式
    ;; fold : (procedure? any? list? -> any?)
    (define hint-fold
      '((procedure? any? list? -> any?)
        (procedure? any? list? list? -> any?)
        (procedure? any? list? list? list? -> any?))
    ) ;define

    ;; fold-right : 同 fold
    (define hint-fold-right
      '((procedure? any? list? -> any?)
        (procedure? any? list? list? -> any?)
        (procedure? any? list? list? list? -> any?))
    ) ;define

    ;; reduce/reduce-right : (procedure? any? list? -> any?)
    (define hint-reduce '((procedure? any? list? -> any?)))
    (define hint-reduce-right '((procedure? any? list? -> any?)))

    ;; every : 列表中每个元素都满足谓词时返回 #t
    (define hint-every '((procedure? list? -> boolean?)))

    ;; filter : (procedure? list? -> list?)
    (define hint-filter '((procedure? list? -> list?)))

    ;; alist-cons : (any? any? alist? -> alist?)，由 (liii alist) 再导出
    (define hint-alist-cons '((any? any? alist? -> alist?)))

    (define (hint-srfi/srfi-1 name)
      (case name
       ((fold) hint-fold)
       ((fold-right) hint-fold-right)
       ((take) hint-take)
       ((drop) hint-drop)
       ((take-right) hint-take-right)
       ((drop-right) hint-drop-right)
       ((split-at) hint-split-at)
       ((first) hint-first)
       ((second) hint-second)
       ((third) hint-third)
       ((fourth) hint-fourth)
       ((fifth) hint-fifth)
       ((sixth) hint-sixth)
       ((seventh) hint-seventh)
       ((eighth) hint-eighth)
       ((ninth) hint-ninth)
       ((tenth) hint-tenth)
       ((last) hint-last)
       ((last-pair) hint-last-pair)
       ((circular-list?) hint-circular-list?)
       ((proper-list?) hint-proper-list?)
       ((dotted-list?) hint-dotted-list?)
       ((null-list?) hint-null-list?)
       ((xcons) hint-xcons)
       ((cons*) hint-cons*)
       ((reduce) hint-reduce)
       ((reduce-right) hint-reduce-right)
       ((every) hint-every)
       ((filter) hint-filter)
       ((alist-cons) hint-alist-cons)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library

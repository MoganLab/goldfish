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
        (liii array-buffer)
        (liii oop)
        (liii lang)
) ;import

(check-set-mode! 'report-failed)
(define == class=?)

(check (array-buffer :from-vector #(1 2 3) :collect) => #(1 2 3))

(check (array-buffer :from-list '(1 2 3) :collect) => #(1 2 3))

(let ((arr (array-buffer :from-list '(1 2 3))))
  (check (arr :length) => 3)
  (check (arr 0) => 1)
  (check (arr 1) => 2)
  (check (arr 2) => 3)
  (check-catch 'index-error (arr 3))
) ;let

(let ((arr (array-buffer :from-list '(1 2 3))))
  (check (arr :set! 0 4 :collect) => #(4 2 3))
  (check (arr :update! 1 5 :collect) => #(4 5 3))
) ;let

(let ((arr (array-buffer :from-list '(1 2 3))))
  (check (arr :extend! 5 :length) => 3)
  (check-true (== (arr :extend! 10)
                  (array-buffer :from-list '(1 2 3)))
  ) ;check-true
  (check (arr :size-hint! 15 :length) => 3)
  (check-true (== (arr :size-hint! 20)
                  (array-buffer :from-list '(1 2 3)))
  ) ;check-true
) ;let

(let ((arr (array-buffer :from-list '(1 2 3))))
  (check (arr :add-one! 4 :collect) => #(1 2 3 4))
  (check (arr :add-one! 5 :length) => 5)
  (check-true (== (arr :add-one! 6)
                  (array-buffer :from-list '(1 2 3 4 5 6)))
  ) ;check-true
) ;let

(let ((arr (array-buffer :from-list '(1 2 3))))
  (check (arr :clear! :length) => 0)
  (check (arr :add-one! 4 :collect) => #(4))
  (check-true (== (arr :clear!)
                  (array-buffer :from-list '()))
  ) ;check-true
) ;let

(let ((arr (array-buffer :from-list '(1 2 3))))
  (check (arr :clear/shrink! :length) => 0)
  (check (arr :add-one! 4 :collect) => #(4))
  (check-true (== (arr :clear/shrink!)
                  (array-buffer :from-list '()))
  ) ;check-true
) ;let

(let ((arr (array-buffer :from-list '(1 2 3))))
  (check (arr :insert! 0 0 :collect) ==> #(0 1 2 3))
  (check (arr :insert! 2 5 :collect) ==> #(0 1 5 2 3))
  (check (arr :insert! 5 6 :length) ==> 6)
  (check-true (== (arr :insert! 3 4)
                  (array-buffer :from-list '(0 1 5 4 2 3 6)))
  ) ;check-true
  (check-catch 'index-error (arr :insert! 8 9))
) ;let

(check-true (== (array-buffer :from-list '(1 2 3))
                (array-buffer :from-list '(1 2) :add-one! 3))
) ;check-true
(check-true (== (array-buffer :from-list '(1 2 3))
                (array-buffer :from-list '(1 2 3) :extend! 10))
) ;check-true

(check (array-buffer :from-list '(1 2 3) :to-list) => '(1 2 3))

(check ((array-buffer :from-list '(1 2 3) :to-rich-list) :collect) => '(1 2 3))

(let ((arb (array-buffer :from-list '(3 1 2 5 4))))
  (check (arb :collect) => #(3 1 2 5 4))
  (check (arb :to-vector) => (rich-vector #(3 1 2 5 4)))
  (check (arb :to-list) => '(3 1 2 5 4))
  (check ((arb :to-rich-list) :collect) => '(3 1 2 5 4))

  (arb :add-one! 0)
  (check (arb :collect) => #(3 1 2 5 4 0))
  (check (arb :to-vector) => (rich-vector #(3 1 2 5 4 0)))
  (check (arb :to-list) => '(3 1 2 5 4 0))
  (check ((arb :to-rich-list) :collect) => '(3 1 2 5 4 0))

  (arb :insert! 0 0)
  (check (arb :collect) => #(0 3 1 2 5 4 0))
  (check (arb :to-vector) => (rich-vector #(0 3 1 2 5 4 0)))
  (check (arb :to-list) => '(0 3 1 2 5 4 0))
  (check ((arb :to-rich-list) :collect) => '(0 3 1 2 5 4 0))

  (check (arb :resize! 4 :to-list) => '(0 3 1 2))
  (check ((arb :to-rich-list) :collect) => '(0 3 1 2))

  (check (arb :resize! 3 :to-vector) => (rich-vector #(0 3 1)))
  (check (arb :insert! 1 2 :to-list) => '(0 2 3 1))
  (check ((arb :to-rich-list) :collect) => '(0 2 3 1))

  (check (arb :collect) => #(0 2 3 1))
  (check (arb 0) => 0)
  (check (arb 1) => 2)
  (check-catch 'index-error (arb 5))
) ;let

(check-report)


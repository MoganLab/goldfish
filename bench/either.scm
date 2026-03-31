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

(import (liii timeit)
        (liii rich-either)
        (liii base))

(define (benchmark-either-operations)
  (display "=== Either Operations Benchmark ===\n\n")

  ; Test either%get-or-else with right value
  (let ((time (timeit (lambda () ((right 65536) :get-or-else 0)) :number 100000)))
    (display* "either%get-or-else (right):\t" (number->string time) " seconds\n"))

  ; Test either%get-or-else with left value
  (let ((time (timeit (lambda () ((left "error") :get-or-else 0)) :number 100000)))
    (display* "either%get-or-else (left):\t" (number->string time) " seconds\n"))

  ; Test either%or-else with left value
  (let ((time (timeit (lambda ()
                              ((left "error") :or-else (right 0))) :number 100000)))
    (display* "either%or-else (left):\t\t" (number->string time) " seconds\n"))

  ; Test either%or-else with right value
  (let ((time (timeit (lambda () ((right 65536) :or-else (left 0))) :number 100000)))
    (display* "either%or-else (right):\t\t" (number->string time) " seconds\n"))

  ; Test either%left? and either%right?
  (let ((time (timeit (lambda () ((right 42) :left?)) :number 100000)))
    (display* "either%left? (right):\t\t" (number->string time) " seconds\n"))

  (let ((time (timeit (lambda () ((left "error") :right?)) :number 100000)))
    (display* "either%right? (left):\t\t" (number->string time) " seconds\n"))

  ; Test either%get
  (let ((time (timeit (lambda () ((right 42) :get)) :number 100000)))
    (display* "either%get (right):\t\t" (number->string time) " seconds\n"))

  ; Test either%filter-or-else
  (let ((time (timeit (lambda () ((right 42) :filter-or-else even? 0)) :number 100000)))
    (display* "either%filter-or-else (right):\t" (number->string time) " seconds\n"))

  (let ((time (timeit (lambda () ((left "error") :filter-or-else even? 0)) :number 100000)))
    (display* "either%filter-or-else (left):\t" (number->string time) " seconds\n"))

  ; Test either%contains
  (let ((time (timeit (lambda () ((right 42) :contains 42)) :number 100000)))
    (display* "either%contains (right):\t\t" (number->string time) " seconds\n"))

  (let ((time (timeit (lambda () ((left "error") :contains 42)) :number 100000)))
    (display* "either%contains (left):\t\t" (number->string time) " seconds\n"))

  ; Test either%map
  (let ((time (timeit (lambda () ((right 42) :map (lambda (x) (* x 2)))) :number 100000)))
    (display* "either%map (right):\t\t" (number->string time) " seconds\n"))

  (let ((time (timeit (lambda () ((left "error") :map (lambda (x) (* x 2)))) :number 100000)))
    (display* "either%map (left):\t\t" (number->string time) " seconds\n"))

  ; Test either%flat-map
  (let ((time (timeit (lambda () ((right 42) :flat-map (lambda (x) (right (* x 2))))) :number 100000)))
    (display* "either%flat-map (right):\t" (number->string time) " seconds\n"))

  (let ((time (timeit (lambda () ((left "error") :flat-map (lambda (x) (right (* x 2))))) :number 100000)))
    (display* "either%flat-map (left):\t" (number->string time) " seconds\n"))

  ; Test either%to-option
  (let ((time (timeit (lambda () ((right 42) :to-option)) :number 100000)))
    (display* "either%to-option (right):\t" (number->string time) " seconds\n"))

  (let ((time (timeit (lambda () ((left "error") :to-option)) :number 100000)))
    (display* "either%to-option (left):\t" (number->string time) " seconds\n"))

  ; Test either%forall and either%exists
  (let ((time (timeit (lambda () ((right 42) :forall even?)) :number 100000)))
    (display* "either%forall (right):\t\t" (number->string time) " seconds\n"))

  (let ((time (timeit (lambda () ((right 42) :exists even?)) :number 100000)))
    (display* "either%exists (right):\t\t" (number->string time) " seconds\n"))

  (display "\n"))

; Run the benchmark
(benchmark-either-operations)


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
        (rename (liii rich-option) (rich-option option) (rich-none none))
        (liii lang))

(define (run-option-benchmarks)
  (display "=== Option 模块性能基准测试 ===\n\n")

  ; 测试 option 构造性能
  (let ((time (timeit (lambda () (option 42)) :number 100000)))
    (display* "option 构造: \t\t" (number->string time) " 秒\n"))

  ; 测试 none 构造性能
  (let ((time (timeit (lambda () (none)) :number 100000)))
    (display* "none 构造: \t\t" (number->string time) " 秒\n"))

  ; 测试 %get 方法性能（包含构造时间）
  (let ((time (timeit (lambda ()
                        (let ((opt (option 42)))
                          (opt :get))) :number 100000)))
    (display* "%get 方法（含构造）: \t" (number->string time) " 秒\n"))

  ; 测试 %get 方法性能（纯访问）
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :get)) :number 100000)))
      (display* "%get 方法（纯访问）: \t" (number->string time) " 秒\n")))

  ; 测试 %get-or-else 方法性能（有值情况，包含构造时间）
  (let ((time (timeit (lambda ()
                        (let ((opt (option 42)))
                          (opt :get-or-else 0))) :number 100000)))
    (display* "%get-or-else (有值，含构造): " (number->string time) " 秒\n"))

  ; 测试 %get-or-else 方法性能（有值情况，纯访问）
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :get-or-else 0)) :number 100000)))
      (display* "%get-or-else (有值，纯访问): " (number->string time) " 秒\n")))

  ; 测试 %get-or-else 方法性能（无值情况，包含构造时间）
  (let ((time (timeit (lambda ()
                        (let ((opt (none)))
                          (opt :get-or-else 0))) :number 100000)))
    (display* "%get-or-else (无值，含构造): " (number->string time) " 秒\n"))

  ; 测试 %get-or-else 方法性能（无值情况，纯访问）
  (let ((opt (none)))
    (let ((time (timeit (lambda () (opt :get-or-else 0)) :number 100000)))
      (display* "%get-or-else (无值，纯访问): " (number->string time) " 秒\n")))

  ; 测试 %defined? 方法性能（包含构造时间）
  (let ((time (timeit (lambda ()
                        (let ((opt (option 42)))
                          (opt :defined?))) :number 100000)))
    (display* "%defined? 方法（含构造）: \t" (number->string time) " 秒\n"))

  ; 测试 %defined? 方法性能（纯访问）
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :defined?)) :number 100000)))
      (display* "%defined? 方法（纯访问）: \t" (number->string time) " 秒\n")))

  ; 测试 %empty? 方法性能（包含构造时间）
  (let ((time (timeit (lambda ()
                        (let ((opt (none)))
                          (opt :empty?))) :number 100000)))
    (display* "%empty? 方法（含构造）: \t" (number->string time) " 秒\n"))

  ; 测试 %empty? 方法性能（纯访问）
  (let ((opt (none)))
    (let ((time (timeit (lambda () (opt :empty?)) :number 100000)))
      (display* "%empty? 方法（纯访问）: \t" (number->string time) " 秒\n")))

  ; 测试 %map 方法性能
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :map (lambda (x) (+ x 1)))) :number 100000)))
      (display* "%map 方法: \t\t" (number->string time) " 秒\n")))

  ; 测试 %flat-map 方法性能
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :flat-map (lambda (x) (option (+ x 1))))) :number 100000)))
      (display* "%flat-map 方法: \t" (number->string time) " 秒\n")))

  ; 测试 %filter 方法性能（通过）
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :filter (lambda (x) #t))) :number 100000)))
      (display* "%filter (通过): \t" (number->string time) " 秒\n")))

  ; 测试 %filter 方法性能（不通过）
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :filter (lambda (x) #f))) :number 100000)))
      (display* "%filter (不通过): \t" (number->string time) " 秒\n")))

  ; 测试 %forall 方法性能
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :forall (lambda (x) #t))) :number 100000)))
      (display* "%forall 方法: \t" (number->string time) " 秒\n")))

  ; 测试 %exists 方法性能
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :exists (lambda (x) #t))) :number 100000)))
      (display* "%exists 方法: \t" (number->string time) " 秒\n")))

  ; 测试 %contains 方法性能
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :contains 42)) :number 100000)))
      (display* "%contains 方法: \t" (number->string time) " 秒\n")))

  ; 测试 %for-each 方法性能
  (let ((opt (option 42)))
    (let ((time (timeit (lambda () (opt :for-each (lambda (x) #t))) :number 100000)))
      (display* "%for-each 方法: \t" (number->string time) " 秒\n")))

  ; 测试 %equals 方法性能
  (let ((opt1 (option 42))
        (opt2 (option 42)))
    (let ((time (timeit (lambda () (opt1 :equals opt2)) :number 100000)))
      (display* "%equals 方法: \t" (number->string time) " 秒\n")))

  (display "\n=== 测试完成 ===\n"))

; 运行基准测试
(run-option-benchmarks)

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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(import (scheme base)
  (liii string)
  (liii timeit)
) ;import

;; string-contains2: 基于 string-position 的实现
(define (string-contains2 str sub-str)
  (if (string-position sub-str str) #t #f))

(define (bench-case label str sub-str iterations)
  (let ((t1 (timeit
               (lambda () (string-contains str sub-str))
               '()
               iterations))
        (t2 (timeit
               (lambda () (string-contains2 str sub-str))
               '()
               iterations)))
    (display label)
    (newline)
    (display "  string-contains : ")
    (display t1)
    (display "s")
    (newline)
    (display "  string-contains2: ")
    (display t2)
    (display "s")
    (newline)
    (display "  ratio           : ")
    (display (/ t1 t2))
    (newline)
    (newline)))

(define iterations 1000000)

;; 短字符串匹配
(bench-case "short match" "hello world" "world" iterations)

;; 短字符串不匹配
(bench-case "short no match" "hello world" "xyz" iterations)

;; 子串在开头
(bench-case "match at start" "hello world" "hello" iterations)

;; 子串在末尾
(bench-case "match at end" "hello world" "world" iterations)

;; 空子串
(bench-case "empty sub-str" "hello world" "" iterations)

;; 长字符串
(let ((long-str (make-string 10000 #\a))
      (needle (string-append (make-string 99 #\a) "b")))
  (bench-case "long no match" long-str needle 10000))

(let ((long-str (string-append (make-string 5000 #\a) "abcde"))
      (needle "abcde"))
  (bench-case "long match at end" long-str needle 10000))

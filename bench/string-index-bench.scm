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
  (scheme char)
  (liii string)
  (liii timeit)
) ;import

;; string-index2: 基于 char-position 的实现（仅支持字符查找）
(define (string-index2 str char/pred? . start+end)
  (if (char? char/pred?)
    (let ((start (if (null? start+end) 0 (car start+end))))
      (char-position char/pred? str start))
    ;; fallback to string-index for predicates
    (apply string-index str char/pred? start+end)))

(define (bench-case label str char iterations)
  (let ((t1 (timeit
               (lambda () (string-index str char))
               '()
               iterations))
        (t2 (timeit
               (lambda () (string-index2 str char))
               '()
               iterations)))
    (display label)
    (newline)
    (display "  string-index : ")
    (display t1)
    (display "s")
    (newline)
    (display "  string-index2: ")
    (display t2)
    (display "s")
    (newline)
    (display "  ratio        : ")
    (display (/ t1 t2))
    (newline)
    (display "  result       : ")
    (display (string-index str char))
    (newline)
    (newline)))

(define iterations 1000000)

;; 短字符串，字符在开头
(bench-case "short match at start" "hello world" #\h iterations)

;; 短字符串，字符在末尾
(bench-case "short match at end" "hello world" #\d iterations)

;; 短字符串，不匹配
(bench-case "short no match" "hello world" #\z iterations)

;; 空字符串
(bench-case "empty string" "" #\a iterations)

;; 长字符串，匹配在末尾
(let ((long-str (string-append (make-string 10000 #\a) "b")))
  (bench-case "long match at end" long-str #\b 10000))

;; 长字符串，不匹配
(let ((long-str (make-string 10000 #\a)))
  (bench-case "long no match" long-str #\z 10000))

;; 长字符串，匹配在开头
(let ((long-str (string-append "b" (make-string 10000 #\a))))
  (bench-case "long match at start" long-str #\b 10000))

;; 长字符串，匹配在中间
(let ((long-str (string-append (make-string 5000 #\a) "b" (make-string 5000 #\a))))
  (bench-case "long match at middle" long-str #\b 10000))

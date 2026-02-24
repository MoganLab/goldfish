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

(import (liii check)
        (liii base)
        (rename (liii json)
          (string->json ljson-string->json)
          (json->string ljson-json->string)
          (json-ref ljson-ref)
          (json-set ljson-set)
          (json-push ljson-push)
          (json-drop ljson-drop)
          (json-contains-key? ljson-contains-key?)
          (json-keys ljson-keys))
        (liii njson))

(define sample-json
  "{\"name\":\"Goldfish\",\"version\":\"17.11.26\",\"active\":true,\"score\":3.14,\"nums\":[1,2,3,4,5],\"meta\":{\"arch\":\"x86_64\",\"os\":\"linux\"}}")

;; Functional checks: direct nlohmann::json-handle operations
(define root (njson-string->json sample-json))
(check-true (njson? root))
(check (njson-ref root "name") => "Goldfish")
(check (njson-ref root "active") => #t)
(check (njson-ref root "meta" "arch") => "x86_64")
(check-catch 'parse-error (njson-string->json "{name:\"Goldfish\"}"))
(check-catch 'type-error (njson-ref root 'meta))
(check (njson-json->string 'null) => "null")
(check-catch 'type-error (njson-json->string 'foo))

(define root2 (njson-set root "meta" "os" "debian"))
(check (njson-ref root2 "meta" "os") => "debian")
(check (njson-ref root "meta" "os") => "linux")

(define root3 (njson-push root2 "nums" 5 99))
(check (njson-ref root3 "nums" 5) => 99)

(define root4 (njson-drop root3 "active"))
(check (njson-ref root4 "active") => '())

(define meta (njson-ref root4 "meta"))
(check-true (njson? meta))
(check (njson-ref meta "os") => "debian")

(check-true (njson-contains-key? root4 "meta"))
(check-false (njson-contains-key? root4 "active"))
(check-true (> (length (njson-keys root4)) 0))

(define roundtrip (njson-string->json (njson-json->string root4)))
(check (njson-ref roundtrip "meta" "os") => "debian")
(check (njson-ref roundtrip "nums" 5) => 99)

;; Free handle checks
(check-true (njson-free meta))
(check-catch 'type-error (njson-ref meta "os"))
(check-true (njson-free roundtrip))
(check-true (njson-free root4))
(check-true (njson-free root3))
(check-true (njson-free root2))
(check-true (njson-free root))

(define sample-json-scm (ljson-string->json sample-json))

(define bench-top-key-count 600)
(define bench-array-length 600)

(define (build-bench-array-json n)
  (let ((out (open-output-string)))
    (display "[" out)
    (do ((i 0 (+ i 1)))
        ((= i n))
      (when (> i 0)
        (display "," out))
      (display i out))
    (display "]" out)
    (get-output-string out)))

(define (build-bench-key-fields-json n)
  (let ((out (open-output-string)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (when (> i 0)
        (display "," out))
      (display "\"k" out)
      (display i out)
      (display "\":" out)
      (display i out))
    (get-output-string out)))

(define bench-json
  (let ((arr-json (build-bench-array-json bench-array-length))
        (fields-json (build-bench-key-fields-json bench-top-key-count)))
    (string-append "{"
                   "\"name\":\"Goldfish\","
                   "\"version\":\"17.11.26\","
                   "\"active\":true,"
                   "\"score\":3.14,"
                   "\"nums\":" arr-json ","
                   "\"meta\":{\"arch\":\"x86_64\",\"os\":\"linux\"},"
                   fields-json
                   "}")))

(define bench-ref-key (string-append "k" (number->string (quotient bench-top-key-count 2))))
(define bench-drop-key (string-append "k" (number->string (- bench-top-key-count 1))))
(define bench-set-value 999999)
(define bench-push-index (quotient bench-array-length 2))
(define bench-push-value 777777)

(define bench-json-scm (ljson-string->json bench-json))

(define (bench-ns-once thunk count)
  (let ((start (g_monotonic-nanosecond)))
    (do ((i 0 (+ i 1)))
        ((= i count))
      (thunk))
    (- (g_monotonic-nanosecond) start)))

(define (insert-sorted x xs)
  (cond ((null? xs) (list x))
        ((<= x (car xs)) (cons x xs))
        (else (cons (car xs) (insert-sorted x (cdr xs))))))

(define (sort-list xs)
  (let loop ((rest xs) (acc '()))
    (if (null? rest)
        acc
        (loop (cdr rest) (insert-sorted (car rest) acc)))))

(define (median xs)
  (let* ((sorted (sort-list xs))
         (n (length sorted)))
    (if (= n 0)
        0
        (list-ref sorted (quotient n 2)))))

(define (bench-ns-median thunk count rounds)
  (let loop ((i 0) (samples '()))
    (if (= i rounds)
        (median samples)
        (loop (+ i 1) (cons (bench-ns-once thunk count) samples)))))

(define (safe-ratio lhs rhs)
  (if (= rhs 0)
      0.0
      (/ (exact->inexact lhs) (exact->inexact rhs))))

(define (report-bench title count rounds liii-ns njson-ns)
  (display "[基准测试] ")
  (display title)
  (display " x")
  (display count)
  (display "，轮次=")
  (display rounds)
  (display "(取中位数)")
  (display " liii-json耗时(ns)=")
  (display liii-ns)
  (display " nlohmann-json耗时(ns)=")
  (display njson-ns)
  (display " 倍率(liii/nlohmann)=")
  (display (safe-ratio liii-ns njson-ns))
  (newline))

;; warmup to reduce first-run bias
(do ((i 0 (+ i 1)))
    ((= i 20))
  (ljson-string->json bench-json)
  (let ((h (njson-string->json bench-json)))
    (njson-free h))
  (ljson-json->string bench-json-scm)
  (let ((h (njson-string->json bench-json)))
    (njson-json->string h)
    (njson-free h))
  (ljson-ref bench-json-scm bench-ref-key)
  (let ((h (njson-string->json bench-json)))
    (njson-ref h bench-ref-key)
    (njson-free h))
  (ljson-set bench-json-scm bench-ref-key bench-set-value)
  (let ((h (njson-string->json bench-json)))
    (let ((x (njson-set h bench-ref-key bench-set-value)))
      (njson-free x))
    (njson-free h))
  (ljson-push bench-json-scm "nums" bench-push-index bench-push-value)
  (let ((h (njson-string->json bench-json)))
    (let ((x (njson-push h "nums" bench-push-index bench-push-value)))
      (njson-free x))
    (njson-free h))
  (ljson-drop bench-json-scm bench-drop-key)
  (let ((h (njson-string->json bench-json)))
    (let ((x (njson-drop h bench-drop-key)))
      (njson-free x))
    (njson-free h))
  (ljson-contains-key? bench-json-scm bench-ref-key)
  (let ((h (njson-string->json bench-json)))
    (njson-contains-key? h bench-ref-key)
    (njson-free h))
  (ljson-keys bench-json-scm)
  (let ((h (njson-string->json bench-json)))
    (njson-keys h)
    (njson-free h)))

(define parse-count 12)
(define stringify-count 12)
(define ref-count 300)
(define set-count 150)
(define push-count 150)
(define drop-count 150)
(define contains-key-count 300)
(define keys-count 200)
(define round-count 7)

(define liii-parse-ns
  (bench-ns-median (lambda () (ljson-string->json bench-json)) parse-count round-count))

(define njson-parse-ns
  (bench-ns-median
    (lambda ()
      (let ((h (njson-string->json bench-json)))
        (njson-free h)))
    parse-count
    round-count))

(define liii-stringify-ns
  (bench-ns-median (lambda () (ljson-json->string bench-json-scm)) stringify-count round-count))

(define stringify-handle (njson-string->json bench-json))
(define njson-stringify-ns
  (bench-ns-median (lambda () (njson-json->string stringify-handle)) stringify-count round-count))
(check-true (njson-free stringify-handle))

(define ref-handle (njson-string->json bench-json))
(define liii-ref-ns
  (bench-ns-median (lambda () (ljson-ref bench-json-scm bench-ref-key)) ref-count round-count))
(define njson-ref-ns
  (bench-ns-median (lambda () (njson-ref ref-handle bench-ref-key)) ref-count round-count))
(check-true (njson-free ref-handle))

(define set-handle (njson-string->json bench-json))
(define liii-set-ns
  (bench-ns-median (lambda () (ljson-set bench-json-scm bench-ref-key bench-set-value)) set-count round-count))
(define njson-set-ns
  (bench-ns-median
    (lambda ()
      (let ((h (njson-set set-handle bench-ref-key bench-set-value)))
        (njson-free h)))
    set-count
    round-count))
(check-true (njson-free set-handle))

(define push-handle (njson-string->json bench-json))
(define liii-push-ns
  (bench-ns-median
    (lambda () (ljson-push bench-json-scm "nums" bench-push-index bench-push-value))
    push-count
    round-count))
(define njson-push-ns
  (bench-ns-median
    (lambda ()
      (let ((h (njson-push push-handle "nums" bench-push-index bench-push-value)))
        (njson-free h)))
    push-count
    round-count))
(check-true (njson-free push-handle))

(define drop-handle (njson-string->json bench-json))
(define liii-drop-ns
  (bench-ns-median (lambda () (ljson-drop bench-json-scm bench-drop-key)) drop-count round-count))
(define njson-drop-ns
  (bench-ns-median
    (lambda ()
      (let ((h (njson-drop drop-handle bench-drop-key)))
        (njson-free h)))
    drop-count
    round-count))
(check-true (njson-free drop-handle))

(define contains-key-handle (njson-string->json bench-json))
(define liii-contains-key-ns
  (bench-ns-median
    (lambda () (ljson-contains-key? bench-json-scm bench-ref-key))
    contains-key-count
    round-count))
(define njson-contains-key-ns
  (bench-ns-median
    (lambda () (njson-contains-key? contains-key-handle bench-ref-key))
    contains-key-count
    round-count))
(check-true (njson-free contains-key-handle))

(define keys-handle (njson-string->json bench-json))
(define liii-keys-ns
  (bench-ns-median (lambda () (ljson-keys bench-json-scm)) keys-count round-count))
(define njson-keys-ns
  (bench-ns-median (lambda () (njson-keys keys-handle)) keys-count round-count))
(check-true (njson-free keys-handle))

(check-true (> liii-parse-ns 0))
(check-true (> njson-parse-ns 0))
(check-true (> liii-stringify-ns 0))
(check-true (> njson-stringify-ns 0))
(check-true (> liii-ref-ns 0))
(check-true (> njson-ref-ns 0))
(check-true (> liii-set-ns 0))
(check-true (> njson-set-ns 0))
(check-true (> liii-push-ns 0))
(check-true (> njson-push-ns 0))
(check-true (> liii-drop-ns 0))
(check-true (> njson-drop-ns 0))
(check-true (> liii-contains-key-ns 0))
(check-true (> njson-contains-key-ns 0))
(check-true (> liii-keys-ns 0))
(check-true (> njson-keys-ns 0))

(display "[基准测试数据] 顶层动态键=")
(display bench-top-key-count)
(display "，数组长度=")
(display bench-array-length)
(newline)

(report-bench "解析(string->json)" parse-count round-count liii-parse-ns njson-parse-ns)
(report-bench "序列化(json->string)" stringify-count round-count liii-stringify-ns njson-stringify-ns)
(report-bench "读取(json-ref)" ref-count round-count liii-ref-ns njson-ref-ns)
(report-bench "修改(json-set)" set-count round-count liii-set-ns njson-set-ns)
(report-bench "插入(json-push)" push-count round-count liii-push-ns njson-push-ns)
(report-bench "删除(json-drop)" drop-count round-count liii-drop-ns njson-drop-ns)
(report-bench "键存在(json-contains-key?)" contains-key-count round-count liii-contains-key-ns njson-contains-key-ns)
(report-bench "获取键(json-keys)" keys-count round-count liii-keys-ns njson-keys-ns)

(newline)

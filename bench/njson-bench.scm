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
    (json-keys ljson-keys)
  ) ;rename
  (liii njson)
) ;import

(define sample-json
  "{\"name\":\"Goldfish\",\"version\":\"17.11.26\",\"active\":true,\"score\":3.14,\"nums\":[1,2,3,4,5],\"meta\":{\"arch\":\"x86_64\",\"os\":\"linux\"}}"
) ;define

(define sample-json-scm
  (ljson-string->json sample-json)
) ;define



(define bench-top-key-count 600)
(define bench-array-length 600)

(define (build-bench-array-json n)
  (let ((out (open-output-string)))
    (display "[" out)
    (do ((i 0 (+ i 1)))
      ((= i n))
      (when (> i 0)
        (display "," out)
      ) ;when
      (display i out)
    ) ;do
    (display "]" out)
    (get-output-string out)
  ) ;let
) ;define

(define (build-bench-key-fields-json n)
  (let ((out (open-output-string)))
    (do ((i 0 (+ i 1)))
      ((= i n))
      (when (> i 0)
        (display "," out)
      ) ;when
      (display "\"k" out)
      (display i out)
      (display "\":" out)
      (display i out)
    ) ;do
    (get-output-string out)
  ) ;let
) ;define

(define bench-json
  (let ((arr-json (build-bench-array-json bench-array-length
                  ) ;build-bench-array-json
        ) ;arr-json
        (fields-json (build-bench-key-fields-json bench-top-key-count
                     ) ;build-bench-key-fields-json
        ) ;fields-json
       ) ;
    (string-append "{"
      "\"name\":\"Goldfish\","
      "\"version\":\"17.11.26\","
      "\"active\":true,"
      "\"score\":3.14,"
      "\"nums\":"
      arr-json
      ","
      "\"meta\":{\"arch\":\"x86_64\",\"os\":\"linux\"},"
      fields-json
      "}"
    ) ;string-append
  ) ;let
) ;define

(define bench-ref-key
  (string-append "k"
    (number->string (quotient bench-top-key-count 2)
    ) ;number->string
  ) ;string-append
) ;define
(define bench-drop-key
  (string-append "k"
    (number->string (- bench-top-key-count 1)
    ) ;number->string
  ) ;string-append
) ;define
(define bench-set-value 999999)
(define bench-push-index
  bench-array-length
) ;define
(define bench-append-drop-index
  bench-array-length
) ;define
(define bench-push-value 777777)

(define bench-json-scm
  (ljson-string->json bench-json)
) ;define

(define (bench-ns-once thunk count)
  (let ((start (g_monotonic-nanosecond)))
    (do ((i 0 (+ i 1)))
      ((= i count))
      (thunk)
    ) ;do
    (- (g_monotonic-nanosecond) start)
  ) ;let
) ;define

(define (insert-sorted x xs)
  (cond ((null? xs) (list x))
        ((<= x (car xs)) (cons x xs))
        (else (cons (car xs)
                (insert-sorted x (cdr xs))
              ) ;cons
        ) ;else
  ) ;cond
) ;define

(define (sort-list xs)
  (let loop
    ((rest xs) (acc '()))
    (if (null? rest)
      acc
      (loop (cdr rest)
        (insert-sorted (car rest) acc)
      ) ;loop
    ) ;if
  ) ;let
) ;define

(define (median xs)
  (let* ((sorted (sort-list xs))
         (n (length sorted))
        ) ;
    (if (= n 0)
      0
      (list-ref sorted (quotient n 2))
    ) ;if
  ) ;let*
) ;define

(define (bench-ns-median thunk count rounds)
  (let loop
    ((i 0) (samples '()))
    (if (= i rounds)
      (median samples)
      (loop (+ i 1)
        (cons (bench-ns-once thunk count)
          samples
        ) ;cons
      ) ;loop
    ) ;if
  ) ;let
) ;define

(define (safe-ratio lhs rhs)
  (if (= rhs 0)
    0.0
    (/ (exact->inexact lhs)
      (exact->inexact rhs)
    ) ;/
  ) ;if
) ;define

(define (report-bench title
          count
          rounds
          liii-ns
          njson-ns
        ) ;report-bench
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
  (newline)
) ;define

(define (report-variant-bench title
          count
          rounds
          lhs-name
          lhs-ns
          rhs-name
          rhs-ns
        ) ;report-variant-bench
  (display "[基准测试] ")
  (display title)
  (display " x")
  (display count)
  (display "，轮次=")
  (display rounds)
  (display "(取中位数)")
  (display " ")
  (display lhs-name)
  (display "耗时(ns)=")
  (display lhs-ns)
  (display " ")
  (display rhs-name)
  (display "耗时(ns)=")
  (display rhs-ns)
  (display " 倍率(")
  (display lhs-name)
  (display "/")
  (display rhs-name)
  (display ")=")
  (display (safe-ratio lhs-ns rhs-ns))
  (newline)
) ;define

;; warmup to reduce first-run bias
(do ((i 0 (+ i 1)))
  ((= i 20))
  (ljson-string->json bench-json)
  (let ((h (string->njson bench-json)))
    (njson-free h)
  ) ;let
  (ljson-json->string bench-json-scm)
  (let ((h (string->njson bench-json)))
    (njson->string h)
    (njson-free h)
  ) ;let
  (ljson-ref bench-json-scm bench-ref-key)
  (let ((h (string->njson bench-json)))
    (njson-ref h bench-ref-key)
    (njson-free h)
  ) ;let
  (ljson-set bench-json-scm
    bench-ref-key
    bench-set-value
  ) ;ljson-set
  (let ((h (string->njson bench-json)))
    (let ((x (njson-set h
               bench-ref-key
               bench-set-value
             ) ;njson-set
          ) ;x
         ) ;
      (njson-free x)
    ) ;let
    (njson-free h)
  ) ;let
  (ljson-push bench-json-scm
    "nums"
    bench-push-index
    bench-push-value
  ) ;ljson-push
  (let ((h (string->njson bench-json)))
    (let ((x (njson-append h "nums" bench-push-value)
          ) ;x
         ) ;
      (njson-free x)
    ) ;let
    (njson-free h)
  ) ;let
  (ljson-drop bench-json-scm
    bench-drop-key
  ) ;ljson-drop
  (let ((h (string->njson bench-json)))
    (let ((x (njson-drop h bench-drop-key)))
      (njson-free x)
    ) ;let
    (njson-free h)
  ) ;let
  (let ((h (string->njson bench-json)))
    (njson-set! h
      bench-ref-key
      bench-set-value
    ) ;njson-set!
    (njson-free h)
  ) ;let
  (let ((h (string->njson bench-json)))
    (njson-append! h
      "nums"
      bench-push-value
    ) ;njson-append!
    (njson-drop! h
      "nums"
      bench-append-drop-index
    ) ;njson-drop!
    (njson-free h)
  ) ;let
  (let ((h (string->njson bench-json)))
    (njson-set! h bench-drop-key 1)
    (njson-drop! h bench-drop-key)
    (njson-free h)
  ) ;let
  (ljson-contains-key? bench-json-scm
    bench-ref-key
  ) ;ljson-contains-key?
  (let ((h (string->njson bench-json)))
    (njson-contains-key? h bench-ref-key)
    (njson-free h)
  ) ;let
  (ljson-keys bench-json-scm)
  (let ((h (string->njson bench-json)))
    (njson-keys h)
    (njson-free h)
  ) ;let
) ;do

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
  (bench-ns-median (lambda ()
                     (ljson-string->json bench-json)
                   ) ;lambda
    parse-count
    round-count
  ) ;bench-ns-median
) ;define

(define njson-parse-ns
  (bench-ns-median (lambda ()
                     (let ((h (string->njson bench-json)))
                       (njson-free h)
                     ) ;let
                   ) ;lambda
    parse-count
    round-count
  ) ;bench-ns-median
) ;define

(define liii-stringify-ns
  (bench-ns-median (lambda ()
                     (ljson-json->string bench-json-scm)
                   ) ;lambda
    stringify-count
    round-count
  ) ;bench-ns-median
) ;define

(define stringify-handle
  (string->njson bench-json)
) ;define
(define njson-stringify-ns
  (bench-ns-median (lambda ()
                     (njson->string stringify-handle)
                   ) ;lambda
    stringify-count
    round-count
  ) ;bench-ns-median
) ;define
(check-true (njson-free stringify-handle)
) ;check-true

(define ref-handle
  (string->njson bench-json)
) ;define
(define liii-ref-ns
  (bench-ns-median (lambda ()
                     (ljson-ref bench-json-scm bench-ref-key)
                   ) ;lambda
    ref-count
    round-count
  ) ;bench-ns-median
) ;define
(define njson-ref-ns
  (bench-ns-median (lambda ()
                     (njson-ref ref-handle bench-ref-key)
                   ) ;lambda
    ref-count
    round-count
  ) ;bench-ns-median
) ;define
(check-true (njson-free ref-handle))

(define set-handle
  (string->njson bench-json)
) ;define
(define liii-set-ns
  (bench-ns-median (lambda ()
                     (ljson-set bench-json-scm
                       bench-ref-key
                       bench-set-value
                     ) ;ljson-set
                   ) ;lambda
    set-count
    round-count
  ) ;bench-ns-median
) ;define
(define njson-set-ns
  (bench-ns-median (lambda ()
                     (let ((h (njson-set set-handle
                                bench-ref-key
                                bench-set-value
                              ) ;njson-set
                           ) ;h
                          ) ;
                       (njson-free h)
                     ) ;let
                   ) ;lambda
    set-count
    round-count
  ) ;bench-ns-median
) ;define
(check-true (njson-free set-handle))

(define push-handle
  (string->njson bench-json)
) ;define
(define liii-push-ns
  (bench-ns-median (lambda ()
                     (ljson-push bench-json-scm
                       "nums"
                       bench-push-index
                       bench-push-value
                     ) ;ljson-push
                   ) ;lambda
    push-count
    round-count
  ) ;bench-ns-median
) ;define
(define njson-append-ns
  (bench-ns-median (lambda ()
                     (let ((h (njson-append push-handle
                                "nums"
                                bench-push-value
                              ) ;njson-append
                           ) ;h
                          ) ;
                       (njson-free h)
                     ) ;let
                   ) ;lambda
    push-count
    round-count
  ) ;bench-ns-median
) ;define
(check-true (njson-free push-handle))

(define drop-handle
  (string->njson bench-json)
) ;define
(define liii-drop-ns
  (bench-ns-median (lambda ()
                     (ljson-drop bench-json-scm
                       bench-drop-key
                     ) ;ljson-drop
                   ) ;lambda
    drop-count
    round-count
  ) ;bench-ns-median
) ;define
(define njson-drop-ns
  (bench-ns-median (lambda ()
                     (let ((h (njson-drop drop-handle bench-drop-key)
                           ) ;h
                          ) ;
                       (njson-free h)
                     ) ;let
                   ) ;lambda
    drop-count
    round-count
  ) ;bench-ns-median
) ;define
(check-true (njson-free drop-handle))

(define set-x-handle
  (string->njson bench-json)
) ;define
(njson-keys set-x-handle)
(define njson-set!-ns
  (bench-ns-median (lambda ()
                     (njson-set! set-x-handle
                       bench-ref-key
                       bench-set-value
                     ) ;njson-set!
                   ) ;lambda
    set-count
    round-count
  ) ;bench-ns-median
) ;define
(check-true (njson-free set-x-handle))

(define push-x-handle
  (string->njson bench-json)
) ;define
(njson-keys push-x-handle)
(define njson-append!-pair-ns
  (bench-ns-median (lambda ()
                     (njson-append! push-x-handle
                       "nums"
                       bench-push-value
                     ) ;njson-append!
                     ;; restore array shape to keep each iteration comparable
                     (njson-drop! push-x-handle
                       "nums"
                       bench-append-drop-index
                     ) ;njson-drop!
                   ) ;lambda
    push-count
    round-count
  ) ;bench-ns-median
) ;define
(check-true (njson-free push-x-handle))

(define drop-x-handle
  (string->njson bench-json)
) ;define
(njson-keys drop-x-handle)
(define njson-drop!-pair-ns
  (bench-ns-median (lambda ()
                     ;; restore key first so drop! always executes a real deletion
                     (njson-set! drop-x-handle
                       bench-drop-key
                       1
                     ) ;njson-set!
                     (njson-drop! drop-x-handle
                       bench-drop-key
                     ) ;njson-drop!
                   ) ;lambda
    drop-count
    round-count
  ) ;bench-ns-median
) ;define
(check-true (njson-free drop-x-handle))

(define contains-key-handle
  (string->njson bench-json)
) ;define
(define liii-contains-key-ns
  (bench-ns-median (lambda ()
                     (ljson-contains-key? bench-json-scm
                       bench-ref-key
                     ) ;ljson-contains-key?
                   ) ;lambda
    contains-key-count
    round-count
  ) ;bench-ns-median
) ;define
(define njson-contains-key-ns
  (bench-ns-median (lambda ()
                     (njson-contains-key? contains-key-handle
                       bench-ref-key
                     ) ;njson-contains-key?
                   ) ;lambda
    contains-key-count
    round-count
  ) ;bench-ns-median
) ;define
(check-true (njson-free contains-key-handle)
) ;check-true

(define keys-handle
  (string->njson bench-json)
) ;define
(define liii-keys-ns
  (bench-ns-median (lambda () (ljson-keys bench-json-scm))
    keys-count
    round-count
  ) ;bench-ns-median
) ;define
(define njson-keys-ns
  (bench-ns-median (lambda () (njson-keys keys-handle))
    keys-count
    round-count
  ) ;bench-ns-median
) ;define
(check-true (njson-free keys-handle))

(check-true (>= liii-parse-ns 0))
(check-true (>= njson-parse-ns 0))
(check-true (>= liii-stringify-ns 0))
(check-true (>= njson-stringify-ns 0))
(check-true (>= liii-ref-ns 0))
(check-true (>= njson-ref-ns 0))
(check-true (>= liii-set-ns 0))
(check-true (>= njson-set-ns 0))
(check-true (>= liii-push-ns 0))
(check-true (>= njson-append-ns 0))
(check-true (>= liii-drop-ns 0))
(check-true (>= njson-drop-ns 0))
(check-true (>= njson-set!-ns 0))
(check-true (>= njson-append!-pair-ns 0)
) ;check-true
(check-true (>= njson-drop!-pair-ns 0))
(check-true (>= liii-contains-key-ns 0))
(check-true (>= njson-contains-key-ns 0)
) ;check-true
(check-true (>= liii-keys-ns 0))
(check-true (>= njson-keys-ns 0))

(display "[基准测试数据] 顶层动态键="
) ;display
(display bench-top-key-count)
(display "，数组长度=")
(display bench-array-length)
(newline)

(report-bench "解析(string->json)"
  parse-count
  round-count
  liii-parse-ns
  njson-parse-ns
) ;report-bench
(report-bench "序列化(json->string)"
  stringify-count
  round-count
  liii-stringify-ns
  njson-stringify-ns
) ;report-bench
(report-bench "读取(json-ref)"
  ref-count
  round-count
  liii-ref-ns
  njson-ref-ns
) ;report-bench
(report-bench "修改(json-set)"
  set-count
  round-count
  liii-set-ns
  njson-set-ns
) ;report-bench
(report-bench "插入(json-push vs njson-append)"
  push-count
  round-count
  liii-push-ns
  njson-append-ns
) ;report-bench
(report-bench "删除(json-drop)"
  drop-count
  round-count
  liii-drop-ns
  njson-drop-ns
) ;report-bench
(report-variant-bench "原地修改对比(liii-set vs njson-set!)"
  set-count
  round-count
  "liii-set"
  liii-set-ns
  "njson-set!"
  njson-set!-ns
) ;report-variant-bench
(report-variant-bench "原地插入对比(liii-push vs njson-append!+drop!)"
  push-count
  round-count
  "liii-push"
  liii-push-ns
  "njson-append!+drop!"
  njson-append!-pair-ns
) ;report-variant-bench
(report-variant-bench "原地删除对比(liii-drop vs njson-set!+drop!)"
  drop-count
  round-count
  "liii-drop"
  liii-drop-ns
  "njson-set!+drop!"
  njson-drop!-pair-ns
) ;report-variant-bench
(report-bench "键存在(json-contains-key?)"
  contains-key-count
  round-count
  liii-contains-key-ns
  njson-contains-key-ns
) ;report-bench
(report-bench "获取键(json-keys)"
  keys-count
  round-count
  liii-keys-ns
  njson-keys-ns
) ;report-bench


(check-report)

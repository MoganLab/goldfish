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

#|
njson-with-json
创建“解析 + 自动释放”的作用域宏。

语法
----
(njson-with-json (var json-string) body ...)
(njson-with-json ((var1 json-string1)
                  (var2 json-string2)
                  ...)
  body ...)

功能
----
- 进入作用域前解析 JSON 字符串
- 支持一次绑定多个 JSON 字符串
- 退出作用域时自动释放句柄（异常路径也生效）
|#

(check-catch 'type-error
  (njson-with-json ((j 1))
    j))
(check-catch 'type-error
  (njson-with-json ()
    #t))

(define auto-macro-root '())
(check
  (njson-with-json ((j sample-json))
    (set! auto-macro-root j)
    (njson-ref j "active"))
  => #t)
(check-catch 'type-error (njson-ref auto-macro-root "active"))

(define auto-macro-root-multi-a '())
(define auto-macro-root-multi-b '())
(check
  (njson-with-json ((j1 sample-json)
                    (j2 "{\"env\":\"test\",\"nums\":[10,20]}"))
    (set! auto-macro-root-multi-a j1)
    (set! auto-macro-root-multi-b j2)
    (+ (njson-ref j1 "nums" 0)
       (njson-ref j2 "nums" 1)))
  => 21)
(check-catch 'type-error (njson-ref auto-macro-root-multi-a "active"))
(check-catch 'type-error (njson-ref auto-macro-root-multi-b "env"))

(define auto-macro-root-multi-on-error '())
(check-catch 'parse-error
  (njson-with-json ((j1 sample-json)
                    (j2 "{name:\"bad\"}"))
    (set! auto-macro-root-multi-on-error j1)
    #t))
(check-catch 'type-error (njson-ref auto-macro-root-multi-on-error "name"))



#|
njson-with-value
统一处理“可能是句柄也可能是标量”的作用域宏。

语法
----
(njson-with-value (var value-expr) body ...)
(njson-with-value ((var1 value-expr1)
                   (var2 value-expr2)
                   ...)
  body ...)

功能
----
- value-expr 为句柄时自动释放
- 支持一次绑定多个 value-expr
- value-expr 为标量时直接传递
|#

(check (njson-with-value ((x 7) (y 1)) (+ x y)) => 8)
(check-catch 'type-error
  (njson-with-value ()
    #t))

(define auto-macro-value-multi-a '())
(define auto-macro-value-multi-b '())
(check
  (njson-with-value ((j1 (njson-string->json sample-json))
                     (j2 (njson-string->json "{\"meta\":{\"os\":\"debian\"}}"))
                     (x 10))
    (set! auto-macro-value-multi-a j1)
    (set! auto-macro-value-multi-b j2)
    (+ x
       (njson-ref j1 "nums" 1)
       (if (string=? (njson-ref j2 "meta" "os") "debian") 1 0)))
  => 13)
(check-catch 'type-error (njson-ref auto-macro-value-multi-a "name"))
(check-catch 'type-error (njson-ref auto-macro-value-multi-b "meta" "os"))

(define auto-macro-value-multi-on-error-a '())
(define auto-macro-value-multi-on-error-b '())
(check-catch 'value-error
  (njson-with-value ((j1 (njson-string->json sample-json))
                     (j2 (njson-string->json "{\"k\":1}")))
    (set! auto-macro-value-multi-on-error-a j1)
    (set! auto-macro-value-multi-on-error-b j2)
    (value-error "boom in multi njson-with-value")))
(check-catch 'type-error (njson-ref auto-macro-value-multi-on-error-a "name"))
(check-catch 'type-error (njson-ref auto-macro-value-multi-on-error-b "k"))

(define auto-macro-meta '())
(check
  (njson-with-value ((j (njson-string->json sample-json))
                     (m (njson-ref j "meta")))
    (set! auto-macro-meta m)
    (njson-ref m "os"))
  => "linux")
(check-catch 'type-error (njson-ref auto-macro-meta "os"))

(define auto-macro-set '())
(check
  (njson-with-value ((j (njson-string->json sample-json))
                     (j2 (njson-set j "meta" "os" "debian")))
    (set! auto-macro-set j2)
    (njson-ref j2 "meta" "os"))
  => "debian")
(check-catch 'type-error (njson-ref auto-macro-set "meta" "os"))

(define auto-macro-root-on-error '())
(check-catch 'value-error
  (njson-with-json ((j sample-json))
    (set! auto-macro-root-on-error j)
    (value-error "boom in njson-with-value")))
(check-catch 'type-error (njson-ref auto-macro-root-on-error "name"))

(define owned-handle (njson-string->json sample-json))
(define auto-owned '())
(check
  (njson-with-value ((j owned-handle))
    (set! auto-owned j)
    (njson-ref j "version"))
  => "17.11.26")
(check-catch 'type-error (njson-ref auto-owned "version"))

#|
njson-string->json / njson?
验证解析与句柄谓词行为。
|#

(njson-with-json ((root sample-json))
  (check-true (njson? root)))
(check-catch 'parse-error (njson-string->json "{name:\"Goldfish\"}"))

#|
njson-ref
验证标量读取、多级路径读取与类型错误。
|#

(njson-with-json ((root sample-json))
  (check (njson-ref root "name") => "Goldfish")
  (check (njson-ref root "active") => #t)
  (check (njson-ref root "meta" "arch") => "x86_64"))
(check-catch 'type-error
  (njson-with-json ((root sample-json))
    (njson-ref root 'meta)))

#|
njson-set
验证函数式更新：新句柄变化，旧句柄保持不变。
|#

(njson-with-value ((root (njson-string->json sample-json))
                   (root2 (njson-set root "meta" "os" "debian")))
  (check (njson-ref root2 "meta" "os") => "debian")
  (check (njson-ref root "meta" "os") => "linux"))

#|
njson-push
验证数组位置插入行为。
|#

(njson-with-value ((root (njson-string->json sample-json))
                   (root3 (njson-push root "nums" 5 99)))
  (check (njson-ref root3 "nums" 5) => 99))

#|
njson-drop
验证字段删除行为与旧句柄不变。
|#

(njson-with-value ((root (njson-string->json sample-json))
                   (root4 (njson-drop root "active")))
  (check (njson-ref root4 "active") => '())
  (check (njson-ref root "active") => #t))

#|
njson-ref (子结构句柄返回)
当 ref 命中 object/array 时返回句柄，并可被自动释放。
|#

(define functional-meta '())
(njson-with-value ((root (njson-string->json sample-json))
                   (meta (njson-ref root "meta")))
  (set! functional-meta meta)
  (check-true (njson? meta))
  (check (njson-ref meta "os") => "linux"))
(check-catch 'type-error (njson-ref functional-meta "os"))

#|
njson-contains-key? / njson-keys
验证对象键查询接口。
|#

(njson-with-value ((root (njson-string->json sample-json))
                   (root4 (njson-drop root "active")))
  (check-true (njson-contains-key? root4 "meta"))
  (check-false (njson-contains-key? root4 "active"))
  (check-true (> (length (njson-keys root4)) 0)))

#|
njson-json->string / roundtrip
验证序列化与反序列化回环。
|#

(check (njson-json->string 'null) => "null")
(check-catch 'type-error (njson-json->string 'foo))

(define functional-roundtrip '())
(njson-with-value ((root (njson-string->json sample-json))
                   (root2 (njson-set root "meta" "os" "debian"))
                   (root3 (njson-push root2 "nums" 5 99))
                   (root4 (njson-drop root3 "active"))
                   (roundtrip (njson-string->json (njson-json->string root4))))
  (set! functional-roundtrip roundtrip)
  (check (njson-ref roundtrip "meta" "os") => "debian")
  (check (njson-ref roundtrip "nums" 5) => 99)
  (check (njson-ref roundtrip "active") => '()))
(check-catch 'type-error (njson-ref functional-roundtrip "meta" "os"))

#|
njson-schema-valid?
验证 JSON Schema 校验接口，包括通过、失败、非法 schema、参数类型错误和已释放句柄。

语法
----
(njson-schema-valid? schema-handle instance)

参数
----
schema-handle : njson-handle
  JSON Schema（对象形式）。
instance : njson-handle | string | number | boolean | 'null
  被校验的实例。

返回值
-----
- #t : 实例满足 schema
- #f : 实例不满足 schema
- 抛错 : 参数类型错误、schema 非法或运行时异常

功能
----
- object/array/scalar schema 场景验证
- additionalProperties / required / items 约束验证
- 非法输入与释放后句柄行为验证
|#

(define schema-object-json
  "{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"},\"age\":{\"type\":\"integer\"}},\"required\":[\"name\"],\"additionalProperties\":false}")
(define schema-instance-ok-json "{\"name\":\"Alice\",\"age\":18}")
(define schema-instance-bad-type-json "{\"name\":\"Alice\",\"age\":\"18\"}")
(define schema-instance-bad-missing-json "{\"age\":18}")
(define schema-instance-bad-extra-json "{\"name\":\"Alice\",\"city\":\"HZ\"}")
(define schema-instance-name-only-json "{\"name\":\"Alice\"}")
(define schema-instance-array-json "[1,2,3]")
(define schema-bad-handle-json "{\"type\":\"object\",\"required\":1}")
(define schema-bad-non-object-json "1")
(define schema-array-items-int-json "{\"type\":\"array\",\"items\":{\"type\":\"integer\"}}")
(define schema-array-ok-json "[1,2,3]")
(define schema-array-bad-json "[1,\"2\",3]")
(define schema-scalar-int-json "{\"type\":\"integer\"}")
(define schema-scalar-null-json "{\"type\":\"null\"}")

(define (njson-schema-valid-with-json schema-json instance-json)
  (njson-with-value ((schema (njson-string->json schema-json))
                     (instance (njson-string->json instance-json)))
    (njson-schema-valid? schema instance)))

(define (njson-schema-valid-with-schema schema-json instance)
  (njson-with-value ((schema (njson-string->json schema-json)))
    (njson-schema-valid? schema instance)))

(define (schema-invalid? thunk)
  (let ((result (catch 'schema-error thunk (lambda args 'schema-error))))
    (or (eq? result 'schema-error)
        (eq? result #f))))
(check-true (njson-schema-valid-with-json schema-object-json schema-instance-ok-json))
(check-false (njson-schema-valid-with-json schema-object-json schema-instance-bad-type-json))
(check-false (njson-schema-valid-with-json schema-object-json schema-instance-bad-missing-json))
(check-false (njson-schema-valid-with-json schema-object-json schema-instance-bad-extra-json))
(check-true (njson-schema-valid-with-json schema-object-json schema-instance-name-only-json))
(check-false (njson-schema-valid-with-json schema-object-json schema-instance-array-json))
(check-true (njson-schema-valid-with-json schema-array-items-int-json schema-array-ok-json))
(check-false (njson-schema-valid-with-json schema-array-items-int-json schema-array-bad-json))
(check-true (njson-schema-valid-with-schema schema-scalar-int-json 18))
(check-false (njson-schema-valid-with-schema schema-scalar-int-json "18"))
(check-true (njson-schema-valid-with-schema schema-scalar-null-json 'null))
(check-false (njson-schema-valid-with-schema schema-scalar-null-json 0))
(check-true (schema-invalid? (lambda () (njson-schema-valid-with-json schema-bad-handle-json schema-instance-ok-json))))
(check-true (schema-invalid? (lambda () (njson-schema-valid-with-json schema-bad-non-object-json schema-instance-ok-json))))
(check-catch 'type-error
  (njson-with-value ((instance (njson-string->json schema-instance-ok-json)))
    (njson-schema-valid? 'foo instance)))
(check-catch 'type-error (njson-schema-valid-with-schema schema-object-json 'foo))
(check-catch 'type-error (njson-schema-valid-with-schema schema-object-json '(1 2 3)))
(define schema-handle-for-freed-check (njson-string->json schema-object-json))
(define freed-instance-handle (njson-string->json "{\"name\":\"Bob\"}"))
(check-true (njson-free freed-instance-handle))
(check-catch 'type-error (njson-schema-valid? schema-handle-for-freed-check freed-instance-handle))
(check-true (njson-free schema-handle-for-freed-check))

(define sample-json-scm (ljson-string->json sample-json))

#|
性能基准 (liii json vs njson)
对解析、序列化、读取、修改、插入、删除、键存在、取键进行中位数对比。

统计口径
--------
- 每项基准多轮执行，取中位数（median）
- 先进行 warmup，降低首轮偏差
- 输出倍率为 liii-json / njson

说明
----
该段基准仍保留手动 free，用于控制基准逻辑与资源生命周期，避免将宏层开销混入对比数据。
|#

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

(check-true (>= liii-parse-ns 0))
(check-true (>= njson-parse-ns 0))
(check-true (>= liii-stringify-ns 0))
(check-true (>= njson-stringify-ns 0))
(check-true (>= liii-ref-ns 0))
(check-true (>= njson-ref-ns 0))
(check-true (>= liii-set-ns 0))
(check-true (>= njson-set-ns 0))
(check-true (>= liii-push-ns 0))
(check-true (>= njson-push-ns 0))
(check-true (>= liii-drop-ns 0))
(check-true (>= njson-drop-ns 0))
(check-true (>= liii-contains-key-ns 0))
(check-true (>= njson-contains-key-ns 0))
(check-true (>= liii-keys-ns 0))
(check-true (>= njson-keys-ns 0))

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


(check-report)

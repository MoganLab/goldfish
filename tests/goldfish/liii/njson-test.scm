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
        (liii error)
        (liii njson))


(define sample-json
  "{\"name\":\"Goldfish\",\"version\":\"17.11.26\",\"active\":true,\"score\":3.14,\"nums\":[1,2,3,4,5],\"meta\":{\"arch\":\"x86_64\",\"os\":\"linux\"}}")

(define (string-list-contains? s xs)
  (cond ((null? xs) #f)
        ((string=? s (car xs)) #t)
        (else (string-list-contains? s (cdr xs)))))

#|
let-njson
统一处理“可能是句柄也可能是标量”的作用域宏。

语法
----
(let-njson (var value-expr) body ...)
(let-njson ((var1 value-expr1)
                   (var2 value-expr2)
                   ...)
  body ...)

参数
----
var : symbol
  绑定名。
value-expr : any
  待绑定表达式；当结果为 njson-handle 时进入自动释放流程。
body ... : expression
  在绑定作用域内执行的表达式序列。

返回值
-----
- body 最后一个表达式的返回值
- 抛错 : 绑定语法非法（如空绑定列表）

功能
----
- value-expr 为句柄时自动释放
- 支持一次绑定多个 value-expr
- value-expr 为标量时直接传递
|#

(check-catch 'type-error
  (let-njson ((j (njson-string->json 1)))
    j))

(define auto-macro-root '())
(check
  (let-njson ((j (njson-string->json sample-json)))
    (set! auto-macro-root j)
    (njson-ref j "active"))
  => #t)
(check-catch 'type-error (njson-ref auto-macro-root "active"))

(define auto-macro-root-multi-a '())
(define auto-macro-root-multi-b '())
(check
  (let-njson ((j1 (njson-string->json sample-json))
              (j2 (njson-string->json "{\"env\":\"test\",\"nums\":[10,20]}")))
    (set! auto-macro-root-multi-a j1)
    (set! auto-macro-root-multi-b j2)
    (+ (njson-ref j1 "nums" 0)
       (njson-ref j2 "nums" 1)))
  => 21)
(check-catch 'type-error (njson-ref auto-macro-root-multi-a "active"))
(check-catch 'type-error (njson-ref auto-macro-root-multi-b "env"))

(define auto-macro-root-multi-on-error '())
(check-catch 'parse-error
  (let-njson ((j1 (njson-string->json sample-json))
              (j2 (njson-string->json "{name:\"bad\"}")))
    (set! auto-macro-root-multi-on-error j1)
    #t))
(check-catch 'type-error (njson-ref auto-macro-root-multi-on-error "name"))





(check (let-njson ((x 7) (y 1)) (+ x y)) => 8)
(check-catch 'type-error
  (let-njson ()
    #t))

(define auto-macro-value-multi-a '())
(define auto-macro-value-multi-b '())
(check
  (let-njson ((j1 (njson-string->json sample-json))
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
  (let-njson ((j1 (njson-string->json sample-json))
                     (j2 (njson-string->json "{\"k\":1}")))
    (set! auto-macro-value-multi-on-error-a j1)
    (set! auto-macro-value-multi-on-error-b j2)
    (value-error "boom in multi let-njson")))
(check-catch 'type-error (njson-ref auto-macro-value-multi-on-error-a "name"))
(check-catch 'type-error (njson-ref auto-macro-value-multi-on-error-b "k"))

(define auto-macro-meta '())
(check
  (let-njson ((j (njson-string->json sample-json))
                     (m (njson-ref j "meta")))
    (set! auto-macro-meta m)
    (njson-ref m "os"))
  => "linux")
(check-catch 'type-error (njson-ref auto-macro-meta "os"))

(define auto-macro-set '())
(check
  (let-njson ((j (njson-string->json sample-json))
                     (j2 (njson-set j "meta" "os" "debian")))
    (set! auto-macro-set j2)
    (njson-ref j2 "meta" "os"))
  => "debian")
(check-catch 'type-error (njson-ref auto-macro-set "meta" "os"))

(define auto-macro-root-on-error '())
(check-catch 'value-error
  (let-njson ((j (njson-string->json sample-json)))
    (set! auto-macro-root-on-error j)
    (value-error "boom in let-njson")))
(check-catch 'type-error (njson-ref auto-macro-root-on-error "name"))

(define owned-handle (njson-string->json sample-json))
(define auto-owned '())
(check
  (let-njson ((j owned-handle))
    (set! auto-owned j)
    (njson-ref j "version"))
  => "17.11.26")
(check-catch 'type-error (njson-ref auto-owned "version"))

#|
njson-string->json
将 JSON 字符串解析为 njson-handle。

语法
----
(njson-string->json json-string)

参数
----
json-string : string
  严格 JSON 文本。

返回值
-----
- njson-handle : 解析成功
- 抛错 : parse-error（语法非法）/ type-error（参数类型非法）

功能
----
- 用于从文本入口构建 njson 句柄
- 与 let-njson 配合可自动管理句柄生命周期
|#

(let-njson ((root (njson-string->json sample-json)))
  (check (njson-ref root "name") => "Goldfish"))
(check-catch 'parse-error (njson-string->json "{name:\"Goldfish\"}"))
(check-catch 'type-error (njson-string->json 1))

#|
njson?
判断值是否为 njson-handle。

语法
----
(njson? x)

参数
----
x : any
  任意 Scheme 值。

返回值
-----
- #t : x 是 njson-handle
- #f : x 不是 njson-handle

功能
----
- 句柄类型判定
- 常用于 API 调用前的防御式校验
|#

(let-njson ((root (njson-string->json sample-json)))
  (check-true (njson? root)))
(check-false (njson? 'foo))
(check-false (njson? 1))

#|
njson-ref
验证标量读取、多级路径读取与类型错误。

语法
----
(njson-ref json key)
(njson-ref json k1 k2 ... kn)

参数
----
json : njson-handle
  待读取的 JSON 句柄。
key / k1..kn : string | integer
  路径 token。object 层使用 string，array 层使用 integer。

返回值
-----
- 标量值 : string | number | boolean | 'null
- njson-handle : 命中 object/array 子结构
- () : 路径不存在
- 抛错 : 参数类型错误或路径 token 非法

功能
----
- 支持对象与数组的多级路径读取
- 命中子 object/array 时返回句柄，可继续链式访问
|#

(let-njson ((root (njson-string->json sample-json)))
  (check (njson-ref root "name") => "Goldfish")
  (check (njson-ref root "active") => #t)
  (check (njson-ref root "meta" "arch") => "x86_64"))
(check-catch 'type-error
  (let-njson ((root (njson-string->json sample-json)))
    (njson-ref root 'meta)))

(define functional-meta '())
(let-njson ((root (njson-string->json sample-json))
                   (meta (njson-ref root "meta")))
  (set! functional-meta meta)
  (check-true (njson? meta))
  (check (njson-ref meta "os") => "linux"))
(check-catch 'type-error (njson-ref functional-meta "os"))

#|
njson-set
函数式更新：返回新句柄，不修改原句柄。

语法
----
(njson-set json key ... value)

参数
----
json : njson-handle
  待更新的 JSON 句柄。
key ... : string | integer
  路径 token，可为多层路径。
value : njson-handle | string | number | boolean | 'null
  写入值。

返回值
-----
- njson-handle : 返回更新后的新句柄
- 抛错 : 参数类型错误或路径非法

功能
----
- 保持函数式语义（原句柄不变）
- 支持对象字段与数组位置更新
|#

(let-njson ((root (njson-string->json sample-json))
                   (root2 (njson-set root "meta" "os" "debian")))
  (check (njson-ref root2 "meta" "os") => "debian")
  (check (njson-ref root "meta" "os") => "linux"))
(check-catch 'type-error (njson-set 'foo "meta" "os" "debian"))

#|
njson-set!
原地更新：直接修改输入句柄。

语法
----
(njson-set! json key ... value)

参数
----
json : njson-handle
  待原地更新的 JSON 句柄。
key ... : string | integer
  路径 token，可为多层路径。
value : njson-handle | string | number | boolean | 'null
  写入值。

返回值
-----
- njson-handle : 与输入同一逻辑句柄（已更新）
- 抛错 : 参数类型错误或路径非法

功能
----
- 避免整棵复制，适合性能敏感路径
- 更新后可直接继续在同一句柄上读取
|#

(let-njson ((root (njson-string->json sample-json)))
  (check-true (njson? (njson-set! root "meta" "os" "debian")))
  (check (njson-ref root "meta" "os") => "debian"))
(check-catch 'type-error (njson-set! 'foo "meta" "os" "debian"))

#|
njson-push
函数式插入：返回新句柄，原句柄不变。

语法
----
(njson-push json key ... value)

参数
----
json : njson-handle
  待插入的 JSON 句柄。
key ... : string | integer
  路径 token，末级通常指向对象键或数组位置。
value : njson-handle | string | number | boolean | 'null
  插入值。

返回值
-----
- njson-handle : 返回插入后的新句柄
- 抛错 : 参数类型错误或路径非法

功能
----
- 对对象可新增键
- 对数组可按位置插入元素
|#

(let-njson ((root (njson-string->json sample-json))
                   (root3 (njson-push root "nums" 5 99)))
  (check (njson-ref root3 "nums" 5) => 99))
(check-catch 'type-error (njson-push 'foo "nums" 0 99))

#|
njson-push!
原地插入：直接修改输入句柄。

语法
----
(njson-push! json key ... value)

参数
----
json : njson-handle
  待原地插入的 JSON 句柄。
key ... : string | integer
  路径 token，末级通常指向对象键或数组位置。
value : njson-handle | string | number | boolean | 'null
  插入值。

返回值
-----
- njson-handle : 与输入同一逻辑句柄（已更新）
- 抛错 : 参数类型错误或路径非法

功能
----
- 减少复制与分配成本
- 适合批量构造或热路径更新
|#

(let-njson ((root (njson-string->json sample-json)))
  (njson-push! root "nums" 5 99)
  (check (njson-ref root "nums" 5) => 99))
(check-catch 'type-error (njson-push! 'foo "nums" 0 99))

#|
njson-drop
函数式删除：返回新句柄，原句柄不变。

语法
----
(njson-drop json key ...)

参数
----
json : njson-handle
  待删除的 JSON 句柄。
key ... : string | integer
  路径 token，定位待删除目标。

返回值
-----
- njson-handle : 返回删除后的新句柄
- 抛错 : 参数类型错误或路径非法

功能
----
- 删除对象字段或数组元素
- 保持函数式语义（原句柄可继续使用）
|#

(let-njson ((root (njson-string->json sample-json))
                   (root4 (njson-drop root "active")))
  (check (njson-ref root4 "active") => '())
  (check (njson-ref root "active") => #t))
(check-catch 'type-error (njson-drop 'foo "active"))

#|
njson-drop!
原地删除：直接修改输入句柄。

语法
----
(njson-drop! json key ...)

参数
----
json : njson-handle
  待原地删除的 JSON 句柄。
key ... : string | integer
  路径 token，定位待删除目标。

返回值
-----
- njson-handle : 与输入同一逻辑句柄（已更新）
- 抛错 : 参数类型错误或路径非法

功能
----
- 删除对象字段或数组元素
- 适合性能敏感的可变更新场景
|#

(let-njson ((root (njson-string->json sample-json)))
  (njson-drop! root "active")
  (check (njson-ref root "active") => '()))
(check-catch 'type-error (njson-drop! 'foo "active"))

#|
njson-contains-key?
检查对象是否包含指定键。

语法
----
(njson-contains-key? json key)

参数
----
json : njson-handle
  待检查的 JSON 句柄（应为对象）。
key : string
  待查询的键名。

返回值
-----
- #t : 键存在
- #f : 键不存在或目标不是对象
- 抛错 : 参数类型错误

功能
----
- 用于对象键存在性判断
- 常与 njson-ref 搭配进行防御式读取
|#

(let-njson ((root (njson-string->json sample-json)))
  (check-true (njson-contains-key? root "meta"))
  (check-false (njson-contains-key? root "not-found")))
(check-catch 'type-error (njson-contains-key? 'foo "meta"))

#|
njson-keys
获取对象所有键名列表。

语法
----
(njson-keys json)

参数
----
json : njson-handle
  待读取键集合的 JSON 句柄。

返回值
-----
- (list string ...) : 对象键列表
- '() : 目标不是对象或对象为空
- 抛错 : 参数类型错误

功能
----
- 提供对象字段枚举能力
- 支持 keys 缓存与写后失效、读时重建
|#

(let-njson ((root (njson-string->json sample-json)))
  (check-true (> (length (njson-keys root)) 0))
  (check-true (string-list-contains? "active" (njson-keys root)))
  (njson-drop! root "active")
  (check-false (string-list-contains? "active" (njson-keys root)))
  (njson-push! root "active" #t)
  (check-true (string-list-contains? "active" (njson-keys root)))
  (njson-set! root "name" "Goldfish++")
  (check-true (string-list-contains? "active" (njson-keys root)))
  (njson-push! root "new-key" 1)
  (check-true (string-list-contains? "new-key" (njson-keys root))))
(let-njson ((root (njson-string->json sample-json)))
  (njson-keys root)
  (njson-drop! root "active")
  (njson-push! root "lazy-key" 1)
  (njson-set! root "name" "Goldfish-Lazy")
  (let ((keys (njson-keys root)))
    (check-false (string-list-contains? "active" keys))
    (check-true (string-list-contains? "lazy-key" keys))
    (check-true (string-list-contains? "name" keys)))
  ;; Second read should remain consistent.
  (let ((keys2 (njson-keys root)))
    (check-false (string-list-contains? "active" keys2))
    (check-true (string-list-contains? "lazy-key" keys2))
    (check-true (string-list-contains? "name" keys2))))
(check-catch 'type-error (njson-keys 'foo))

#|
njson-json->string
将 njson-handle 或标量值序列化为 JSON 字符串。

语法
----
(njson-json->string value)

参数
----
value : njson-handle | string | number | boolean | 'null
  待序列化的输入值。

返回值
-----
- string : 合法 JSON 文本
- 抛错 : 参数类型错误

功能
----
- 支持句柄与标量统一序列化
- 可与 njson-string->json 组合完成回环验证
|#

(check (njson-json->string 'null) => "null")
(check-catch 'type-error (njson-json->string 'foo))

(define functional-roundtrip '())
(let-njson ((root (njson-string->json sample-json))
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
  (let-njson ((schema (njson-string->json schema-json))
                     (instance (njson-string->json instance-json)))
    (njson-schema-valid? schema instance)))

(define (njson-schema-valid-with-schema schema-json instance)
  (let-njson ((schema (njson-string->json schema-json)))
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
  (let-njson ((instance (njson-string->json schema-instance-ok-json)))
    (njson-schema-valid? 'foo instance)))
(check-catch 'type-error (njson-schema-valid-with-schema schema-object-json 'foo))
(check-catch 'type-error (njson-schema-valid-with-schema schema-object-json '(1 2 3)))
(define schema-handle-for-freed-check (njson-string->json schema-object-json))
(define freed-instance-handle (njson-string->json "{\"name\":\"Bob\"}"))
(check-true (njson-free freed-instance-handle))
(check-catch 'type-error (njson-schema-valid? schema-handle-for-freed-check freed-instance-handle))
(check-true (njson-free schema-handle-for-freed-check))


(check-report)

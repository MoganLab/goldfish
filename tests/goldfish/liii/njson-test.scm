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
        (liii path)
        (liii njson)
        (rename (liii json)
                (string->json ljson-string->json)
                (json-object? ljson-object?)
                (json-ref ljson-ref)))


(define sample-json
  "{\"name\":\"Goldfish\",\"version\":\"17.11.26\",\"active\":true,\"score\":3.14,\"nums\":[1,2,3,4,5],\"meta\":{\"arch\":\"x86_64\",\"os\":\"linux\"}}")

(define (string-list-contains? s xs)
  (cond ((null? xs) #f)
        ((string=? s (car xs)) #t)
        (else (string-list-contains? s (cdr xs)))))

(define (capture-key-error-message thunk)
  (catch 'key-error
    thunk
    (lambda args
      (let ((payload (if (and (pair? args) (pair? (cdr args))) (cadr args) '())))
        (if (and (pair? payload) (string? (car payload)))
            (car payload)
            "")))))

#|
let-njson
统一处理“可能是 njson 句柄，也可能是普通标量”的作用域宏。

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
  待绑定表达式；可返回 njson-handle 或普通值。
body ... : expression
  在绑定作用域内执行的表达式序列。

行为逻辑
--------
1. 先求值 `value-expr` 并绑定到 `var`。
2. 若结果是 njson-handle，则在退出作用域时自动调用 `njson-free` 释放。
3. 若结果不是句柄，则按普通 `let` 语义传递，不做释放。
4. 支持多绑定；每个绑定独立判定是否需要自动释放。

返回值
-----
- 返回 `body` 最后一个表达式的值。
- 若 `body` 内抛错，错误会继续向上传播；已绑定的句柄仍会在离开作用域时清理。

错误
----
- `type-error`：绑定语法非法（例如空绑定列表、不是 `(var expr)` 或 `((var expr) ...)` 结构）。
|#

(check-catch 'type-error
  (let-njson ((j (string->njson 1)))
    j))

(define auto-macro-root '())
(check
  (let-njson ((j (string->njson sample-json)))
    (set! auto-macro-root j)
    (njson-ref j "active"))
  => #t)
(check-catch 'type-error (njson-ref auto-macro-root "active"))

(define auto-macro-root-multi-a '())
(define auto-macro-root-multi-b '())
(check
  (let-njson ((j1 (string->njson sample-json))
              (j2 (string->njson "{\"env\":\"test\",\"nums\":[10,20]}")))
    (set! auto-macro-root-multi-a j1)
    (set! auto-macro-root-multi-b j2)
    (+ (njson-ref j1 "nums" 0)
       (njson-ref j2 "nums" 1)))
  => 21)
(check-catch 'type-error (njson-ref auto-macro-root-multi-a "active"))
(check-catch 'type-error (njson-ref auto-macro-root-multi-b "env"))

(define auto-macro-root-multi-on-error '())
(check-catch 'parse-error
  (let-njson ((j1 (string->njson sample-json))
              (j2 (string->njson "{name:\"bad\"}")))
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
  (let-njson ((j1 (string->njson sample-json))
                     (j2 (string->njson "{\"meta\":{\"os\":\"debian\"}}"))
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
  (let-njson ((j1 (string->njson sample-json))
                     (j2 (string->njson "{\"k\":1}")))
    (set! auto-macro-value-multi-on-error-a j1)
    (set! auto-macro-value-multi-on-error-b j2)
    (value-error "boom in multi let-njson")))
(check-catch 'type-error (njson-ref auto-macro-value-multi-on-error-a "name"))
(check-catch 'type-error (njson-ref auto-macro-value-multi-on-error-b "k"))

(define auto-macro-meta '())
(check
  (let-njson ((j (string->njson sample-json))
                     (m (njson-ref j "meta")))
    (set! auto-macro-meta m)
    (njson-ref m "os"))
  => "linux")
(check-catch 'type-error (njson-ref auto-macro-meta "os"))

(define auto-macro-set '())
(check
  (let-njson ((j (string->njson sample-json))
                     (j2 (njson-set j "meta" "os" "debian")))
    (set! auto-macro-set j2)
    (njson-ref j2 "meta" "os"))
  => "debian")
(check-catch 'type-error (njson-ref auto-macro-set "meta" "os"))

(define auto-macro-root-on-error '())
(check-catch 'value-error
  (let-njson ((j (string->njson sample-json)))
    (set! auto-macro-root-on-error j)
    (value-error "boom in let-njson")))
(check-catch 'type-error (njson-ref auto-macro-root-on-error "name"))

(define owned-handle (string->njson sample-json))
(define auto-owned '())
(check
  (let-njson ((j owned-handle))
    (set! auto-owned j)
    (njson-ref j "version"))
  => "17.11.26")
(check-catch 'type-error (njson-ref auto-owned "version"))

#|
string->njson
将 JSON 字符串解析为 njson-handle。

语法
----
(string->njson json-string)

参数
----
json-string : string
  严格 JSON 文本。

行为逻辑
--------
1. 校验 `json-string` 必须为字符串。
2. 使用 nlohmann-json 解析文本。
3. 解析结果存入 njson 句柄池并返回句柄。

返回值
-----
- `njson-handle`：解析成功时返回的句柄；可用于后续 `njson-ref/set/...`。

错误
----
- `type-error`：`json-string` 不是字符串。
- `parse-error`：字符串不是合法 JSON。
|#

(let-njson ((root (string->njson sample-json)))
  (check (njson-ref root "name") => "Goldfish"))
(check-catch 'parse-error (string->njson "{name:\"Goldfish\"}"))
(check-catch 'type-error (string->njson 1))

(define njson-string-free-check (string->njson "{\"x\":1}"))
(check-true (njson-free njson-string-free-check))
(check-catch 'type-error (njson-ref njson-string-free-check "x"))
(check-catch 'type-error (njson-free 'foo))

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

行为逻辑
--------
1. 仅检查结构是否符合 njson 句柄格式（`(njson-handle . id)`）。
2. 不负责判断该句柄是否已释放；“已释放”通常在具体 API 调用时触发错误。

返回值
-----
- `#t`：`x` 结构上是 njson 句柄。
- `#f`：`x` 不是 njson 句柄。

错误
----
- 无（该谓词本身不抛错）。
|#

(let-njson ((root (string->njson sample-json)))
  (check-true (njson? root)))
(check-false (njson? 'foo))
(check-false (njson? 1))

#|
njson-null?/object?/array?/string?/number?/integer?/boolean?
统一类型谓词接口，支持 njson-handle 与 JSON 标量输入。

语法
----
(njson-null? x)
(njson-object? x)
(njson-array? x)
(njson-string? x)
(njson-number? x)
(njson-integer? x)
(njson-boolean? x)

行为逻辑
--------
1. 若 `x` 不是句柄，则按 Scheme 标量语义直接判定。
2. 若 `x` 是句柄，则读取句柄对应 JSON 值并判定其底层类型。
3. 对已释放句柄会在访问句柄池时报错。

返回值
-----
- #t / #f : 是否匹配目标 JSON 类型

错误
----
- `type-error`：`x` 形似句柄但非法，或句柄已释放。
|#

(let-njson ((object-h (string->njson "{\"k\":1}"))
            (array-h (string->njson "[1,2]"))
            (string-h (string->njson "\"s\""))
            (number-h (string->njson "3.14"))
            (integer-h (string->njson "7"))
            (boolean-h (string->njson "true"))
            (null-h (string->njson "null")))
  (check-true (njson-object? object-h))
  (check-true (njson-array? array-h))
  (check-true (njson-string? string-h))
  (check-true (njson-number? number-h))
  (check-true (njson-number? integer-h))
  (check-true (njson-integer? integer-h))
  (check-true (njson-boolean? boolean-h))
  (check-true (njson-null? null-h))
  (check-false (njson-array? object-h))
  (check-false (njson-object? null-h))
  (check-false (njson-integer? number-h)))

(check-true (njson-string? "hello"))
(check-true (njson-number? 3.14))
(check-true (njson-integer? 7))
(check-true (njson-boolean? #t))
(check-true (njson-null? 'null))
(check-false (njson-null? 'foo))
(check-false (njson-object? "x"))
(check-false (njson-array? #(1 2 3)))
(check-false (njson-string? 'foo))
(check-false (njson-number? 'foo))
(check-false (njson-integer? 3.14))
(check-false (njson-boolean? 1))

(define njson-predicate-freed (string->njson "{\"k\":1}"))
(check-true (njson-free njson-predicate-freed))
(check-catch 'type-error (njson-object? njson-predicate-freed))

#|
njson-ref
读取 JSON 中指定路径的值（支持 object/array 多级路径）。

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

行为逻辑
--------
1. 从 `json` 根开始，按路径逐层下钻。
2. object 层要求 token 为字符串；array 层要求 token 为非负整数。
3. 路径中任一层不存在时抛 `key-error`。
4. 命中 object/array 子结构时返回新的 njson-handle；命中标量返回标量。

返回值
-----
- 标量值 : string | number | boolean | 'null
- njson-handle : 命中 object/array 子结构

错误
----
- `type-error`：`json` 非句柄或句柄已释放。
- `key-error`：路径 token 类型与当前层不匹配、路径结构非法、缺少路径参数、路径不存在。
|#

(let-njson ((root (string->njson sample-json)))
  (check (njson-ref root "name") => "Goldfish")
  (check (njson-ref root "active") => #t)
  (check (njson-ref root "meta" "arch") => "x86_64"))
(check-catch 'key-error
  (let-njson ((root (string->njson sample-json)))
    (njson-ref root 'meta)))
(let-njson ((root (string->njson sample-json)))
  (check (catch 'key-error (lambda () (njson-ref root "not-found")) (lambda args 'key-error)) => 'key-error)
  (check (catch 'key-error (lambda () (njson-ref root "nums" 999)) (lambda args 'key-error)) => 'key-error)
  (check (catch 'key-error (lambda () (njson-ref root "name" "x")) (lambda args 'key-error)) => 'key-error))

(define functional-meta '())
(let-njson ((root (string->njson sample-json))
                   (meta (njson-ref root "meta")))
  (set! functional-meta meta)
  (check-true (njson? meta))
  (check (njson-ref meta "os") => "linux"))
(check-catch 'type-error (njson-ref functional-meta "os"))

#|
njson-set
函数式更新（upsert）：返回新句柄，不修改原句柄。

语法
----
(njson-set json key ... value)

参数
----
json : njson-handle
  待更新的 JSON 句柄。
key ... : string | integer
  路径 token，可为多层路径；最后一个 token 表示写入位置。
value : njson-handle | string | number | boolean | 'null
  写入值。

行为逻辑
--------
1. 复制输入句柄对应 JSON，保证函数式语义。
2. 定位到目标父节点后写入末级 token。
3. 中间路径必须存在；任一层不存在时抛 `key-error`。
4. object：若键存在则覆盖，若不存在则新建（upsert）。
5. array：`idx < size` 覆盖，`idx >= size` 抛错。

返回值
-----
- `njson-handle`：包含更新结果的新句柄（与输入不是同一逻辑对象）。

错误
----
- `type-error`：`json` 非句柄、句柄已释放、value 类型非法。
- `key-error`：路径 token 非法、路径结构非法、中间路径不存在、参数个数不合法、数组索引越界（`idx >= size`）。
|#

(let-njson ((root (string->njson sample-json))
                   (root2 (njson-set root "meta" "os" "debian"))
                   (root3 (njson-set root "city" "HZ"))
                   (root4 (njson-set root "nums" 4 99)))
  (check (njson-ref root2 "meta" "os") => "debian")
  (check (njson-ref root "meta" "os") => "linux")
  (check (njson-ref root3 "city") => "HZ")
  (check-false (njson-contains-key? root "city"))
  (check (njson-ref root4 "nums" 4) => 99)
  (check (njson-size (njson-ref root "nums")) => 5))

(let-njson ((root (string->njson sample-json))
            (root-idx-update (njson-set root "nums" 1 200))
            (meta (njson-ref root "meta"))
            (root-handle-value (njson-set root "meta-copy" meta)))
  (check (njson-ref root-idx-update "nums" 1) => 200)
  (check (njson-ref root-handle-value "meta-copy" "os") => "linux")
  (check-false (njson-contains-key? root "meta-copy")))

(check-catch 'type-error (njson-set 'foo "meta" "os" "debian"))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set root 'meta "os" "debian")))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set root "nums" 5 1)))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set root "nums" 999 1)))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set root "meta" "missing" "k" 1))
  (check (capture-key-error-message (lambda () (njson-set root "meta" "missing" "k" 1)))
         => "g_njson-set: path not found: missing object key 'missing'"))
(let-njson ((root (string->njson sample-json)))
  (check (capture-key-error-message (lambda () (njson-set root "nums" 5 1)))
         => "g_njson-set: array index out of range (index=5, size=5)"))

#|
njson-set!
原地更新（upsert）：直接修改输入句柄。

语法
----
(njson-set! json key ... value)

参数
----
json : njson-handle
  待原地更新的 JSON 句柄。
key ... : string | integer
  路径 token，可为多层路径；最后一个 token 表示写入位置。
value : njson-handle | string | number | boolean | 'null
  写入值。

行为逻辑
--------
1. 直接在原句柄对应 JSON 上更新，不做整棵复制。
2. 中间路径必须存在；任一层不存在时抛 `key-error`。
3. object：存在则覆盖，不存在则新建（upsert）。
4. array：`idx < size` 覆盖，`idx >= size` 抛错。
5. 更新成功后同句柄继续可读，`njson-keys` 缓存会自动失效并在下次读取重建。

返回值
-----
- `njson-handle`：返回原句柄本身（已更新）。

错误
----
- `type-error`：`json` 非句柄、句柄已释放、value 类型非法。
- `key-error`：路径 token 非法、路径结构非法、中间路径不存在、参数个数不合法、数组索引越界（`idx >= size`）。
|#

(let-njson ((root (string->njson sample-json)))
  (check-true (njson? (njson-set! root "meta" "os" "debian")))
  (njson-set! root "city" "HZ")
  (njson-set! root "nums" 4 99)
  (check (njson-ref root "meta" "os") => "debian")
  (check (njson-ref root "city") => "HZ")
  (check (njson-ref root "nums" 4) => 99))

(let-njson ((root (string->njson sample-json))
            (meta (njson-ref root "meta")))
  (njson-set! root "meta-copy" meta)
  (check (njson-ref root "meta-copy" "arch") => "x86_64")
  (check-false (njson-contains-key? meta "missing")))

(check-catch 'type-error (njson-set! 'foo "meta" "os" "debian"))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set! root 'meta "os" "debian")))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set! root "nums" 5 1)))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set! root "meta" "missing" "k" 1))
  (check (capture-key-error-message (lambda () (njson-set! root "meta" "missing" "k" 1)))
         => "g_njson-set!: path not found: missing object key 'missing'"))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-set! root "nums" 999 1)))

#|
njson-append
数组追加（函数式）：返回新句柄，原句柄不变。

语法
----
(njson-append json value)
(njson-append json k1 k2 ... kn value)

参数
----
json : njson-handle
  待追加的 JSON 句柄。
k1..kn : string | integer（可选）
  指向目标数组的路径；省略时目标为根。
value : njson-handle | string | number | boolean | 'null
  追加值。

行为逻辑
--------
1. 复制输入句柄对应 JSON，保证函数式语义。
2. 若提供路径，先定位目标路径；路径不存在时抛 `key-error`。
3. 目标必须是 array，否则抛 `key-error`。
4. 在数组末尾追加 `value`。

返回值
-----
- `njson-handle`：追加后的新句柄。

错误
----
- `type-error`：`json` 非句柄、句柄已释放、value 类型非法。
- `key-error`：缺少 value、路径非法、路径不存在、目标不是数组。
|#

(let-njson ((root (string->njson sample-json))
            (root2 (njson-append root "nums" 99)))
  (check (njson-ref root2 "nums" 5) => 99)
  (check (njson-size (njson-ref root "nums")) => 5))

(let-njson ((arr (string->njson "[1,2]"))
            (arr2 (njson-append arr 3)))
  (check (njson-ref arr2 2) => 3)
  (check (njson-size arr) => 2))

(check-catch 'type-error (njson-append 'foo 1))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-append root))
  (check-catch 'key-error (njson-append root "as"))
  (check (capture-key-error-message (lambda () (njson-append root "as")))
         => "g_njson-append: append target must be array")
  (check-catch 'key-error (njson-append root "nums"))
  (check (capture-key-error-message (lambda () (njson-append root "nums")))
         => "g_njson-append: append target must be array")
  (check-catch 'key-error (njson-append root "name" 1))
  (check (capture-key-error-message (lambda () (njson-append root "name" 1)))
         => "g_njson-append: append target must be array"))

#|
njson-append!
数组追加（原地）：直接修改输入句柄。

语法
----
(njson-append! json value)
(njson-append! json k1 k2 ... kn value)

参数
----
json : njson-handle
  待原地追加的 JSON 句柄。
k1..kn : string | integer（可选）
  指向目标数组的路径；省略时目标为根。
value : njson-handle | string | number | boolean | 'null
  追加值。

行为逻辑
--------
1. 在输入句柄上原地更新，不做整棵复制。
2. 若提供路径，先定位目标路径；路径不存在时抛 `key-error`。
3. 目标必须是 array，否则抛 `key-error`。
4. 在数组末尾追加 `value`，返回同一个句柄。

返回值
-----
- `njson-handle`：输入句柄本身（已更新）。

错误
----
- `type-error`：`json` 非句柄、句柄已释放、value 类型非法。
- `key-error`：缺少 value、路径非法、路径不存在、目标不是数组。
|#

(let-njson ((root (string->njson sample-json)))
  (check-true (njson? (njson-append! root "nums" 99)))
  (check (njson-ref root "nums" 5) => 99))

(let-njson ((arr (string->njson "[1,2]")))
  (njson-append! arr 3)
  (check (njson-ref arr 2) => 3))

(check-catch 'type-error (njson-append! 'foo 1))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-append! root))
  (check-catch 'key-error (njson-append! root "as"))
  (check (capture-key-error-message (lambda () (njson-append! root "as")))
         => "g_njson-append!: append target must be array")
  (check-catch 'key-error (njson-append! root "nums"))
  (check (capture-key-error-message (lambda () (njson-append! root "nums")))
         => "g_njson-append!: append target must be array")
  (check-catch 'key-error (njson-append! root "name" 1))
  (check (capture-key-error-message (lambda () (njson-append! root "name" 1)))
         => "g_njson-append!: append target must be array"))

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

行为逻辑
--------
1. 复制输入句柄对应 JSON，确保原句柄不被修改。
2. 按路径逐层定位到待删除目标的父节点。
3. object 层使用 string 键删除字段；字段不存在时抛出 `key-error`。
4. array 层使用非负 integer 索引删除元素；索引越界时抛出 `key-error`。
5. 删除后返回新句柄；原句柄仍可继续读取原值。

返回值
-----
- `njson-handle`：删除结果对应的新句柄（与输入句柄不同）。

错误
----
- `type-error`：`json` 非句柄或句柄已释放。
- `key-error`：路径 token 类型与当前层不匹配、路径结构非法、缺少路径参数，或删除目标不存在。
|#

(let-njson ((root (string->njson sample-json))
                   (root4 (njson-drop root "active")))
  (check-false (njson-contains-key? root4 "active"))
  (check (njson-ref root "active") => #t))

(let-njson ((arr (string->njson "[10,20,30]"))
            (arr2 (njson-drop arr 1)))
  (check (njson-ref arr2 0) => 10)
  (check (njson-ref arr2 1) => 30)
  (check (njson-size arr2) => 2)
  (check (njson-ref arr 1) => 20))

(check-catch 'type-error (njson-drop 'foo "active"))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-drop root 'active))
  (check-catch 'key-error (njson-drop root "not-found"))
  (check-catch 'key-error (njson-drop root "meta" "not-found"))
  (check-catch 'key-error (njson-drop root "name" "as" "as"))
  (check (capture-key-error-message (lambda () (njson-drop root "not-found")))
         => "g_njson-drop: path not found: missing object key 'not-found'")
  (check (capture-key-error-message (lambda () (njson-drop root "name" "as" "as")))
         => "g_njson-drop: path not found: missing object key 'as'"))
(let-njson ((arr (string->njson "[10,20,30]")))
  (check-catch 'key-error (njson-drop arr 3))
  (check (capture-key-error-message (lambda () (njson-drop arr 3)))
         => "g_njson-drop: path not found: array index out of range (index=3, size=3)"))

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

行为逻辑
--------
1. 在输入句柄对应 JSON 上直接执行删除，不做整棵复制。
2. object 层按字符串键删除字段；array 层按非负整数索引删除元素。
3. 删除目标不存在时抛出 `key-error`。
4. 删除成功后继续返回并复用同一输入句柄。
5. 若目标是对象，`njson-keys` 缓存会标记失效并在下次读取时重建。

返回值
-----
- `njson-handle`：输入句柄本身（已更新）。

错误
----
- `type-error`：`json` 非句柄或句柄已释放。
- `key-error`：路径 token 类型与当前层不匹配、路径结构非法、缺少路径参数，或删除目标不存在。
|#

(let-njson ((root (string->njson sample-json)))
  (njson-drop! root "active")
  (check-false (njson-contains-key? root "active")))

(let-njson ((arr (string->njson "[10,20,30]")))
  (njson-drop! arr 1)
  (check (njson-ref arr 0) => 10)
  (check (njson-ref arr 1) => 30)
  (check (njson-size arr) => 2))

(check-catch 'type-error (njson-drop! 'foo "active"))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-drop! root 'active))
  (check-catch 'key-error (njson-drop! root "not-found"))
  (check-catch 'key-error (njson-drop! root "meta" "not-found"))
  (check (capture-key-error-message (lambda () (njson-drop! root "meta" "not-found")))
         => "g_njson-drop!: path not found: missing object key 'not-found'"))
(let-njson ((arr (string->njson "[10,20,30]")))
  (check-catch 'key-error (njson-drop! arr 3))
  (check (capture-key-error-message (lambda () (njson-drop! arr 3)))
         => "g_njson-drop!: path not found: array index out of range (index=3, size=3)"))

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

行为逻辑
--------
1. 校验 `json` 为可用句柄且 `key` 为字符串。
2. 若句柄指向对象，则检查该键是否存在。
3. 若句柄不是对象（array/scalar/null），直接返回 `#f`，不抛错。

返回值
-----
- `#t`：目标为对象且包含 `key`。
- `#f`：目标不是对象，或对象中不存在该键。

错误
----
- `type-error`：`json` 非句柄或句柄已释放。
- `key-error`：`key` 非字符串（路径键非法）。
|#

(let-njson ((root (string->njson sample-json)))
  (check-true (njson-contains-key? root "meta"))
  (check-false (njson-contains-key? root "not-found")))

(let-njson ((arr (string->njson "[1,2]"))
            (scalar (string->njson "1")))
  (check-false (njson-contains-key? arr "0"))
  (check-false (njson-contains-key? scalar "x")))

(check-catch 'type-error (njson-contains-key? 'foo "meta"))
(let-njson ((root (string->njson sample-json)))
  (check-catch 'key-error (njson-contains-key? root 1)))
(let-njson ((root (string->njson sample-json)))
  (check (capture-key-error-message (lambda () (njson-contains-key? root 1)))
         => "g_njson-contains-key?: json object key must be string?"))

(define njson-contains-freed (string->njson "{\"k\":1}"))
(check-true (njson-free njson-contains-freed))
(check-catch 'type-error (njson-contains-key? njson-contains-freed "k"))

#|
njson-size / njson-empty?
获取 JSON 容器大小并判断是否为空。

语法
----
(njson-size json)
(njson-empty? json)

参数
----
json : njson-handle
  待查询的 JSON 句柄。

行为逻辑
--------
1. `njson-size` 仅对 object/array 返回成员数；其余类型统一视为 0。
2. `njson-empty?` 以“容器是否有元素”判定空性。
3. 对 scalar/null，按非容器语义处理：`njson-size => 0`，`njson-empty? => #t`。
4. 句柄已释放或句柄非法时立即抛错。

返回值
-----
- njson-size : integer
  object/array 返回元素个数；其他类型返回 0。
- njson-empty? : boolean
  object/array 按成员数判空；其他类型返回 #t。

错误
----
- `type-error`：`json` 非句柄或句柄已释放。
|#

(let-njson ((root (string->njson sample-json))
            (arr (string->njson "[1,2,3]"))
            (empty-obj (string->njson "{}"))
            (empty-arr (string->njson "[]"))
            (scalar (string->njson "3.14")))
  (check (njson-size root) => 6)
  (check-false (njson-empty? root))
  (check (njson-size arr) => 3)
  (check-false (njson-empty? arr))
  (check (njson-size empty-obj) => 0)
  (check-true (njson-empty? empty-obj))
  (check (njson-size empty-arr) => 0)
  (check-true (njson-empty? empty-arr))
  (check (njson-size scalar) => 0)
  (check-true (njson-empty? scalar)))

(check-catch 'type-error (njson-size 'foo))
(check-catch 'type-error (njson-empty? 'foo))

(define njson-size-freed (string->njson "{\"k\":1}"))
(check-true (njson-free njson-size-freed))
(check-catch 'type-error (njson-size njson-size-freed))
(check-catch 'type-error (njson-empty? njson-size-freed))

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

行为逻辑
--------
1. 若目标为对象，则返回其所有键名（字符串列表）。
2. 若目标不是对象（array/scalar/null），返回空表 `()`。
3. 内部使用键缓存：首次读取构建缓存；对象被 `njson-set!`/`njson-drop!` 修改后标记失效。
4. 缓存失效后下一次 `njson-keys` 调用会按当前对象内容重建。

返回值
-----
- (list string ...) : 对象键列表
- '() : 目标不是对象或对象为空

错误
----
- `type-error`：`json` 非句柄或句柄已释放。
|#

(let-njson ((root (string->njson sample-json)))
  (check-true (> (length (njson-keys root)) 0))
  (check-true (string-list-contains? "active" (njson-keys root)))
  (njson-drop! root "active")
  (check-false (string-list-contains? "active" (njson-keys root)))
  (njson-set! root "active" #t)
  (check-true (string-list-contains? "active" (njson-keys root)))
  (njson-set! root "name" "Goldfish++")
  (check-true (string-list-contains? "active" (njson-keys root)))
  (njson-set! root "new-key" 1)
  (check-true (string-list-contains? "new-key" (njson-keys root))))
(let-njson ((root (string->njson sample-json)))
  (njson-keys root)
  (njson-drop! root "active")
  (njson-set! root "lazy-key" 1)
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

(let-njson ((arr (string->njson "[1,2]"))
            (scalar (string->njson "1"))
            (empty-obj (string->njson "{}")))
  (check (njson-keys arr) => '())
  (check (njson-keys scalar) => '())
  (check (njson-keys empty-obj) => '()))

(check-catch 'type-error (njson-keys 'foo))

(define njson-keys-freed (string->njson "{\"k\":1}"))
(check-true (njson-free njson-keys-freed))
(check-catch 'type-error (njson-keys njson-keys-freed))

#|
file->njson / njson->file
在文件与 njson 之间读写 JSON 文本。

语法
----
(file->njson path)
(njson->file path value)

参数
----
path : string
  文件路径。
value : njson-handle 
  待写入 JSON 值（也可为 strict json scalar）。

行为逻辑
--------
1. `file->njson`：读取 `path` 文本并按严格 JSON 解析，成功后返回新句柄。
2. `njson->file`：先把 `value` 转成 JSON，再以 pretty 格式写入 `path`。
3. `njson->file` 对对象键会按底层序列化规则输出（当前测试断言为字典序输出）。
4. `njson->file` 支持写入 scalar（如 `'null`），后续可通过 `file->njson` 回读验证。

返回值
-----
- file->njson : njson-handle
- njson->file : integer（写入字节数）

错误
----
- `type-error`：`path` 类型错误，或 `value` 不是可序列化 JSON 值。
- `io-error`：文件读写失败（例如路径不可访问）。
- `parse-error`：`file->njson` 读取到的文件内容不是合法 JSON。
|#

(define njson-io-path
  (string-append "/tmp/goldfish-njson-io-" (number->string (g_monotonic-nanosecond)) ".json"))

(let-njson ((root (string->njson sample-json)))
  (check-true (> (njson->file njson-io-path root) 0))
  (let-njson ((loaded (file->njson njson-io-path)))
    (check (njson->string loaded) => (njson->string root))))

(let-njson ((compact (string->njson "{\"b\":1,\"a\":2}")))
  (check-true (> (njson->file njson-io-path compact) 0))
  (check (path-read-text njson-io-path)
         => "{\n  \"a\": 2,\n  \"b\": 1\n}"))

(check-true (> (njson->file njson-io-path 'null) 0))
(let-njson ((loaded-null (file->njson njson-io-path)))
  (check-true (njson-null? loaded-null)))

(path-write-text njson-io-path "{bad:1}")
(check-catch 'parse-error (file->njson njson-io-path))

(check-catch 'type-error (file->njson 1))
(check-catch 'type-error (njson->file 1 'null))
(check-catch 'type-error (njson->file njson-io-path 'foo))

(define njson-io-freed (string->njson "{\"k\":1}"))
(check-true (njson-free njson-io-freed))
(check-catch 'type-error (njson->file njson-io-path njson-io-freed))

#|
njson->string
将 njson-handle 或标量值序列化为 JSON 字符串。

语法
----
(njson->string value)

参数
----
value : njson-handle | string | number | boolean | 'null
  待序列化的输入值。

行为逻辑
--------
1. 若 `value` 是句柄，则读取句柄对应 JSON 并生成紧凑字符串。
2. 若 `value` 是 strict json scalar，则直接按 JSON 语义编码。
3. 输出是可被 `string->njson` 再次解析的合法 JSON 文本（用于回环）。

返回值
-----
- string : 紧凑 JSON 文本

错误
----
- `type-error`：`value` 非句柄且不属于支持的 strict json scalar。
|#

(check (njson->string 'null) => "null")
(check (njson->string "x") => "\"x\"")
(check (njson->string #f) => "false")
(let-njson ((root (string->njson "{\"b\":1,\"a\":2}")))
  (check (njson->string root) => "{\"a\":2,\"b\":1}"))
(check-catch 'type-error (njson->string 'foo))

(define njson-string-freed (string->njson "{\"k\":1}"))
(check-true (njson-free njson-string-freed))
(check-catch 'type-error (njson->string njson-string-freed))

#|
njson-format-string
将 JSON 字符串格式化为可读的多行文本。

语法
----
(njson-format-string json-string [indent])

参数
----
json-string : string
  待格式化的严格 JSON 文本。
indent : integer（可选，默认 2）
  缩进空格数，需 >= 0。

行为逻辑
--------
1. 先解析 `json-string`；解析成功后再进行 pretty 输出。
2. `indent` 未传时默认 2；传入时必须是非负整数。
3. 对对象/数组生成多行缩进文本；对纯标量（如 `"1"`）保持单行。
4. 该 API 只格式化字符串，不返回句柄。

返回值
-----
- string : 格式化后的 JSON 文本

错误
----
- `parse-error`：`json-string` 不是合法 JSON。
- `type-error`：`json-string` 不是字符串，或 `indent` 不是整数。
- `value-error`：`indent < 0`，或参数个数不合法。
|#

(check (njson-format-string "{\"b\":1,\"a\":{\"k\":2}}")
       => "{\n  \"a\": {\n    \"k\": 2\n  },\n  \"b\": 1\n}")
(check (njson-format-string "[1,2,3]" 4)
       => "[\n    1,\n    2,\n    3\n]")
(check (njson-format-string "{\"a\":1}" 0)
       => "{\n\"a\": 1\n}")
(check (njson-format-string "1") => "1")

(check-catch 'parse-error (njson-format-string "{name:1}"))
(check-catch 'type-error (njson-format-string 1))
(check-catch 'type-error (njson-format-string "{}" "2"))
(check-catch 'value-error (njson-format-string "{}" -1))
(check-catch 'value-error (njson-format-string "{}" 2 4))

(define functional-roundtrip '())
(let-njson ((root (string->njson sample-json))
                   (root2 (njson-set root "meta" "os" "debian"))
                   (root3 (njson-append root2 "nums" 99))
                   (root4 (njson-drop root3 "active"))
                   (roundtrip (string->njson (njson->string root4))))
  (set! functional-roundtrip roundtrip)
  (check (njson-ref roundtrip "meta" "os") => "debian")
  (check (njson-ref roundtrip "nums" 5) => 99)
  (check-false (njson-contains-key? roundtrip "active")))
(check-catch 'type-error (njson-ref functional-roundtrip "meta" "os"))

#|
json->njson / njson->json
在 liii json 与 njson 之间做双向转换。

语法
----
(json->njson value)
(njson->json value)

参数
----
value : any
  - json->njson: liii json 值（object/array）或 strict json scalar
  - njson->json: njson-handle 或 strict json scalar

行为逻辑
--------
1. `json->njson`：把 `(liii json)` 的值树转换为 njson 存储，并返回句柄。
2. `njson->json`：把句柄或 strict scalar 转回 `(liii json)` 可处理的值。
3. 对 strict scalar（如 `7`、`'null`）两侧都支持直通转换。
4. 对已释放句柄调用 `njson->json` 会因句柄不可用而报错。

返回值
-----
- json->njson : njson-handle
- njson->json : liii json 值

错误
----
- `type-error`：输入类型不在支持范围内，或句柄已释放。
|#

(define ljson-bridge-sample (ljson-string->json sample-json))
(let-njson ((bridge-handle (json->njson ljson-bridge-sample)))
  (check (njson-ref bridge-handle "name") => "Goldfish")
  (check (njson-ref bridge-handle "nums" 2) => 3))

(let-njson ((bridge-handle (string->njson sample-json)))
  (let ((ljson-val (njson->json bridge-handle)))
    (check-true (ljson-object? ljson-val))
    (check (ljson-ref ljson-val "name") => "Goldfish")
    (check (ljson-ref ljson-val "active") => #t)
    (check (ljson-ref ljson-val "nums" 1) => 2)))

(check (njson->json 'null) => 'null)
(check (njson->json 7) => 7)

(let-njson ((null-handle (json->njson 'null)))
  (check-true (njson-null? null-handle)))

(let-njson ((njson-str (json->njson "abc"))
            (njson-bool (json->njson #f)))
  (check-true (njson-string? njson-str))
  (check (njson->string njson-str) => "\"abc\"")
  (check (njson->string njson-bool) => "false")
  (check-true (njson-boolean? njson-bool)))

(check (njson->json "abc") => "abc")
(check (njson->json #f) => #f)

(check-catch 'type-error (json->njson 'foo))
(check-catch 'type-error (njson->json 'foo))

(define njson->json-freed (string->njson "{\"a\":1}"))
(check-true (njson-free njson->json-freed))
(check-catch 'type-error (njson->json njson->json-freed))

#|
njson-schema-report
返回结构化校验报告，便于调用方定位失败路径并提取错误消息。

语法
----
(njson-schema-report schema-handle instance)

参数
----
schema-handle : njson-handle
  JSON Schema（对象形式）。
instance : njson-handle | string | number | boolean | 'null
  被校验的实例。

行为逻辑
--------
1. 校验 `schema-handle` 为可用句柄，且其根值必须是 object schema。
2. 将 `instance` 规范化为可校验 JSON（支持句柄和 strict scalar）。
3. 调用底层 JSON Schema 校验器执行校验。
4. 无论通过或失败，都会返回统一报告结构；失败明细进入 `errors` 列表。
5. 每条错误包含失败路径、错误描述和触发失败的实例片段字符串。
6. 对非法 schema（结构不符合规范）不会返回报告，而是直接抛 `schema-error`。

返回值
-----
- hash-table : 校验报告（成功与失败都会返回）
  顶层字段：
  - 'valid? : boolean
    #t 表示通过，#f 表示不通过
  - 'error-count : integer
    错误条数
  - 'errors : list
    错误列表，每项是 hash-table，字段如下：
    - 'instance-path : string
      失败位置（JSON Pointer）；根节点失败时通常为空字符串 `""`
    - 'message : string
      失败原因描述
    - 'instance : string
      触发失败的实例片段（JSON dump）

错误
----
- `type-error`：`schema-handle` 非句柄、句柄已释放，或 `instance` 类型不支持。
- `schema-error`：schema 本身非法（例如 keyword 类型不符合规范，或 schema 根不是对象）。
- 其他运行时错误：底层校验器异常时向上抛出（测试中主要覆盖 `schema-error`）。
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
(define schema-default-count-json
  "{\"type\":\"object\",\"properties\":{\"count\":{\"type\":\"integer\",\"default\":7}}}")
(define schema-empty-object-json "{}")

(define (njson-schema-report-with-json schema-json instance-json)
  (let-njson ((schema (string->njson schema-json))
                     (instance (string->njson instance-json)))
    (njson-schema-report schema instance)))

(define (njson-schema-report-with-schema schema-json instance)
  (let-njson ((schema (string->njson schema-json)))
    (njson-schema-report schema instance)))

(define (run-schema-report mode schema-input instance-input)
  (if (eq? mode 'json)
      (njson-schema-report-with-json schema-input instance-input)
      (njson-schema-report-with-schema schema-input instance-input)))

(define (check-schema-report-shape report expected-valid expected-error-count)
  (check (hash-table-ref report 'valid?) => expected-valid)
  (check (hash-table-ref report 'error-count) => expected-error-count)
  (check (length (hash-table-ref report 'errors)) => expected-error-count))

(define (check-schema-report-error error-entry expected-path expected-message expected-instance)
  (check (hash-table-ref error-entry 'instance-path) => expected-path)
  (check (hash-table-ref error-entry 'message) => expected-message)
  (check (hash-table-ref error-entry 'instance) => expected-instance))

(define (check-schema-report-invalid-min-errors report min-error-count)
  (check (hash-table-ref report 'valid?) => #f)
  (check-true (>= (hash-table-ref report 'error-count) min-error-count))
  (check-true (>= (length (hash-table-ref report 'errors)) min-error-count)))

(define (run-schema-report-shape-case case)
  (let* ((mode (list-ref case 0))
         (schema-input (list-ref case 1))
         (instance-input (list-ref case 2))
         (expected-valid (list-ref case 3))
         (expected-error-count (list-ref case 4))
         (report (run-schema-report mode schema-input instance-input)))
    (check-schema-report-shape report expected-valid expected-error-count)
    report))

(define (run-schema-report-error-case case)
  (let* ((schema-json (list-ref case 0))
         (instance-json (list-ref case 1))
         (expected-path (list-ref case 2))
         (expected-message (list-ref case 3))
         (expected-instance (list-ref case 4))
         (report (run-schema-report-shape-case (list 'json schema-json instance-json #f 1)))
         (error-entry (car (hash-table-ref report 'errors))))
    (check-schema-report-error error-entry expected-path expected-message expected-instance)))

(define schema-report-shape-cases
  (list
    (list 'json schema-object-json schema-instance-ok-json #t 0)
    (list 'json schema-object-json schema-instance-name-only-json #t 0)
    (list 'json schema-array-items-int-json schema-array-ok-json #t 0)
    (list 'schema schema-scalar-int-json 18 #t 0)
    (list 'schema schema-scalar-null-json 'null #t 0)
    (list 'json schema-default-count-json schema-empty-object-json #t 0)
    (list 'schema schema-scalar-int-json "18" #f 1)
    (list 'schema schema-scalar-null-json 0 #f 1)))

(define schema-report-error-cases
  (list
    (list schema-object-json schema-instance-bad-type-json
          "/age"
          "unexpected instance type"
          "\"18\"")
    (list schema-object-json schema-instance-bad-missing-json
          ""
          "required property 'name' not found in object"
          "{\"age\":18}")
    (list schema-object-json schema-instance-bad-extra-json
          ""
          "validation failed for additional property 'city': instance invalid as per false-schema"
          "{\"city\":\"HZ\",\"name\":\"Alice\"}")
    (list schema-array-items-int-json schema-array-bad-json
          "/1"
          "unexpected instance type"
          "\"2\"")))

(for-each run-schema-report-shape-case schema-report-shape-cases)
(for-each run-schema-report-error-case schema-report-error-cases)

(let ((instance-array-report (njson-schema-report-with-json schema-object-json schema-instance-array-json)))
  (check-schema-report-invalid-min-errors instance-array-report 1))

(check-catch 'type-error
  (let-njson ((instance (string->njson schema-instance-ok-json)))
    (njson-schema-report 'foo instance)))
(check-catch 'type-error (njson-schema-report-with-schema schema-object-json 'foo))
(check-catch 'schema-error (njson-schema-report-with-json schema-bad-handle-json schema-instance-ok-json))
(check-catch 'schema-error (njson-schema-report-with-json schema-bad-non-object-json schema-instance-ok-json))

(define schema-handle-for-freed-check (string->njson schema-object-json))
(define freed-instance-handle (string->njson "{\"name\":\"Bob\"}"))
(check-true (njson-free freed-instance-handle))
(check-catch 'type-error (njson-schema-report schema-handle-for-freed-check freed-instance-handle))
(check-true (njson-free schema-handle-for-freed-check))

(define freed-schema-handle (string->njson schema-object-json))
(check-true (njson-free freed-schema-handle))
(check-catch 'type-error (njson-schema-report freed-schema-handle 1))


(check-report)

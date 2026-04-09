;
; Copyright (C) 2025 The Goldfish Scheme Authors
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
        (liii hash-table)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

(define (generator tokens)
  (let ((stream tokens))
    (lambda ()
      (if (null? stream)
        (values #f #f)
        (let ((base-token (car stream)))
          (set! stream (cdr stream))
          (values #f base-token)
        ) ;let
      ) ;if
    ) ;lambda
  ) ;let
) ;define

;; simple parser

(define simple-parser
  (packrat-parser expr
    (expr ((a <- 'num) a)
          ((a <- 'id) a)
    ) ;expr
  ) ;packrat-parser
) ;define
(check-true (procedure? simple-parser))

(let* ((gen-num (generator '((num . 123))))
       (r-num (simple-parser (base-generator->results gen-num))))
  (check-true (parse-result-successful? r-num))
  (check (parse-result-semantic-value r-num) => 123)
) ;let*

(let* ((gen-id (generator '((id . foo))))
       (r-id (simple-parser (base-generator->results gen-id))))
  (check-true (parse-result-successful? r-id))
  (check (parse-result-semantic-value r-id) => 'foo)
) ;let*

(let* ((gen-invalid (generator '((foo . bar))))
       (r-invalid (simple-parser (base-generator->results gen-invalid))))
  (check-false (parse-result-successful? r-invalid))
) ;let*

;; calc

(define calc-env (make-hash-table))
(define calc
  (packrat-parser expr
    (expr (('begin body <- exprs 'end) body)
          ((var <- 'id ':= val <- expr) (hash-table-set! calc-env var val))
          ((a <- mulexp '+ b <- expr) (+ a b))
          ((a <- mulexp '- b <- expr) (- a b))
          ((a <- mulexp) a)
    ) ;expr
    (mulexp ((a <- powexp '* b <- mulexp) (* a b))
            ((a <- powexp '/ b <- mulexp) (/ a b))
            ((a <- powexp) a)
    ) ;mulexp
    (powexp ((a <- simple '^ b <- powexp) (expt a b))
            ((a <- simple) a)
    ) ;powexp
    (simple ((a <- 'num) a)
            ((a <- 'id) (calc-env a))
            (('oparen a <- expr 'cparen) a)
    ) ;simple
    (exprs ((a <- expr rest <- exprs) rest)
           ((a <- expr) a)
    ) ;exprs
  ) ;packrat-parser
) ;define
(check-true (procedure? calc))

(let* ((g (generator '((num . 2) (+) (num . 3))))
       (expected (+ 2 3))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

;; NOTE: the `calc` parser is right recursion;
;;       packrat hates left recursion
(let* ((g (generator '((num . 1) (-) (num . 2) (+) (num . 3))))
       (expected (- 1 (+ 2 3)))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

;; ditto
(let* ((g (generator '((num . 1) (*) (num . 2) (/) (num . 3))))
       (expected (* 1 (/ 2 3)))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let*
  ((g
     (generator '((oparen) (num . 2) (+) (num . 3) (cparen)
                  (*) (num . 4))
     ) ;generator
   ) ;g
   (expected (* (+ 2 3) 4))
   (r (calc (base-generator->results g)))
  ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g (generator '((num . 2) (^) (num . 3))))
       (expected (expt 2 3))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g (generator '((num . 8) (/) (num . 2))))
       (expected (/ 8 2))
       (r (calc (base-generator->results g))))
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let*
  ((g
     (generator
       '((begin) (id . ans) (:=) (num . 42)
                 (oparen) (num . 2) (+) (id . ans) (cparen)
                 (^) (num . 3)
         (end))
     ) ;generator
   ) ;g
   (expected (begin (define ans 42)
                    (expt (+ 2 ans)
                          3)
                    ) ;expt
   ) ;expected
   (r (calc (base-generator->results g)))
  ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let*
  ((g
     (generator '((oparen) (num . 2) (+) (num . 3) (cparen)
                  (^)
                  (oparen) (num . 1) (+) (num . 1) (cparen))
     ) ;generator
   ) ;g
   (expected (expt (+ 2 3) (+ 1 1)))
   (r (calc (base-generator->results g)))
  ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let*
  ((g
     (generator '((begin) (id . a) (:=) (num . 10)
                  (id . b) (:=) (num . 20)
                  (id . a) (*) (id . b)
                  (end))
     ) ;generator
   ) ;g
   (expected (begin (define a 10) (define b 20) (* a b)))
   (r (calc (base-generator->results g)))
  ) ;
  (check-true (parse-result-successful? r))
  (check (parse-result-semantic-value r) => expected)
) ;let*
(hash-table-clear! calc-env)

(let* ((g-invalid (generator '((begin) (foo . bar) (end))))
       (r-invalid (calc (base-generator->results g-invalid))))
  (check-false (parse-result-successful? r-invalid))
) ;let*
(hash-table-clear! calc-env)

#|
make-result
构造表示成功解析的 parse-result 对象

语法
----
(make-result semantic-value next-parse-results)

参数
----
semantic-value : any
作为语义值使用的对象
next-results : parse-results
表示继续解析位置的 parse-results 对象

返回值
-----
parse-result
表示成功的解析结果

|#

(let ()
  (define success (make-result 42 #f))
  (check-true (parse-result? success))
) ;let

#|
parse-result?
判断对象是否为 parse-result 对象

语法
----
(parse-result? obj)

参数
----
obj : any
待判断的对象

返回值
-----
bool
若为 parse-result 对象则返回 #t，否则返回 #f

|#

(let ()
  (check-true (parse-result? (make-result 42 #f)))
  (check-false (parse-result? 42))
) ;let

#|
parse-result-successful?
判断解析结果是否表示成功解析

语法
----
(parse-result-successful? result)

参数
----
result : parse-result
待判断的 parse-result 对象

返回值
-----
bool
若为成功解析则返回 #t，否则返回 #f

|#

(let ()
  (define success (make-result 42 #f))
  (check-true (parse-result-successful? success))
) ;let

#|
parse-result-semantic-value
获取成功解析的语义值

语法
----
(parse-result-semantic-value result)

参数
----
result: parse-result

返回值
-----
any
若解析成功则返回成功解析的语义值，否则返回 #f

|#

(let ()
  (define success (make-result 42 #f))
  (check (parse-result-semantic-value success) => 42)
) ;let

#|
make-expected-result
构造表示期望失败的 parse-result 对象

语法
----
(make-expected-result position expected)

参数
----
position : parse-position
解析位置
expected : any
期望的对象

返回值
-----
parse-result
表示失败的解析结果

|#

(let ()
  (define fail (make-expected-result (make-parse-position #f 1 0) "num"))
  (check-false (parse-result-successful? fail))
) ;let

#|
make-message-result
构造表示带有错误消息的 parse-result 对象

语法
----
(make-message-result position message)

参数
----
position : parse-position
解析位置
message : string
错误消息字符串

返回值
-----
parse-result
表示带有错误消息的失败解析结果

|#

(let ()
  (define pos (make-parse-position "test.scm" 1 5))
  (define message (make-message-result pos "error"))
  (check-false (parse-result-successful? message))
) ;let

#|
parse-result-next
获取解析后剩余输入流的位置信息

语法
----
(parse-result-next result)

参数
----
result : parse-result
待查询的 parse-result 对象

返回值
-----
parse-results or #f
后续输入流的 parse-results 对象，或 #f 表示输入结束

|#

(let ()
  (define success (make-result 42 #f))
  (check (parse-result-next success) => #f)
) ;let

#|
create-parse-position
构造解析位置对象

语法
----
(make-parse-position filename line column)

参数
----
filename : string or #f
文件名
column : number
列号（从 0 开始）
line : number
行号（从 1 开始）

返回值
-----
parse-position
表示文件中的位置信息

|#

(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check-true (parse-position? pos))
) ;let

#|
parse-position?
判断对象是否为解析位置记录

语法
----
(parse-position? obj)

参数
----
obj : any
待判断的对象

返回值
-----
bool
#t 若为 parse-position 对象，#f 否则

|#

(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check-true (parse-position? pos))
) ;let

#|
parse-position-file
获取解析位置关联的文件名

语法
----
(parse-position-file position)

参数
----
position : parse-position
parse-position 对象

返回值
-----
string or #f
关联的文件名，或 #f 表示文件名未知

|#

(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check (parse-position-file pos) => "test.scm")
) ;let

#|
parse-position-line
获取解析位置的行号

语法
----
(parse-position-line position)

参数
----
position : parse-position
parse-position 对象

返回值
-----
number
行号（从 1 开始）

|#

(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check (parse-position-line pos) => 3)
) ;let

#|
parse-position-column
获取解析位置的列号

语法
----
(parse-position-column position)

参数
----
position : parse-position
parse-position 对象

返回值
-----
number
列号（从 0 开始）

|#

(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check (parse-position-column pos) => 15)
) ;let

#|
base-generator->results
将基础 token 生成器转换为 parse-results 对象

语法
----
(base-generator->results generator)

参数
----
generator : procedure
基础 token 生成函数，应返回两个值：parse-position 或 #f，以及 token 对或 #f

返回值
-----
parse-results
表示从生成器读取的解析结果

|#

(let ()
  (define gen (lambda () (values (make-parse-position "test" 1 0) #f)))
  (define results (base-generator->results gen))
  (check-true (parse-results? results))
) ;let

#|
parse-results?
判断对象是否为 parse-results 记录

语法
----
(parse-results? obj)

参数
----
obj : any
待判断的对象

返回值
-----
bool
若为 parse-results 对象则返回 #t，否则返回 #f

|#

(let ()
  (define gen (let ((tokens '((num . 100) (id . x))))
                (lambda ()
                  (if (null? tokens)
                      (values #f #f)
                      (let ((token (car tokens)))
                        (set! tokens (cdr tokens))
                        (values #f token))
                      ) ;let
                  ) ;if
                ) ;lambda
  ) ;define
  (define results (base-generator->results gen))
  (check (parse-results-token-kind results) => 'num)
) ;let

#|
parse-results-token-kind
获取基础 token 的类型标识符

语法
----
(parse-results-token-kind results)

参数
----
results : parse-results
parse-results 对象

返回值
-----
kind-object or #f
token 类型标识符，或 #f 表示输入结束

|#

(let ()
  (define gen (let ((tokens '((num . 100))))
                (lambda ()
                  (if (null? tokens)
                      (values #f #f)
                      (let ((token (car tokens)))
                        (set! tokens (cdr tokens))
                        (values #f token))
                      ) ;let
                  ) ;if
                ) ;lambda
  ) ;define
  (define results (base-generator->results gen))
  (check (parse-results-token-kind results) => 'num)
) ;let

#|
parse-results-token-value
获取基础 token 的语义值

语法
----
(parse-results-token-value results)

参数
----
results : parse-results
parse-results 对象

返回值
-----
value-object or #f
token 的语义值，或 #f 表示输入结束

|#

(let ()
  (define gen (let ((tokens '((num . 100))))
                (lambda ()
                  (if (null? tokens)
                      (values #f #f)
                      (let ((token (car tokens)))
                        (set! tokens (cdr tokens))
                        (values #f token))
                      ) ;let
                  ) ;if
                ) ;lambda
  ) ;define
  (define results (base-generator->results gen))
  (check (parse-results-token-value results) => 100)
) ;let

#|
make-error-expected
构造期望类型的解析错误对象

语法
----
(make-error-expected position expected)

参数
----
position : parse-position
解析位置
expected : any
期望的对象

返回值
-----
parse-error
表示期望错误的解析错误对象

|#

(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (define error-ex (make-error-expected pos "open-paren"))
  (check-true (parse-error? error-ex))
) ;let

#|
make-error-message
构造带有错误消息的解析错误对象

语法
----
(make-error-message position message)

参数
----
position : parse-position
解析位置
message : string
错误消息字符串

返回值
-----
parse-error
表示带有错误消息的解析错误对象

|#

(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (define error-msg (make-error-message pos "syntax error"))
  (check-true (parse-error? error-msg))
) ;let

#|
parse-error?
判断对象是否为解析错误记录

语法
----
(parse-error? obj)

参数
----
obj : any
待判断的对象

返回值
-----
bool
若为 parse-error 对象则返回 #t，否则返回 #f

|#

(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (check-true (parse-error? (make-error-expected pos "test")))
) ;let

#|
parse-error-position
获取解析错误的位置信息

语法
----
(parse-error-position error)

参数
----
error : parse-error
parse-error 对象

返回值
-----
parse-position or #f
解析位置的 parse-position 对象，或 #f 表示未知位置

|#

(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (define error-ex (make-error-expected pos "open-paren"))
  (check (parse-error-position error-ex) => pos)
) ;let

#|
packrat-check-base
构造匹配指定类型 token 的基础 combinator

语法
----
(packrat-check-base kind acceptor)

参数
----
kind : kind-object
token 类型标识符
acceptor : procedure
语义值接受器函数，接收 token 语义值并返回 combinator

返回值
-----
procedure
token 匹配 combinator 函数

|#

(let ((gen (generator '((num . 42)))))
  (define %parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
  (define result (%parse-num (base-generator->results gen)))
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 42)
) ;let

#|
packrat-or
构造尝试多个可选解析方案的 combinator

语法
----
(packrat-or comb1 comb2)

参数
----
comb1 : procedure
第一个 combinator
comb2 : procedure
第二个 combinator

返回值
-----
procedure
选择 combinator 函数

|#

(let* ((gen (generator '((num . 777))))
       (%parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
       (%parse-id (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r)))))
       (%parse-or (packrat-or %parse-num %parse-id)))
  (let ((r (%parse-or (base-generator->results gen))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => 777)
  ) ;let
) ;let*

#|
packrat-check
构造变换解析结果的 combinator

语法
----
(packrat-check comb transformer)

参数
----
comb : procedure
基础 combinator
transformer : procedure
结果变换函数，接收解析结果语义值并返回 combinator

返回值
-----
procedure
变换 combinator 函数

|#

(let ((gen (generator '((num . 25)))))
  (define %parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
  (define %parse-check (packrat-check %parse-num (lambda (n) (lambda (r) (make-result (* n 2) r)))))
  (define result (%parse-check (base-generator->results gen)))
  (check-true (parse-result-successful? result))
  (check (parse-result-semantic-value result) => 50)
) ;let

#|
packrat-unless
构造排除规则 not-followed-by 的 combinator

语法
----
(packrat-unless message not-wanted fallback)

参数
----
message : string
失败时显示的消息
not-wanted : procedure
不希望的 token 匹配 combinator
fallback : procedure
备选 combinator

返回值
-----
procedure
条件 combinator 函数

|#

(let ((gen-id (generator '((id . test)))))
  (define %parse-num (packrat-check-base 'num (lambda (v) (lambda (r) (make-result v r)))))
  (define %parse-id (packrat-check-base 'id (lambda (v) (lambda (r) (make-result v r)))))
  (define %parse-unless (packrat-unless "not expected" %parse-num %parse-id))
  (let ((r (%parse-unless (base-generator->results gen-id))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => 'test)
  ) ;let
) ;let

#|
packrat-parser
创建基于 packrat 的完整解析器

语法
----
(packrat-parser result-expr nonterminal-def ...)

参数
----
result-expr : any
结果表达式，作为最终返回的解析器
nonterminal-def : list
非终结符定义列表

返回值
-----
procedure
完整解析器过程

|#

(let ()
  (define calc
    (packrat-parser expr
      (expr ((a <- mulexp '+ b <- expr) (+ a b))
            ((a <- mulexp) a)
      ) ;expr
      (mulexp ((a <- simple '* b <- mulexp) (* a b))
              ((a <- simple) a)
      ) ;mulexp
      (simple ((a <- 'num) a)
              (('oparen a <- expr 'cparen) a)
      ) ;simple
    ) ;packrat-parser
  ) ;define

  (let* ((g (generator '((num . 2) (+) (num . 3))))
         (expected (+ 2 3))
         (r (calc (base-generator->results g))))
    (check-true (parse-result-successful? r))
    (check (parse-result-semantic-value r) => expected)
  ) ;let*
) ;let

(check-report)

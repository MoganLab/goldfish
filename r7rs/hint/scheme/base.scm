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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; (hint scheme base) - (scheme base) 的类型接口
;;
;; 本库只提供类型签名数据，不导出 (scheme base) 的任何过程，
;; 也不导入其他 hint 库，因此可以被独立加载。

(define-library (hint scheme base)
  (import (scheme base))
  (export hint-scheme/base)
  (begin

    ;; -------------------- 语法形式 --------------------
    (define hint-let-values '((any? any? -> any?)))
    (define hint-let*-values '((any? any? -> any?)))
    (define hint-define-syntax '((any? any? -> any?)))
    (define hint-syntax-rules '((any? any? -> any?)))
    (define hint-syntax-error '((any? -> any?) (any? any? -> any?)))
    (define hint-let-syntax '((any? any? -> any?)))
    (define hint-letrec-syntax '((any? any? -> any?)))
    (define hint-define-values '((any? any? -> any?)))
    (define hint-define-record-type '((any? any? any? -> any?)))
    (define hint-parameterize '((any? any? -> any?)))
    (define hint-guard '((any? any? -> any?)))

    ;; -------------------- 谓词 --------------------
    (define hint-boolean? '((any? -> boolean?)))
    (define hint-not '((any? -> boolean?)))
    (define hint-boolean=?
      '((boolean? boolean? -> boolean?)
        (boolean? boolean? boolean? -> boolean?))
    ) ;define
    (define hint-symbol? '((any? -> boolean?)))
    (define hint-symbol=?
      '((symbol? symbol? -> boolean?) (symbol? symbol? symbol? -> boolean?))
    ) ;define
    (define hint-char? '((any? -> boolean?)))
    (define hint-number? '((any? -> boolean?)))
    (define hint-complex? '((any? -> boolean?)))
    (define hint-real? '((any? -> boolean?)))
    (define hint-rational? '((any? -> boolean?)))
    (define hint-integer? '((any? -> boolean?)))
    (define hint-exact? '((any? -> boolean?)))
    (define hint-inexact? '((any? -> boolean?)))
    (define hint-exact-integer? '((any? -> boolean?)))
    (define hint-zero? '((number? -> boolean?)))
    (define hint-positive? '((real? -> boolean?)))
    (define hint-negative? '((real? -> boolean?)))
    (define hint-odd? '((integer? -> boolean?)))
    (define hint-even? '((integer? -> boolean?)))
    (define hint-pair? '((any? -> boolean?)))
    (define hint-null? '((any? -> boolean?)))
    (define hint-list? '((any? -> boolean?)))
    (define hint-string? '((any? -> boolean?)))
    (define hint-vector? '((any? -> boolean?)))
    (define hint-bytevector? '((any? -> boolean?)))
    (define hint-procedure? '((any? -> boolean?)))
    (define hint-port? '((any? -> boolean?)))
    (define hint-binary-port? '((any? -> boolean?)))
    (define hint-textual-port? '((any? -> boolean?)))
    (define hint-input-port? '((any? -> boolean?)))
    (define hint-output-port? '((any? -> boolean?)))
    (define hint-input-port-open? '((input-port? -> boolean?)))
    (define hint-output-port-open? '((output-port? -> boolean?)))
    (define hint-eof-object? '((any? -> boolean?)))
    (define hint-char-ready? '((-> boolean?) (input-port? -> boolean?)))
    (define hint-u8-ready? '((-> boolean?) (input-port? -> boolean?)))
    (define hint-read-error? '((any? -> boolean?)))
    (define hint-file-error? '((any? -> boolean?)))

    ;; -------------------- 等价与比较 --------------------
    (define hint-eq? '((any? any? -> boolean?)))
    (define hint-eqv? '((any? any? -> boolean?)))
    (define hint-equal? '((any? any? -> boolean?)))
    (define hint=
      '((number? number? -> boolean?) (number? number? number? -> boolean?))
    ) ;define
    (define hint-<
      '((number? number? -> boolean?) (number? number? number? -> boolean?))
    ) ;define
    (define hint->
      '((number? number? -> boolean?) (number? number? number? -> boolean?))
    ) ;define
    (define hint-<=
      '((number? number? -> boolean?) (number? number? number? -> boolean?))
    ) ;define
    (define hint->=
      '((number? number? -> boolean?) (number? number? number? -> boolean?))
    ) ;define

    ;; -------------------- 数值运算 --------------------
    (define hint+
      '((-> number?)
        (number? -> number?)
        (number? number? -> number?)
        (number? number? number? -> number?))
    ) ;define
    (define hint-
      '((number? -> number?)
        (number? number? -> number?)
        (number? number? number? -> number?))
    ) ;define
    (define hint*
      '((-> number?)
        (number? -> number?)
        (number? number? -> number?)
        (number? number? number? -> number?))
    ) ;define
    (define hint/
      '((number? -> number?)
        (number? number? -> number?)
        (number? number? number? -> number?))
    ) ;define
    (define hint-abs '((real? -> real?)))
    (define hint-square '((number? -> number?)))
    (define hint-expt '((number? number? -> number?)))
    (define hint-exact '((number? -> integer?)))
    (define hint-inexact '((number? -> number?)))
    (define hint-max
      '((real? -> real?) (real? real? -> real?) (real? real? real? -> real?))
    ) ;define
    (define hint-min
      '((real? -> real?) (real? real? -> real?) (real? real? real? -> real?))
    ) ;define
    (define hint-floor '((real? -> integer?)))
    (define hint-floor/ '((integer? integer? -> any?)))
    (define hint-floor-quotient '((integer? integer? -> integer?)))
    (define hint-floor-remainder '((integer? integer? -> integer?)))
    (define hint-ceiling '((real? -> integer?)))
    (define hint-truncate '((real? -> integer?)))
    (define hint-truncate/ '((integer? integer? -> any?)))
    (define hint-truncate-quotient '((integer? integer? -> integer?)))
    (define hint-truncate-remainder '((integer? integer? -> integer?)))
    (define hint-round '((real? -> integer?)))
    (define hint-gcd
      '((-> integer?) (integer? -> integer?) (integer? integer? -> integer?))
    ) ;define
    (define hint-lcm
      '((-> integer?) (integer? -> integer?) (integer? integer? -> integer?))
    ) ;define
    (define hint-quotient '((integer? integer? -> integer?)))
    (define hint-remainder '((integer? integer? -> integer?)))
    (define hint-modulo '((integer? integer? -> integer?)))
    (define hint-numerator '((rational? -> integer?)))
    (define hint-denominator '((rational? -> integer?)))
    (define hint-rationalize '((real? real? -> rational?)))
    (define hint-exact-integer-sqrt '((integer? -> any?)))
    (define hint-number->string
      '((number? -> string?) (number? integer? -> string?))
    ) ;define
    (define hint-string->number
      '((string? -> (or #f number?)) (string? integer? -> (or #f number?)))
    ) ;define

    ;; -------------------- 对与列表 --------------------
    (define hint-cons '((any? any? -> pair?)))
    (define hint-car '((pair? -> any?)))
    (define hint-cdr '((pair? -> any?)))
    (define hint-set-car! '((pair? any? -> any?)))
    (define hint-set-cdr! '((pair? any? -> any?)))
    (define hint-caar '((pair? -> any?)))
    (define hint-cadr '((pair? -> any?)))
    (define hint-cdar '((pair? -> any?)))
    (define hint-cddr '((pair? -> any?)))
    (define hint-make-list '((integer? -> list?) (integer? any? -> list?)))
    (define hint-list
      '((-> list?)
        (any? -> list?)
        (any? any? -> list?)
        (any? any? any? -> list?)
        (any? any? any? any? -> list?)
        (any? any? any? any? any? -> list?)
        (any? any? any? any? any? any? -> list?)
        (any? any? any? any? any? any? any? -> list?))
    ) ;define
    (define hint-length '((any? -> integer?)))
    (define hint-append
      '((-> list?)
        (list? -> list?)
        (list? list? -> list?)
        (list? list? list? -> list?))
    ) ;define
    (define hint-reverse '((list? -> list?)))
    (define hint-list-tail '((list? integer? -> any?)))
    (define hint-list-ref '((list? integer? -> any?)))
    (define hint-list-set! '((list? integer? any? -> any?)))
    (define hint-memq '((any? list? -> (or #f list?))))
    (define hint-memv '((any? list? -> (or #f list?))))
    (define hint-member
      '((any? list? -> (or #f list?)) (any? list? procedure? -> (or #f list?)))
    ) ;define
    (define hint-assq '((any? alist? -> (or #f pair?))))
    (define hint-assv '((any? alist? -> (or #f pair?))))
    (define hint-assoc
      '((any? alist? -> (or #f pair?))
        (any? alist? procedure? -> (or #f pair?)))
    ) ;define
    (define hint-list-copy '((list? -> list?)))
    (define hint-map
      '((procedure? list? -> list?) (procedure? list? list? -> list?))
    ) ;define
    (define hint-for-each
      '((procedure? list? -> any?) (procedure? list? list? -> any?))
    ) ;define
    (define hint-list->string '((list? -> string?)))
    (define hint-list->vector '((list? -> vector?)))
    (define hint-pair-line-number '((pair? -> (or #f integer?))))

    ;; -------------------- 符号与字符 --------------------
    (define hint-symbol->string '((symbol? -> string?)))
    (define hint-string->symbol '((string? -> symbol?)))
    (define hint-char=? '((char? char? -> boolean?)))
    (define hint-char<? '((char? char? -> boolean?)))
    (define hint-char>? '((char? char? -> boolean?)))
    (define hint-char<=? '((char? char? -> boolean?)))
    (define hint-char>=? '((char? char? -> boolean?)))
    (define hint-char->integer '((char? -> integer?)))
    (define hint-integer->char '((integer? -> char?)))

    ;; -------------------- 字符串 --------------------
    (define hint-make-string '((integer? -> string?)
                               (integer? char? -> string?)))
    (define hint-string
      '((-> string?)
        (char? -> string?)
        (char? char? -> string?)
        (char? char? char? -> string?)
        (char? char? char? char? -> string?))
    ) ;define
    (define hint-string-length '((string? -> integer?)))
    (define hint-string-ref '((string? integer? -> char?)))
    (define hint-string-set! '((string? integer? char? -> any?)))
    (define hint-string-copy
      '((string? -> string?)
        (string? integer? -> string?)
        (string? integer? integer? -> string?))
    ) ;define
    (define hint-string-append
      '((-> string?)
        (string? -> string?)
        (string? string? -> string?)
        (string? string? string? -> string?)
        (string? string? string? string? -> string?)
        (string? string? string? string? string? -> string?))
    ) ;define
    (define hint-substring '((string? integer? integer? -> string?)))
    (define hint-string-fill!
      '((string? char? -> any?)
        (string? char? integer? -> any?)
        (string? char? integer? integer? -> any?))
    ) ;define
    (define hint-string-copy!
      '((string? integer? string? -> any?)
        (string? integer? string? integer? -> any?)
        (string? integer? string? integer? integer? -> any?))
    ) ;define
    (define hint-string->list
      '((string? -> list?)
        (string? integer? -> list?)
        (string? integer? integer? -> list?))
    ) ;define
    (define hint-string=? '((string? string? -> boolean?)))
    (define hint-string<? '((string? string? -> boolean?)))
    (define hint-string>? '((string? string? -> boolean?)))
    (define hint-string<=? '((string? string? -> boolean?)))
    (define hint-string>=? '((string? string? -> boolean?)))
    (define hint-string-map '((procedure? string? -> string?)))
    (define hint-string-for-each '((procedure? string? -> any?)))
    (define hint-string->utf8
      '((string? -> bytevector?)
        (string? integer? -> bytevector?)
        (string? integer? integer? -> bytevector?))
    ) ;define
    (define hint-utf8->string
      '((bytevector? -> string?)
        (bytevector? integer? -> string?)
        (bytevector? integer? integer? -> string?))
    ) ;define
    (define hint-utf8-string-length '((string? -> integer?)))
    (define hint-string->vector
      '((string? -> vector?)
        (string? integer? -> vector?)
        (string? integer? integer? -> vector?))
    ) ;define

    ;; -------------------- 向量 --------------------
    (define hint-make-vector '((integer? -> vector?) (integer? any? -> vector?)))
    (define hint-vector
      '((-> vector?)
        (any? -> vector?)
        (any? any? -> vector?)
        (any? any? any? -> vector?)
        (any? any? any? any? -> vector?))
    ) ;define
    (define hint-vector-length '((vector? -> integer?)))
    (define hint-vector-ref '((vector? integer? -> any?)))
    (define hint-vector-set! '((vector? integer? any? -> any?)))
    (define hint-vector->list
      '((vector? -> list?)
        (vector? integer? -> list?)
        (vector? integer? integer? -> list?))
    ) ;define
    (define hint-vector->string
      '((vector? -> string?)
        (vector? integer? -> string?)
        (vector? integer? integer? -> string?))
    ) ;define
    (define hint-vector-copy
      '((vector? -> vector?)
        (vector? integer? -> vector?)
        (vector? integer? integer? -> vector?))
    ) ;define
    (define hint-vector-copy!
      '((vector? integer? vector? -> any?)
        (vector? integer? vector? integer? -> any?)
        (vector? integer? vector? integer? integer? -> any?))
    ) ;define
    (define hint-vector-fill!
      '((vector? any? -> any?)
        (vector? any? integer? -> any?)
        (vector? any? integer? integer? -> any?))
    ) ;define
    (define hint-vector-map '((procedure? vector? -> vector?)))
    (define hint-vector-for-each '((procedure? vector? -> any?)))

    ;; -------------------- 字节向量 --------------------
    (define hint-make-bytevector
      '((integer? -> bytevector?) (integer? integer? -> bytevector?))
    ) ;define
    (define hint-bytevector
      '((-> bytevector?)
        (integer? -> bytevector?)
        (integer? integer? -> bytevector?)
        (integer? integer? integer? -> bytevector?)
        (integer? integer? integer? integer? -> bytevector?))
    ) ;define
    (define hint-bytevector-length '((bytevector? -> integer?)))
    (define hint-bytevector-u8-ref '((bytevector? integer? -> integer?)))
    (define hint-bytevector-u8-set! '((bytevector? integer? integer? -> any?)))
    (define hint-bytevector-copy
      '((bytevector? -> bytevector?)
        (bytevector? integer? -> bytevector?)
        (bytevector? integer? integer? -> bytevector?))
    ) ;define
    (define hint-bytevector-copy!
      '((bytevector? integer? bytevector? -> any?)
        (bytevector? integer? bytevector? integer? -> any?)
        (bytevector? integer? bytevector? integer? integer? -> any?))
    ) ;define
    (define hint-bytevector-append
      '((-> bytevector?)
        (bytevector? -> bytevector?)
        (bytevector? bytevector? -> bytevector?)
        (bytevector? bytevector? bytevector? -> bytevector?))
    ) ;define
    (define hint-bytevector-advance-utf8
      '((bytevector? integer? -> integer?)
        (bytevector? integer? integer? -> integer?))
    ) ;define

    ;; -------------------- 端口与 I/O --------------------
    (define hint-call-with-port '((port? procedure? -> any?)))
    (define hint-current-input-port '((-> input-port?)))
    (define hint-current-output-port '((-> output-port?)))
    (define hint-current-error-port '((-> output-port?)))
    (define hint-close-port '((port? -> any?)))
    (define hint-close-input-port '((input-port? -> any?)))
    (define hint-close-output-port '((output-port? -> any?)))
    (define hint-open-input-string '((string? -> input-port?)))
    (define hint-open-output-string '((-> output-port?)))
    (define hint-get-output-string '((output-port? -> string?)))
    (define hint-open-input-bytevector '((bytevector? -> input-port?)))
    (define hint-open-output-bytevector '((-> output-port?)))
    (define hint-get-output-bytevector '((output-port? -> bytevector?)))
    (define hint-read '((-> any?) (input-port? -> any?)))
    (define hint-read-char '((-> any?) (input-port? -> any?)))
    (define hint-peek-char '((-> any?) (input-port? -> any?)))
    (define hint-read-line '((-> any?) (input-port? -> any?)))
    (define hint-read-string '((integer? -> any?)
                               (integer? input-port? -> any?)))
    (define hint-read-u8 '((-> any?) (input-port? -> any?)))
    (define hint-peek-u8 '((-> any?) (input-port? -> any?)))
    (define hint-read-bytevector
      '((integer? -> any?) (integer? input-port? -> any?))
    ) ;define
    (define hint-read-bytevector!
      '((bytevector? -> any?)
        (bytevector? input-port? -> any?)
        (bytevector? input-port? integer? -> any?)
        (bytevector? input-port? integer? integer? -> any?))
    ) ;define
    (define hint-write-char '((char? -> any?) (char? output-port? -> any?)))
    (define hint-write-string
      '((string? -> any?)
        (string? output-port? -> any?)
        (string? output-port? integer? -> any?)
        (string? output-port? integer? integer? -> any?))
    ) ;define
    (define hint-write-u8 '((integer? -> any?) (integer? output-port? -> any?)))
    (define hint-write-bytevector
      '((bytevector? -> any?)
        (bytevector? output-port? -> any?)
        (bytevector? output-port? integer? -> any?)
        (bytevector? output-port? integer? integer? -> any?))
    ) ;define
    (define hint-newline '((-> any?) (output-port? -> any?)))
    (define hint-flush-output-port '((-> any?) (output-port? -> any?)))
    (define hint-eof-object '((-> eof-object?)))

    ;; -------------------- 控制流与值 --------------------
    (define hint-apply '((procedure? list? -> any?)))
    (define hint-call-with-current-continuation '((procedure? -> any?)))
    (define hint-call/cc '((procedure? -> any?)))
    (define hint-values
      '((-> any?) (any? -> any?) (any? any? -> any?) (any? any? any? -> any?))
    ) ;define
    (define hint-call-with-values '((procedure? procedure? -> any?)))
    (define hint-dynamic-wind '((procedure? procedure? procedure? -> any?)))
    (define hint-make-parameter
      '((any? -> procedure?) (any? procedure? -> procedure?))
    ) ;define
    (define hint-catch '((any? procedure? procedure? -> any?)))
    (define hint-raise '((any? -> any?)))
    (define hint-error '((any? any? -> any?) (any? any? any? -> any?)))

    ;; -------------------- 标准查询函数 --------------------
    (define (hint-scheme/base name)
      (case name
       ((let-values) hint-let-values)
       ((let*-values) hint-let*-values)
       ((define-syntax) hint-define-syntax)
       ((syntax-rules) hint-syntax-rules)
       ((syntax-error) hint-syntax-error)
       ((let-syntax) hint-let-syntax)
       ((letrec-syntax) hint-letrec-syntax)
       ((define-values) hint-define-values)
       ((define-record-type) hint-define-record-type)
       ((parameterize) hint-parameterize)
       ((guard) hint-guard)
       ((boolean?) hint-boolean?)
       ((not) hint-not)
       ((boolean=?) hint-boolean=?)
       ((symbol?) hint-symbol?)
       ((symbol=?) hint-symbol=?)
       ((char?) hint-char?)
       ((number?) hint-number?)
       ((complex?) hint-complex?)
       ((real?) hint-real?)
       ((rational?) hint-rational?)
       ((integer?) hint-integer?)
       ((exact?) hint-exact?)
       ((inexact?) hint-inexact?)
       ((exact-integer?) hint-exact-integer?)
       ((zero?) hint-zero?)
       ((positive?) hint-positive?)
       ((negative?) hint-negative?)
       ((odd?) hint-odd?)
       ((even?) hint-even?)
       ((pair?) hint-pair?)
       ((null?) hint-null?)
       ((list?) hint-list?)
       ((string?) hint-string?)
       ((vector?) hint-vector?)
       ((bytevector?) hint-bytevector?)
       ((procedure?) hint-procedure?)
       ((port?) hint-port?)
       ((binary-port?) hint-binary-port?)
       ((textual-port?) hint-textual-port?)
       ((input-port?) hint-input-port?)
       ((output-port?) hint-output-port?)
       ((input-port-open?) hint-input-port-open?)
       ((output-port-open?) hint-output-port-open?)
       ((eof-object?) hint-eof-object?)
       ((char-ready?) hint-char-ready?)
       ((u8-ready?) hint-u8-ready?)
       ((read-error?) hint-read-error?)
       ((file-error?) hint-file-error?)
       ((eq?) hint-eq?)
       ((eqv?) hint-eqv?)
       ((equal?) hint-equal?)
       ((=) hint=)
       ((<) hint-<)
       ((>) hint->)
       ((<=) hint-<=)
       ((>=) hint->=)
       ((+) hint+)
       ((-) hint-)
       ((*) hint*)
       ((/) hint/)
       ((abs) hint-abs)
       ((square) hint-square)
       ((expt) hint-expt)
       ((exact) hint-exact)
       ((inexact) hint-inexact)
       ((max) hint-max)
       ((min) hint-min)
       ((floor) hint-floor)
       ((floor/) hint-floor/)
       ((floor-quotient) hint-floor-quotient)
       ((floor-remainder) hint-floor-remainder)
       ((ceiling) hint-ceiling)
       ((truncate) hint-truncate)
       ((truncate/) hint-truncate/)
       ((truncate-quotient) hint-truncate-quotient)
       ((truncate-remainder) hint-truncate-remainder)
       ((round) hint-round)
       ((gcd) hint-gcd)
       ((lcm) hint-lcm)
       ((quotient) hint-quotient)
       ((remainder) hint-remainder)
       ((modulo) hint-modulo)
       ((numerator) hint-numerator)
       ((denominator) hint-denominator)
       ((rationalize) hint-rationalize)
       ((exact-integer-sqrt) hint-exact-integer-sqrt)
       ((number->string) hint-number->string)
       ((string->number) hint-string->number)
       ((cons) hint-cons)
       ((car) hint-car)
       ((cdr) hint-cdr)
       ((set-car!) hint-set-car!)
       ((set-cdr!) hint-set-cdr!)
       ((caar) hint-caar)
       ((cadr) hint-cadr)
       ((cdar) hint-cdar)
       ((cddr) hint-cddr)
       ((make-list) hint-make-list)
       ((list) hint-list)
       ((length) hint-length)
       ((append) hint-append)
       ((reverse) hint-reverse)
       ((list-tail) hint-list-tail)
       ((list-ref) hint-list-ref)
       ((list-set!) hint-list-set!)
       ((memq) hint-memq)
       ((memv) hint-memv)
       ((member) hint-member)
       ((assq) hint-assq)
       ((assv) hint-assv)
       ((assoc) hint-assoc)
       ((list-copy) hint-list-copy)
       ((map) hint-map)
       ((for-each) hint-for-each)
       ((list->string) hint-list->string)
       ((list->vector) hint-list->vector)
       ((pair-line-number) hint-pair-line-number)
       ((symbol->string) hint-symbol->string)
       ((string->symbol) hint-string->symbol)
       ((char=?) hint-char=?)
       ((char<?) hint-char<?)
       ((char>?) hint-char>?)
       ((char<=?) hint-char<=?)
       ((char>=?) hint-char>=?)
       ((char->integer) hint-char->integer)
       ((integer->char) hint-integer->char)
       ((make-string) hint-make-string)
       ((string) hint-string)
       ((string-length) hint-string-length)
       ((string-ref) hint-string-ref)
       ((string-set!) hint-string-set!)
       ((string-copy) hint-string-copy)
       ((string-append) hint-string-append)
       ((substring) hint-substring)
       ((string-fill!) hint-string-fill!)
       ((string-copy!) hint-string-copy!)
       ((string->list) hint-string->list)
       ((string=?) hint-string=?)
       ((string<?) hint-string<?)
       ((string>?) hint-string>?)
       ((string<=?) hint-string<=?)
       ((string>=?) hint-string>=?)
       ((string-map) hint-string-map)
       ((string-for-each) hint-string-for-each)
       ((string->utf8) hint-string->utf8)
       ((utf8->string) hint-utf8->string)
       ((utf8-string-length) hint-utf8-string-length)
       ((string->vector) hint-string->vector)
       ((make-vector) hint-make-vector)
       ((vector) hint-vector)
       ((vector-length) hint-vector-length)
       ((vector-ref) hint-vector-ref)
       ((vector-set!) hint-vector-set!)
       ((vector->list) hint-vector->list)
       ((vector->string) hint-vector->string)
       ((vector-copy) hint-vector-copy)
       ((vector-copy!) hint-vector-copy!)
       ((vector-fill!) hint-vector-fill!)
       ((vector-map) hint-vector-map)
       ((vector-for-each) hint-vector-for-each)
       ((make-bytevector) hint-make-bytevector)
       ((bytevector) hint-bytevector)
       ((bytevector-length) hint-bytevector-length)
       ((bytevector-u8-ref) hint-bytevector-u8-ref)
       ((bytevector-u8-set!) hint-bytevector-u8-set!)
       ((bytevector-copy) hint-bytevector-copy)
       ((bytevector-copy!) hint-bytevector-copy!)
       ((bytevector-append) hint-bytevector-append)
       ((bytevector-advance-utf8) hint-bytevector-advance-utf8)
       ((call-with-port) hint-call-with-port)
       ((current-input-port) hint-current-input-port)
       ((current-output-port) hint-current-output-port)
       ((current-error-port) hint-current-error-port)
       ((close-port) hint-close-port)
       ((close-input-port) hint-close-input-port)
       ((close-output-port) hint-close-output-port)
       ((open-input-string) hint-open-input-string)
       ((open-output-string) hint-open-output-string)
       ((get-output-string) hint-get-output-string)
       ((open-input-bytevector) hint-open-input-bytevector)
       ((open-output-bytevector) hint-open-output-bytevector)
       ((get-output-bytevector) hint-get-output-bytevector)
       ((read) hint-read)
       ((read-char) hint-read-char)
       ((peek-char) hint-peek-char)
       ((read-line) hint-read-line)
       ((read-string) hint-read-string)
       ((read-u8) hint-read-u8)
       ((peek-u8) hint-peek-u8)
       ((read-bytevector) hint-read-bytevector)
       ((read-bytevector!) hint-read-bytevector!)
       ((write-char) hint-write-char)
       ((write-string) hint-write-string)
       ((write-u8) hint-write-u8)
       ((write-bytevector) hint-write-bytevector)
       ((newline) hint-newline)
       ((flush-output-port) hint-flush-output-port)
       ((eof-object) hint-eof-object)
       ((apply) hint-apply)
       ((call-with-current-continuation) hint-call-with-current-continuation)
       ((call/cc) hint-call/cc)
       ((values) hint-values)
       ((call-with-values) hint-call-with-values)
       ((dynamic-wind) hint-dynamic-wind)
       ((make-parameter) hint-make-parameter)
       ((catch) hint-catch)
       ((raise) hint-raise)
       ((error) hint-error)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library

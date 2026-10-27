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
;;
;; 目前登记类型检查实际用到的核心过程，其余过程随使用范围扩展逐步补充。

(define-library (hint scheme base)
  (import (scheme base))
  (export hint-scheme/base)
  (begin

    ;; 谓词
    (define hint-list? '((any? -> boolean?)))
    (define hint-pair? '((any? -> boolean?)))
    (define hint-null? '((any? -> boolean?)))
    (define hint-vector? '((any? -> boolean?)))
    (define hint-symbol? '((any? -> boolean?)))
    (define hint-eof-object? '((any? -> boolean?)))
    (define hint-not '((any? -> boolean?)))
    (define hint-string? '((any? -> boolean?)))
    (define hint-boolean? '((any? -> boolean?)))
    (define hint-integer? '((any? -> boolean?)))
    (define hint-procedure? '((any? -> boolean?)))
    (define hint-number? '((any? -> boolean?)))
    (define hint-char? '((any? -> boolean?)))

    ;; 存取与构造
    (define hint-cons '((any? any? -> pair?)))
    (define hint-car '((pair? -> any?)))
    (define hint-cdr '((pair? -> any?)))
    (define hint-caar '((pair? -> any?)))
    (define hint-cadr '((pair? -> any?)))
    (define hint-cadar '((pair? -> any?)))
    (define hint-cddr '((pair? -> any?)))
    (define hint-caddr '((pair? -> any?)))
    (define hint-cadddr '((pair? -> any?)))
    (define hint-list-ref '((list? integer? -> any?)))
    (define hint-pair-line-number '((pair? -> (or #f integer?))))
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
    (define hint-reverse '((list? -> list?)))
    (define hint-vector->list '((vector? -> list?)))

    ;; apply 的可变中间参数暂不能表达；登记无中间参数的形式。
    (define hint-apply '((procedure? list? -> any?)))

    ;; append 的任意数量参数暂不能完整表达；登记 0、1、2 个参数。
    (define hint-append '((-> list?) (list? -> list?) (list? list? -> list?)))

    ;; map 的可变列表参数暂不能完整表达；登记 1、2 个列表。
    (define hint-map
      '((procedure? list? -> list?) (procedure? list? list? -> list?))
    ) ;define

    ;; 字符串与符号
    (define hint-symbol->string '((symbol? -> string?)))
    (define hint-string=? '((string? string? -> boolean?)))
    (define hint-string-length '((string? -> integer?)))
    (define hint-substring '((string? integer? integer? -> string?)))
    (define hint-string-ref '((string? integer? -> char?)))
    (define hint-string->number '((string? -> (or #f number?))))

    ;; string-append 为可变参数，登记 2-5 个参数的形式
    (define hint-string-append
      '((string? string? -> string?)
        (string? string? string? -> string?)
        (string? string? string? string? -> string?)
        (string? string? string? string? string? -> string?))
    ) ;define

    ;; 字符与向量
    (define hint-char=? '((char? char? -> boolean?)))
    (define hint-vector-ref '((vector? integer? -> any?)))
    (define hint-vector-length '((vector? -> integer?)))

    ;; 端口、读取与控制
    (define hint-open-input-string '((string? -> input-port?)))
    (define hint-read '((-> any?) (input-port? -> any?)))
    (define hint-eof-object '((-> eof-object?)))
    (define hint-close-input-port '((input-port? -> any?)))
    (define hint-dynamic-wind '((procedure? procedure? procedure? -> any?)))
    (define hint-catch '((any? procedure? procedure? -> any?)))
    (define hint-call-with-values '((procedure? procedure? -> any?)))

    ;; 长度：s7 中 length 是泛型过程，列表、向量、字符串等均可
    (define hint-length '((any? -> integer?)))

    ;; 数值
    (define hint+ '((number? number? -> number?)))
    (define hint* '((number? number? -> number?)))
    (define hint= '((number? number? -> boolean?)))
    (define hint-< '((number? number? -> boolean?)))
    (define hint-> '((number? number? -> boolean?)))
    (define hint-<= '((number? number? -> boolean?)))
    (define hint->= '((number? number? -> boolean?)))
    (define hint--
      '((number? -> number?)
        (number? number? -> number?)
        (number? number? number? -> number?))
    ) ;define
    (define hint-zero? '((number? -> boolean?)))

    ;; 等价比较
    (define hint-eq? '((any? any? -> boolean?)))
    (define hint-eqv? '((any? any? -> boolean?)))

    ;; 关联表查找：命中返回键值对，未命中返回 #f；第三参可选比较函数
    (define hint-assoc
      '((any? alist? -> (or #f pair?))
        (any? alist? procedure? -> (or #f pair?)))
    ) ;define
    (define hint-assq '((any? alist? -> (or #f pair?))))
    (define hint-assv '((any? alist? -> (or #f pair?))))

    ;; 等价比较
    (define hint-eq? '((any? any? -> boolean?)))
    (define hint-eqv? '((any? any? -> boolean?)))
    (define hint-equal? '((any? any? -> boolean?)))

    ;; 错误抛出：(error type message . irritants) 为可变参数，登记 2、3 参形式
    (define hint-error '((any? any? -> any?) (any? any? any? -> any?)))

    ;; 多返回值
    (define hint-values '((-> any?) (any? -> any?) (any? any? -> any?)))

    (define (hint-scheme/base name)
      (case name
       ((list?) hint-list?)
       ((pair?) hint-pair?)
       ((null?) hint-null?)
       ((vector?) hint-vector?)
       ((symbol?) hint-symbol?)
       ((string?) hint-string?)
       ((boolean?) hint-boolean?)
       ((integer?) hint-integer?)
       ((procedure?) hint-procedure?)
       ((number?) hint-number?)
       ((char?) hint-char?)
       ((eof-object?) hint-eof-object?)
       ((not) hint-not)
       ((cons) hint-cons)
       ((car) hint-car)
       ((cdr) hint-cdr)
       ((caar) hint-caar)
       ((cadr) hint-cadr)
       ((cadar) hint-cadar)
       ((cddr) hint-cddr)
       ((caddr) hint-caddr)
       ((cadddr) hint-cadddr)
       ((list-ref) hint-list-ref)
       ((pair-line-number) hint-pair-line-number)
       ((list) hint-list)
       ((reverse) hint-reverse)
       ((apply) hint-apply)
       ((append) hint-append)
       ((map) hint-map)
       ((vector->list) hint-vector->list)
       ((symbol->string) hint-symbol->string)
       ((string=?) hint-string=?)
       ((string-length) hint-string-length)
       ((substring) hint-substring)
       ((string-ref) hint-string-ref)
       ((string->number) hint-string->number)
       ((string-append) hint-string-append)
       ((char=?) hint-char=?)
       ((vector-ref) hint-vector-ref)
       ((vector-length) hint-vector-length)
       ((open-input-string) hint-open-input-string)
       ((read) hint-read)
       ((eof-object) hint-eof-object)
       ((close-input-port) hint-close-input-port)
       ((dynamic-wind) hint-dynamic-wind)
       ((catch) hint-catch)
       ((call-with-values) hint-call-with-values)
       ((length) hint-length)
       ((+) hint+)
       ((*) hint*)
       ((=) hint=)
       ((-) hint--)
       ((<) hint-<)
       ((>) hint->)
       ((<=) hint-<=)
       ((>=) hint->=)
       ((zero?) hint-zero?)
       ((eq?) hint-eq?)
       ((eqv?) hint-eqv?)
       ((equal?) hint-equal?)
       ((assoc) hint-assoc)
       ((assq) hint-assq)
       ((assv) hint-assv)
       ((values) hint-values)
       ((error) hint-error)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library

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
;;

;; (hint liii base) - (liii base) 的类型接口
;;
;; 本库只提供类型签名数据，不导出 (liii base) 的任何过程，
;; 也不导入其他 hint 库，因此可以被独立加载。
;;
;; (liii base) 导出的过程分为三类，签名均在本库声明：
;;   1. 本地实现：loose-car loose-cdr compose identity any?
;;   2. s7 内置实现：procedure-source procedure-arglist arity defined?
;;      object->string eval-string signature copy keyword? string->keyword
;;      symbol->keyword keyword->symbol make-hook hook-functions
;;      with-output-to-string with-input-from-string call-with-input-string
;;      call-with-output-string reverse! sort! format cyclic-sequences
;;      这些过程没有 Scheme 层实现，由 (liii base) 导出，归属本库。
;;   3. 语法形式：and-let* receive define* lambda* define-macro define-bacro
;;      typed-lambda 不是过程，不提供签名。

(define-library (hint liii base)
  (import (scheme base))
  (export hint-liii/base)
  (begin

    ;; 过程反射
    (define hint-procedure-source '((procedure? -> any?)))
    (define hint-procedure-arglist '((procedure? -> any?)))
    (define hint-arity '((procedure? -> list?)))
    (define hint-defined? '((symbol? -> boolean?)))
    (define hint-object->string '((any? -> string?)))
    (define hint-eval-string '((string? -> any?)))
    (define hint-signature '((any? -> any?)))
    (define hint-copy '((any? -> any?)))

    ;; 关键字
    (define hint-keyword? '((any? -> boolean?)))
    (define hint-string->keyword '((string? -> any?)))
    (define hint-symbol->keyword '((symbol? -> any?)))
    (define hint-keyword->symbol '((any? -> symbol?)))

    ;; 列表与组合
    (define hint-loose-car '((any? -> any?)))
    (define hint-loose-cdr '((any? -> any?)))
    (define hint-compose
      '((procedure? -> procedure?)
        (procedure? procedure? -> procedure?)
        (procedure? procedure? procedure? -> procedure?))
    ) ;define
    (define hint-identity '((any? -> any?)))
    (define hint-any? '((any? -> boolean?)))

    ;; hook
    (define hint-make-hook '((procedure? -> any?)))
    (define hint-hook-functions '((any? -> any?)))

    ;; 端口与字符串
    (define hint-with-output-to-string '((procedure? -> string?)))
    (define hint-with-input-from-string '((string? procedure? -> any?)))
    (define hint-call-with-input-string '((string? procedure? -> any?)))
    (define hint-call-with-output-string '((procedure? -> string?)))
    (define hint-reverse! '((list? -> list?)))
    (define hint-sort! '((list? procedure? -> list?)))

    ;; format 的可变参数暂不能完整表达；登记 1~4 个参数的形式。
    (define hint-format
      '((any? string? -> string?)
        (any? string? any? -> string?)
        (any? string? any? any? -> string?)
        (any? string? any? any? any? -> string?))
    ) ;define

    (define hint-cyclic-sequences '((list? -> any?)))

    (define (hint-liii/base name)
      (case name
       ((procedure-source) hint-procedure-source)
       ((procedure-arglist) hint-procedure-arglist)
       ((arity) hint-arity)
       ((defined?) hint-defined?)
       ((object->string) hint-object->string)
       ((eval-string) hint-eval-string)
       ((signature) hint-signature)
       ((copy) hint-copy)
       ((keyword?) hint-keyword?)
       ((string->keyword) hint-string->keyword)
       ((symbol->keyword) hint-symbol->keyword)
       ((keyword->symbol) hint-keyword->symbol)
       ((loose-car) hint-loose-car)
       ((loose-cdr) hint-loose-cdr)
       ((compose) hint-compose)
       ((identity) hint-identity)
       ((any?) hint-any?)
       ((make-hook) hint-make-hook)
       ((hook-functions) hint-hook-functions)
       ((with-output-to-string) hint-with-output-to-string)
       ((with-input-from-string) hint-with-input-from-string)
       ((call-with-input-string) hint-call-with-input-string)
       ((call-with-output-string) hint-call-with-output-string)
       ((reverse!) hint-reverse!)
       ((sort!) hint-sort!)
       ((format) hint-format)
       ((cyclic-sequences) hint-cyclic-sequences)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library

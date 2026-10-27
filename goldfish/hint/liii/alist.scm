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

;; (hint liii alist) - (liii alist) 的类型接口
;;
;; 本库只提供类型签名数据，不导出 (liii alist) 的任何过程，
;; 也不导入其他 hint 库，因此可以被独立加载。
;;
;; 只在实现库中标记签名：(liii alist) 自身实现的过程在这里声明类型；
;; 再导出的过程（例如 alist-cons）不在这里重复声明。
;;
;; (liii alist) 实现中用到的依赖过程，其签名位于：
;;   (hint scheme base)  - list? pair? null? vector? cons car cdr length
;;                          vector->list + = eqv? assoc error
;;   (hint srfi srfi-1)  - every
;;   (hint liii error)   - key-error
;; case-lambda 与 unless 是语法形式，不提供过程签名。

(define-library (hint liii alist)
  (import (scheme base))
  (export hint-liii-alist)
  (begin

    ;; alist? : (any? -> boolean?)
    (define hint-alist? '((any? -> boolean?)))

    ;; alist-ref : (alist? any? -> any?)
    ;;             (alist? any? procedure? -> any?)
    ;;             (alist? any? procedure? procedure? -> any?)
    (define hint-alist-ref
      '((alist? any? -> any?)
        (alist? any? procedure? -> any?)
        (alist? any? procedure? procedure? -> any?))
    ) ;define

    ;; alist-ref/default : (alist? any? any? -> any?)
    ;;                      (alist? any? any? procedure? -> any?)
    (define hint-alist-ref/default
      '((alist? any? any? -> any?) (alist? any? any? procedure? -> any?))
    ) ;define

    ;; vector->alist : (vector? -> alist?)
    (define hint-vector->alist '((vector? -> alist?)))

    (define (hint-liii-alist name)
      (case name
       ((alist?) hint-alist?)
       ((alist-ref) hint-alist-ref)
       ((alist-ref/default) hint-alist-ref/default)
       ((vector->alist) hint-vector->alist)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library

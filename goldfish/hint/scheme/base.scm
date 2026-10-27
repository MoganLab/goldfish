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
;; 目前只登记 (liii alist) 实际用到的核心过程，
;; 其余过程随使用范围扩展逐步补充。

(define-library (hint scheme base)
  (import (scheme base))
  (export hint-scheme-base)
  (begin

    ;; 谓词
    (define hint-list? '((any? -> boolean?)))
    (define hint-pair? '((any? -> boolean?)))
    (define hint-null? '((any? -> boolean?)))
    (define hint-vector? '((any? -> boolean?)))

    ;; 存取与构造
    (define hint-cons '((any? any? -> pair?)))
    (define hint-car '((pair? -> any?)))
    (define hint-cdr '((pair? -> any?)))
    (define hint-vector->list '((vector? -> list?)))

    ;; 长度：s7 中 length 是泛型过程，列表、向量、字符串等均可
    (define hint-length '((any? -> integer?)))

    ;; 数值
    (define hint+ '((number? number? -> number?)))
    (define hint= '((number? number? -> boolean?)))

    ;; 等价比较
    (define hint-eqv? '((any? any? -> boolean?)))

    ;; 关联表查找：命中返回键值对，未命中返回 #f；第三参可选比较函数
    (define hint-assoc
      '((any? alist? -> (or #f pair?))
        (any? alist? procedure? -> (or #f pair?)))
    ) ;define

    ;; 错误抛出：(error type message . irritants)
    (define hint-error '((any? any? -> any?)))

    (define (hint-scheme-base name)
      (case name
       ((list?) hint-list?)
       ((pair?) hint-pair?)
       ((null?) hint-null?)
       ((vector?) hint-vector?)
       ((cons) hint-cons)
       ((car) hint-car)
       ((cdr) hint-cdr)
       ((vector->list) hint-vector->list)
       ((length) hint-length)
       ((+) hint+)
       ((=) hint=)
       ((eqv?) hint-eqv?)
       ((assoc) hint-assoc)
       ((error) hint-error)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library

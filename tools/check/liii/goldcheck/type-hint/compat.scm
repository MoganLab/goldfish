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

;; 类型兼容规则：实参类型能否作为形参类型传入。
;; 供文件级静态检查（type-hint infer）使用。

(define-library (liii goldcheck type-hint compat)
  (import (scheme base) (liii list))
  (export %accepts?)
  (begin

    ;; 已知互斥的标量类型分组；number 系同组内成层次；
    ;; pair?/list?/null? 同属结构组，与标量组互斥但组内保守
    (define %type-groups
      '((number? integer? real? rational?)
        (string?)
        (boolean?)
        (symbol?)
        (char?)
        (procedure?)
        (vector?)
        (pair? list? null?))
    ) ;define

    (define (%group-of t)
      (let loop
        ((gs %type-groups) (i 0))
        (cond ((null? gs) #f)
              ((memq t (car gs)) i)
              (else (loop (cdr gs) (+ i 1)))
        ) ;cond
      ) ;let
    ) ;define

    ;; actual 可作为 expected 传入
    (define (%accepts? expected actual)
      (cond ((eq? expected 'any?) #t)
            ((not actual) #t)
            ((eq? actual 'unknown) #t)
            ((pair? actual)
             (and (eq? (car actual) 'or)
               (let loop
                 ((ms (cdr actual)))
                 (cond ((null? ms) #t)
                       ((%accepts-1 expected (if (eq? (car ms) #f) 'boolean? (car ms)))
                        (loop (cdr ms))
                       ) ;
                       (else #f)
                 ) ;cond
               ) ;let
             ) ;and
            ) ;
            ((pair? expected)
             ;; 期望类型为联合类型：实际类型至少要能被其中一个成员接受；
             ;; #f 成员对应“可能为假”，实参为 #f 时按 unknown 处理，故跳过
             (if (eq? (car expected) 'or)
               (let loop
                 ((ms (cdr expected)))
                 (cond ((null? ms) #f)
                       ((eq? (car ms) #f) (loop (cdr ms)))
                       ((%accepts? (car ms) actual) #t)
                       (else (loop (cdr ms)))
                 ) ;cond
               ) ;let
               #t
             ) ;if
            ) ;
            ((symbol? expected)
             (or (eq? actual expected)
               ;; number 层次：integer?/real?/rational? 可传 number?
               (and (memq actual '(integer? real? rational?)) (eq? expected 'number?))
               ;; 涉及未知谓词（如 alist? 等自定义类型）时不判定冲突
               (not
                 (and (%group-of actual)
                   (%group-of expected)
                   (not (= (%group-of actual) (%group-of expected)))
                 ) ;and
               ) ;not
             ) ;or
            ) ;
            (else #t)
      ) ;cond
    ) ;define

    (define (%accepts-1 expected member-type)
      (if (not (symbol? expected))
        #t
        (or (eq? member-type expected)
          (eq? expected 'any?)
          (not
            (and (%group-of member-type)
              (%group-of expected)
              (not (= (%group-of member-type) (%group-of expected)))
            ) ;and
          ) ;not
        ) ;or
      ) ;if
    ) ;define

  ) ;begin
) ;define-library

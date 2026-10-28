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

;; A7 校验：hint 库中签名声明的归属唯一性。
;;
;; A7a 归属正确性：再导出的过程不得在再导出库的 hint 中本地声明，
;;     签名应声明在实现库的 hint 中；实现库没有 hint 库时放行。
;; A7b 库内重复声明：同一 hint 变量被多次 define；同一函数名在查询
;;     函数中被重复登记。
;; A7c 跨库重复声明：同一函数名不得在多个 hint 库中被声明。

(define-library (liii goldcheck type-hint owner)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii goldcheck path)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint util)
    (liii goldcheck type-hint hint-data)
    (liii goldcheck type-hint env)
  ) ;import
  (export check-a7 check-type-hint-duplicates)
  (begin

    ;; ---------- A7a 归属正确性 ----------

    ;; 实现库名 -> 提示信息
    (define (owner-message name owner-name)
      (string-append (symbol->string name)
        ": signature belongs to ("
        (syms-join owner-name)
        "); declare it in (hint "
        (syms-join owner-name)
        ") instead"
      ) ;string-append
    ) ;define

    ;; 再导出的过程不得在再导出库 hint 中声明签名。
    ;; 原库无本地 define 时，import 闭包中本地定义了 name 的库即为实现库；
    ;; 实现库有对应 hint 库时，签名应声明在实现库中。
    ;; 实现库没有 hint 库时放行（fallback 声明）；实现库不唯一时放行。
    (define (check-a7-owner rel oinfo closure roots e)
      (let* ((name (car e)) (line (list-ref e 2)))
        (if (assq name (lib-defines oinfo))
          '()
          (let ((impls (defining-libs name closure)))
            (if (or (null? impls) (> (length impls) 1))
              '()
              (check-a7-owner2 rel line name oinfo (cdr (car impls)) roots)
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (check-a7-owner2 rel line name oinfo owner roots)
      (let ((owner-name (lib-name owner)))
        (if (or (equal? owner-name (lib-name oinfo)) (not (hint-lib-file owner-name roots)))
          '()
          (list (list 'type-hint rel line (owner-message name owner-name)))
        ) ;if
      ) ;let
    ) ;define

    ;; ---------- A7b 库内重复声明 ----------

    ;; 同一 hint 变量被多次 define
    (define (check-a7-dupdef rel info)
      (let loop
        ((vs (hint-var-defs info)) (acc '()))
        (if (null? vs)
          (reverse acc)
          (let* ((d (car vs)) (name (def-name d)) (rest (cdr vs)))
            (if (memq name (map def-name rest))
              (loop rest (cons (dupdef-violation rel d name) acc))
              (loop rest acc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (dupdef-violation rel d name)
      (list 'type-hint
        rel
        (def-line d)
        (string-append (symbol->string name) ": duplicate definition of hint variable")
      ) ;list
    ) ;define

    ;; 同一函数名在查询函数中被重复登记
    (define (check-a7-dupreg rel info)
      (let loop
        ((es (registered-entries info (query-func-name (lib-name info)))) (acc '()))
        (if (null? es)
          (reverse acc)
          (let* ((e (car es)) (name (car e)) (rest (cdr es)))
            (if (memq name (map car rest))
              (loop rest (cons (dupreg-violation rel e name) acc))
              (loop rest acc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (dupreg-violation rel e name)
      (list 'type-hint
        rel
        (list-ref e 2)
        (string-append (symbol->string name)
          ": duplicate registration in query function"
        ) ;string-append
      ) ;list
    ) ;define

    (define (check-a7 rel info oinfo roots)
      (append (check-a7-dupdef rel info)
        (check-a7-dupreg rel info)
        (if (not oinfo) '() (check-a7-owners rel info oinfo roots))
      ) ;append
    ) ;define

    (define (check-a7-owners rel info oinfo roots)
      (let ((closure (source-closure oinfo roots))
            (entries (registered-entries info (query-func-name (lib-name info))))
           ) ;
        (apply append
          (map (lambda (e) (check-a7-owner rel oinfo closure roots e)) entries)
        ) ;apply
      ) ;let
    ) ;define

    ;; ---------- A7c 跨库重复声明 ----------

    ;; hint 库中登记的函数名列表
    (define (hint-registered-names hint-file)
      (let ((info (parse-library-file hint-file)))
        (if (not info)
          '()
          (map car (registered-entries info (query-func-name (lib-name info))))
        ) ;if
      ) ;let
    ) ;define

    ;; 声明了 name 的其它 hint 库
    (define (duplicate-hint-files name tables self)
      (filter
        (lambda (t) (and (not (string=? (car t) self)) (memq name (cdr t))))
        tables
      ) ;filter
    ) ;define

    (define (duplicate-violation hint-file name others)
      (list
        (list 'type-hint
          (relativize-path hint-file)
          1
          (string-append (symbol->string name)
            ": signature declared in more than one hint library (also in "
            (join-strings (map relativize-path (map car others)) ", ")
            ")"
          ) ;string-append
        ) ;list
      ) ;list
    ) ;define

    (define (check-type-hint-duplicates hint-files)
      (let ((tables (map (lambda (f) (cons f (hint-registered-names f))) hint-files)))
        (apply append
          (map
            (lambda (t)
              (apply append
                (map
                  (lambda (name)
                    (let ((others (duplicate-hint-files name tables (car t))))
                      (if (null? others) '() (duplicate-violation (car t) name others))
                    ) ;let
                  ) ;lambda
                  (dedup (cdr t))
                ) ;map
              ) ;apply
            ) ;lambda
            tables
          ) ;map
        ) ;apply
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

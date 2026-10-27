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

;; type-hint 的 A 类校验（hint 库自身结构）与 B2 校验（与原库交叉的
;; 参数数量一致性，逐函数粒度）。

(define-library (liii goldcheck type-hint checks)
  (import (scheme base)
    (scheme file)
    (liii base)
    (liii list)
    (liii string)
    (liii goldcheck path)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint util)
    (liii goldcheck type-hint hint-data)
  ) ;import
  (export check-a1 check-a2 check-a3 check-var check-a5 check-b)
  (begin

    ;; A1 库名与文件路径对应
    (define (check-a1 rel hint-file info)
      (let ((expected (expected-lib-name hint-file)) (actual (lib-name info)))
        (if (and expected (not (equal? expected actual)))
          (list (list 'type-hint
                  rel
                  (lib-line info)
                  (string-append "library name ("
                    (syms-join actual)
                    ") does not match file path (expected ("
                    (syms-join expected)
                    "))"
                  ) ;string-append
                ) ;list
          ) ;list
          '()
        ) ;if
      ) ;let
    ) ;define

    ;; A2 必须导出约定命名的标准查询函数
    (define (check-a2 rel info qname)
      (if (memq qname (lib-exports info))
        '()
        (list (list 'type-hint
                rel
                (lib-line info)
                (string-append "standard query function "
                  (symbol->string qname)
                  " must be exported"
                ) ;string-append
              ) ;list
        ) ;list
      ) ;if
    ) ;define

    ;; A3 不得导入其他 hint 库
    (define (check-a3 rel info)
      (let loop
        ((imps (lib-imports info)) (acc '()))
        (if (null? imps)
          (reverse acc)
          (let ((imp (car imps)))
            (loop (cdr imps)
              (if (and (pair? imp) (eq? (car imp) 'hint))
                (cons (list 'type-hint
                        rel
                        (lib-line info)
                        (string-append "hint libraries must not import other hint libraries: ("
                          (syms-join imp)
                          ")"
                        ) ;string-append
                      ) ;list
                  acc
                ) ;cons
                acc
              ) ;if
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 在 def 记录列表中按名字查找
    (define (find-def name ds)
      (let loop
        ((ds ds))
        (cond ((null? ds) #f)
              ((eq? (def-name (car ds)) name) (car ds))
              (else (loop (cdr ds)))
        ) ;cond
      ) ;let
    ) ;define

    ;; A4+A6 单个 hint 变量：签名语法与重复签名
    (define (check-var rel d)
      (let ((sigs (signature-value d))
            (line (def-line d))
            (name (symbol->string (def-name d)))
           ) ;
        (if (not (proper-list? sigs))
          (list (list 'type-hint
                  rel
                  line
                  (string-append name ": signature data is not a proper list")
                ) ;list
          ) ;list
          (append
            (let loop
              ((ss sigs) (acc '()))
              (if (null? ss)
                (reverse acc)
                (loop (cdr ss)
                  (if (signature-ok? (car ss))
                    acc
                    (cons
                      (list 'type-hint
                        rel
                        line
                        (string-append name ": invalid signature " (object->string (car ss)))
                      ) ;list
                      acc
                    ) ;cons
                  ) ;if
                ) ;loop
              ) ;if
            ) ;let
            (let has-dup
              ((ss sigs))
              (cond ((null? ss) '())
                    ((member (car ss) (cdr ss))
                     (list (list 'type-hint rel line (string-append name ": duplicate signatures")))
                    ) ;
                    (else (has-dup (cdr ss)))
              ) ;cond
            ) ;let
          ) ;append
        ) ;if
      ) ;let
    ) ;define

    ;; A5 查询函数 case 分支与 hint 变量定义一一对应
    (define (check-a5 rel vars entries query-exists?)
      (let ((var-names (map def-name vars)) (entry-vars (dedup (map cadr entries))))
        (append
          ;; 登记引用的变量必须有定义
          (let loop
            ((evs entry-vars) (acc '()))
            (if (null? evs)
              (reverse acc)
              (loop (cdr evs)
                (if (memq (car evs) var-names)
                  acc
                  (cons
                    (list 'type-hint
                      rel
                      1
                      (string-append (symbol->string (car evs))
                        " registered in query but not defined"
                      ) ;string-append
                    ) ;list
                    acc
                  ) ;cons
                ) ;if
              ) ;loop
            ) ;if
          ) ;let
          ;; 查询函数不存在时 A2 已报，不再重复报未登记
          (if (not query-exists?)
            '()
            (let loop
              ((vns var-names) (acc '()))
              (if (null? vns)
                (reverse acc)
                (loop (cdr vns)
                  (if (memq (car vns) entry-vars)
                    acc
                    (cons
                      (list 'type-hint
                        rel
                        (def-line (find-def (car vns) vars))
                        (string-append (symbol->string (car vns))
                          " defined but not registered in query"
                        ) ;string-append
                      ) ;list
                      acc
                    ) ;cons
                  ) ;if
                ) ;loop
              ) ;if
            ) ;let
          ) ;if
        ) ;append
      ) ;let
    ) ;define

    ;; ---------- B2：参数数量一致性（逐函数） ----------

    ;; 登记条目 var 对应变量的签名参数个数列表；变量未定义返回 #f
    (define (sig-arities var vars)
      (let ((d (find-def var vars)))
        (if (not d)
          #f
          (let ((sigs (signature-value d)))
            (if (proper-list? sigs)
              (let loop
                ((ss sigs) (acc '()))
                (if (null? ss)
                  (reverse acc)
                  (let ((n (signature-arity (car ss))))
                    (loop (cdr ss) (if n (cons n acc) acc))
                  ) ;let
                ) ;if
              ) ;let
              '()
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (arity-set numbers)
      (sort-numbers (dedup numbers))
    ) ;define

    ;; 单个登记条目与原库 def 的参数数量对比
    (define (check-b-entry rel e odefs vars)
      (let* ((name (car e)) (od (find-def name odefs)) (line (list-ref e 2)))
        (cond
          ;; 无本地 define（再导出或 s7 内置直通）：
          ;; 准确区分需要 hint-ref 来源追踪，初版跳过
          ((not od) '())
          ((eq? (def-kind od) 'lambda)
           (check-arity-mismatch rel
             line
             name
             (sig-arities (cadr e) vars)
             (list (def-arity od))
             "definition arity"
           ) ;check-arity-mismatch
          ) ;
          ((eq? (def-kind od) 'case-lambda)
           (check-arity-mismatch rel
             line
             name
             (sig-arities (cadr e) vars)
             (def-arity od)
             "case-lambda clauses"
           ) ;check-arity-mismatch
          ) ;
          (else '())
        ) ;cond
      ) ;let*
    ) ;define

    (define (check-arity-mismatch rel line name sc actual detail)
      (if
        (and sc (not (equal? (arity-set sc) (arity-set actual))))
        (list
          (list 'type-hint
            rel
            line
            (string-append (symbol->string name)
              ": signature arities "
              (join-strings (map number->string sc) " ")
              " do not match "
              detail
              " "
              (join-strings (map number->string actual) " ")
            ) ;string-append
          ) ;list
        ) ;list
        '()
      ) ;if
    ) ;define

    (define (check-b rel hint-file entries vars)
      (let ((orig (orig-lib-path hint-file)))
        (if (not (and orig (file-exists? orig)))
          '()
          (let ((oinfo (parse-library-file orig)))
            (if (not oinfo)
              '()
              (let ((odefs (lib-defines oinfo)))
                (let loop
                  ((es entries) (acc '()))
                  (if (null? es)
                    (reverse acc)
                    (loop (cdr es) (append acc (check-b-entry rel (car es) odefs vars)))
                  ) ;if
                ) ;let
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

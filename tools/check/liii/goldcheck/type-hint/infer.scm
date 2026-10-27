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

;; 文件级静态检查（spec 8.4 最小子集）：
;; 用原库 import 依赖的 hint 建立类型环境，检查函数体内过程调用的
;; 实参类型冲突。字面量与调用返回值参与推导，无法确定的记 unknown，
;; unknown 不报。只有确定的类型冲突才报告。

(define-library (liii goldcheck type-hint infer)
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
  (export check-infer)
  (begin

    ;; ---------- 类型兼容 ----------

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

    ;; ---------- 类型推导 ----------

    ;; 表达式 -> 类型（符号、 或 #f 表示 unknown）
    ;; 候选签名的公共返回类型：唯一则返回该类型，否则 #f
    (define (call-return-type sigs expr env)
      (let ((survivors (filter (lambda (s) (call-args-ok? s expr env)) sigs)))
        (if (null? survivors)
          #f
          (same-return-type
            (map
              (lambda (s) (let ((p (signature-parts s))) (if p (cadr p) #f)))
              survivors
            ) ;map
          ) ;same-return-type
        ) ;if
      ) ;let
    ) ;define

    (define (same-return-type rets)
      (let loop
        ((rs rets))
        (cond ((null? rs) (car rets))
              ((not (equal? (car rs) (car rets))) #f)
              (else (loop (cdr rs)))
        ) ;cond
      ) ;let
    ) ;define

    (define (infer-type expr env)
      (cond ((integer? expr) 'integer?)
            ((number? expr) 'number?)
            ((string? expr) 'string?)
            ((boolean? expr) 'boolean?)
            ((char? expr) 'char?)
            ((and (pair? expr) (eq? (car expr) 'quote) (pair? (cdr expr)))
             (let ((x (cadr expr)))
               (cond ((symbol? x) 'symbol?)
                     ((null? x) 'null?)
                     ((integer? x) 'integer?)
                     (else #f)
               ) ;cond
             ) ;let
            ) ;
            ((and (pair? expr) (symbol? (car expr)))
             (let ((sigs (assq (car expr) env)))
               (if (not sigs) #f (call-return-type (cdr sigs) expr env))
             ) ;let
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (call-args-ok? sig expr env)
      (let ((p (signature-parts sig)))
        (and p
          (= (length (car p)) (length (cdr expr)))
          (let loop
            ((params (car p)) (args (cdr expr)))
            (cond ((null? params) #t)
                  ((%accepts? (car params) (infer-type (car args) env))
                   (loop (cdr params) (cdr args))
                  ) ;
                  (else #f)
            ) ;cond
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    ;; ---------- 环境与检查 ----------

    (define (hint-root-of hint-file)
      (let loop
        ((pre '()) (rest (split-slash (normalize-sep hint-file))))
        (cond ((null? rest) #f)
              ((string=? (car rest) "hint")
               (join-strings (append (reverse pre) '("hint")) "/")
              ) ;
              (else (loop (cons (car rest) pre) (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 按局部 hint 根、仓库 goldfish/hint 的顺序查找。
    ;; 这样 tools/check 等目录可以只登记自身接口，复用基础库签名。
    ;; tools/<name>/hint/... 与同一源码根下的 goldfish/hint 对应。
    ;; bin/gf test 子进程中的 gfproject-root 未必指向仓库根，因此也从路径推导。
    (define (tool-goldfish-hint-root hint-file)
      (let loop
        ((pre '()) (rest (split-slash (normalize-sep hint-file))))
        (cond ((null? rest) #f)
              ((and (string=? (car rest) "tools")
                 (pair? (cdr rest))
                 (pair? (cddr rest))
                 (string=? (caddr rest) "hint")
               ) ;and
               (join-strings (append (reverse pre) '("goldfish" "hint")) "/")
              ) ;
              (else (loop (cons (car rest) pre) (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (hint-roots-of hint-file)
      (let ((local (hint-root-of hint-file)) (global (tool-goldfish-hint-root hint-file)))
        (append (if local (list local) '()) (if global (list global) '()))
      ) ;let
    ) ;define

    ;; 登记条目 -> (name . sigs) 绑定；变量未定义时值为 #f
    (define (entry->binding e vars)
      (let ((d (find-def (cadr e) vars)))
        (cons (car e) (if d (signature-value d) #f))
      ) ;let
    ) ;define

    ;; 解析后的 hint 信息 -> 类型环境条目列表
    (define (hint-env-entries info)
      (let* ((qname (query-func-name (lib-name info)))
             (vars (hint-var-defs info))
             (entries (registered-entries info qname))
            ) ;
        (filter
          (lambda (kv) (and (cdr kv) (list? (cdr kv))))
          (map (lambda (e) (entry->binding e vars)) entries)
        ) ;filter
      ) ;let*
    ) ;define

    ;; 加载单个 hint 库的登记 -> ((name . sigs) ...)
    (define (load-hint-env-from-roots lib-syms roots)
      (if (null? roots)
        '()
        (let ((path (string-append (car roots)
                      "/"
                      (join-strings (map symbol->string lib-syms) "/")
                      ".scm"
                    ) ;string-append
              ) ;path
             ) ;
          (if (file-exists? path)
            (let ((info (parse-library-file path)))
              (if info (hint-env-entries info) '())
            ) ;let
            (load-hint-env-from-roots lib-syms (cdr roots))
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (build-env oinfo hint-file)
      (let ((roots (hint-roots-of hint-file)))
        (apply append
          (map (lambda (lib) (load-hint-env-from-roots lib roots)) (lib-imports oinfo))
        ) ;apply
      ) ;let
    ) ;define

    ;; define 的体表达式列表
    (define (def-bodies d)
      (let ((e (def-expr d)))
        (cond
         ((and (pair? (cdr e)) (pair? (cadr e)) (symbol? (caadr e))) (cddr e))
         ((and (pair? (cdr e)) (pair? (cddr e)) (pair? (caddr e))) (list (caddr e)))
         (else '())
        ) ;cond
      ) ;let
    ) ;define

    ;; 递归遍历表达式，对环境内函数的调用做实参类型检查
    (define (walk rel expr env acc)
      (if (not (pair? expr))
        acc
        (let ((acc1
                (if (and (symbol? (car expr)) (assq (car expr) env))
                  (check-call rel expr env acc)
                  acc
                ) ;if
              ) ;acc1
             ) ;
          (if (eq? (car expr) 'quote)
            acc1
            (let loop
              ((l expr) (a acc1))
              (if (null? l) a (loop (cdr l) (walk rel (car l) env a)))
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 数量匹配的候选签名
    (define (arity-match-sigs sigs nargs)
      (filter
        (lambda (s) (let ((p (signature-parts s))) (and p (= (length (car p)) nargs))))
        sigs
      ) ;filter
    ) ;define

    ;; 签名第 idx（从 1 计）个参数类型；无签名部分时 any?
    (define (sig-param-type s idx)
      (let ((p (signature-parts s)))
        (if p (list-ref (car p) (- idx 1)) 'any?)
      ) ;let
    ) ;define

    ;; 保留第 idx 参数可接受 ti 的候选
    (define (pos-accept-sigs cands idx ti)
      (filter (lambda (s) (%accepts? (sig-param-type s idx) ti)) cands)
    ) ;define

    (define (check-call rel expr env acc)
      (let* ((f (car expr))
             (sigs (cdr (assq f env)))
             (alive (arity-match-sigs sigs (length (cdr expr))))
             (line (form-line expr))
            ) ;
        (let loop
          ((i 1) (args (cdr expr)) (cands alive))
          (if (or (null? args) (null? cands))
            acc
            (let* ((ti (infer-type (car args) env)) (ok-cands (pos-accept-sigs cands i ti)))
              (if (and ti (not (eq? ti 'unknown)) (null? ok-cands))
                (cons
                  (list 'type-hint
                    rel
                    line
                    (string-append "argument "
                      (number->string i)
                      " of "
                      (symbol->string f)
                      " requires "
                      (object->string (sig-param-type (car alive) i))
                      ", got "
                      (object->string ti)
                    ) ;string-append
                    (car args)
                  ) ;list
                  acc
                ) ;cons
                (loop (+ i 1) (cdr args) (if (null? ok-cands) cands ok-cands))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    ;; 遍历单个 define 的体表达式列表
    (define (walk-bodies orig-rel bodies env acc)
      (let loop
        ((bs bodies) (a acc))
        (if (null? bs) a (loop (cdr bs) (walk orig-rel (car bs) env a)))
      ) ;let
    ) ;define

    (define (check-infer hint-file entries)
      "只检查有 hint 的原函数自身；不递归检查其本地依赖函数。"
      (let ((orig (orig-lib-path hint-file)))
        (if (not (and orig (file-exists? orig)))
          '()
          (let ((oinfo (parse-library-file orig)))
            (if (not oinfo)
              '()
              (let ((env (build-env oinfo hint-file))
                    (orig-rel (relativize-path orig))
                    (hinted-names (map car entries))
                   ) ;
                (let loop
                  ((ds (lib-defines oinfo)) (acc '()))
                  (if (null? ds)
                    (reverse acc)
                    (loop (cdr ds)
                      (if (memq (def-name (car ds)) hinted-names)
                        (walk-bodies orig-rel (def-bodies (car ds)) env acc)
                        acc
                      ) ;if
                    ) ;loop
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

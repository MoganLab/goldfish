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

;; hint 库数据的提取与推导：名称约定、路径映射、签名语法。

(define-library (liii goldcheck type-hint hint-data)
  (import (scheme base)
    (liii base)
    (liii list)
    (liii string)
    (liii goldcheck path)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint util)
  ) ;import
  (export quote-marker? query-func-name expected-lib-name orig-lib-path
    hint-var-defs signature-value query-define registered-entries type-ok?
    signature-parts signature-ok? signature-arity
  ) ;export
  (begin

    ;; s7 的 read 对 'X 产生 #_quote 标记，用 object->string 识别
    (define (quote-marker? x)
      (or (eq? x 'quote) (string=? (object->string x) "#_quote"))
    ) ;define

    ;; 约定查询函数名：hint- + 原库名各部分用 / 连接
    ;; 原库名 = hint 库名去掉首部 hint；非 hint 开头时保守用全名
    (define (query-func-name lib-name-syms)
      (let ((orig
              (if (and (pair? lib-name-syms) (eq? (car lib-name-syms) 'hint))
                (cdr lib-name-syms)
                lib-name-syms
              ) ;if
            ) ;orig
           ) ;
        (string->symbol (string-append "hint-" (join-strings (map symbol->string orig) "/"))
        ) ;string->symbol
      ) ;let
    ) ;define

    ;; 路径组件中 "hint" 之前与之后的组件：(前缀 hint 后段)
    (define (hint-tail-comps comps)
      (let loop
        ((pre '()) (rest comps))
        (cond ((null? rest) #f)
              ((string=? (car rest) "hint") (cons (reverse pre) (cdr rest)))
              (else (loop (cons (car rest) pre) (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 路径 -> 预期库名：(hint liii clean)；无 hint 目录段时 #f
    (define (expected-lib-name hint-path)
      (let ((r (hint-tail-comps (split-slash (normalize-sep hint-path)))))
        (if r (cons 'hint (map string->symbol (map strip-scm (cdr r)))) #f)
      ) ;let
    ) ;define

    ;; hint 路径 -> 原库路径：删除路径中的 "hint" 目录段
    (define (orig-lib-path hint-path)
      (let ((r (hint-tail-comps (split-slash (normalize-sep hint-path)))))
        (if r (join-strings (append (car r) (cdr r)) "/") #f)
      ) ;let
    ) ;define

    ;; hint 签名变量：value define 且名字 hint 前缀（含 hint+、hint= 等）
    (define (hint-var-defs info)
      (filter
        (lambda (d)
          (and (eq? (def-kind d) 'value)
            (string-starts-with? (symbol->string (def-name d)) "hint")
          ) ;and
        ) ;lambda
        (lib-defines info)
      ) ;filter
    ) ;define

    ;; 变量 define 的签名数据：兼容 (quote X) 与直接列表
    (define (signature-value d)
      (let ((v (caddr (def-expr d))))
        (if (and (pair? v) (pair? (cdr v)) (quote-marker? (car v))) (cadr v) v)
      ) ;let
    ) ;define

    ;; case 表达式 -> 登记条目 ((name var line) ...)
    (define (case-entries expr)
      (let loop
        ((clauses (cdr expr)) (acc '()))
        (if (null? clauses)
          (reverse acc)
          (let* ((c (car clauses))
                 (head
                   (if (and (pair? c) (pair? (cdr c))) (car c) #f)
                 ) ;head
                 (var
                   (if (and (pair? c) (pair? (cdr c))) (cadr c) #f)
                 ) ;var
                ) ;
            (if
              (and (pair? head) (symbol? var) (not (eq? (car head) 'else)))
              (loop (cdr clauses)
                (append acc (map (lambda (n) (list n var (form-line c))) head))
              ) ;loop
              (loop (cdr clauses) acc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 按约定名查找查询函数的 def 记录
    (define (query-define info qname)
      (let loop
        ((ds (lib-defines info)))
        (cond ((null? ds) #f)
              ((eq? (def-name (car ds)) qname) (car ds))
              (else (loop (cdr ds)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 查询函数体中首个 (case name ...) 的登记条目
    (define (registered-entries info qname)
      (let ((qd (query-define info qname)))
        (let find-case
          ((body (if qd (cddr (def-expr qd)) #f)))
          (cond ((not body) '())
                ((null? body) '())
                ((and (pair? (car body)) (eq? (caar body) 'case)) (case-entries (car body)))
                (else (find-case (cdr body)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; ---------- 签名语法 ----------

    ;; 类型：符号或 (or #f T ...)
    (define (type-ok? t)
      (cond ((symbol? t) #t)
            ((and (pair? t) (eq? (car t) 'or) (proper-list? t))
             (every (lambda (m) (or (symbol? m) (boolean? m))) (cdr t))
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    ;; 签名 -> (参数类型列表 返回类型)；语法坏返回 #f
    (define (signature-parts sig)
      (let loop
        ((l sig) (params '()))
        (cond ((null? l) #f)
              ((eq? (car l) '->)
               (let ((ret (cdr l)))
                 (and (= (length ret) 1) (list (reverse params) (car ret)))
               ) ;let
              ) ;
              (else (loop (cdr l) (cons (car l) params)))
        ) ;cond
      ) ;let
    ) ;define

    (define (signature-ok? sig)
      (and (proper-list? sig)
        (let ((parts (signature-parts sig)))
          (and parts (every type-ok? (car parts)) (type-ok? (cadr parts)))
        ) ;let
      ) ;and
    ) ;define

    ;; 单条签名 -> -> 前参数个数；语法坏返回 #f
    (define (signature-arity sig)
      (let ((parts (signature-parts sig)))
        (if parts (length (car parts)) #f)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

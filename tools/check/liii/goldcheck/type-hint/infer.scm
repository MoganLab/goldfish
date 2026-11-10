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
    (liii goldcheck type-hint narrow)
    (liii goldcheck type-hint env)
    (liii goldcheck type-hint compat)
    (liii goldcheck type-hint value)
    (liii goldcheck type-hint type)
  ) ;import
  (export check-infer check-file-infer walk-bodies)
  (begin

    ;; ---------- 环境与检查 ----------

    ;; let 绑定列表 -> 局部类型绑定表
    ;; 绑定形如 (name value)：name 取 (car b)，value 取 (cadr b)。
    ;; sequential? 为 #t（let*）时按顺序推导，后面的绑定可见前面的结果；
    ;; 否则（普通 let、命名 let）为并行语义，只能看到外层的 locals。
    (define (let-bindings bindings env locals sequential?)
      (let loop
        ((bs bindings) (acc locals))
        (if (null? bs)
          acc
          (let* ((b (car bs))
                 (name (and (pair? b) (symbol? (car b)) (car b)))
                 (value (and (pair? (cdr b)) (cadr b)))
                 (base (if sequential? acc locals))
                 (type (and name (infer-type value env base)))
                ) ;
            (loop (cdr bs) (if (and name type) (cons (cons name type) acc) acc))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; when/unless 形式：when 的 body 按 test 为真收窄；
    ;; unless 的 body 按 test 为假（不收窄，保守）检查，但
    ;; unless 常用作前置校验（body 为 error/return），其后的
    ;; 序列代码按 test 全部成立收窄——这由 walk-body-seq 处理。
    ;; 返回 (violations . 收窄后 locals) 供序列传播。
    (define (walk-when-unless rel expr env locals acc head)
      (let* ((test (cadr expr))
             (body (cddr expr))
             (acc1 (walk rel test env locals acc))
             (locals1 (if (eq? head 'when) (apply-assumptions locals (test-assumptions test)) locals)
             ) ;locals1
             (exit-only? (and (eq? head 'unless) (unless-exits? body)))
             (locals-after (if exit-only? (apply-assumptions locals (test-assumptions test)) locals1)
             ) ;locals-after
            ) ;
        (cons (walk-body rel body env locals1 acc1) locals-after)
      ) ;let*
    ) ;define

    ;; unless body 是否全部为退出控制（error/exit/返回）：
    ;; 前置校验惯用法，后续序列代码处于测试成立的分支
    (define (unless-exits? body)
      (and (pair? body)
        (pair? (car body))
        (memq (car (car body)) '(error exit raise))
      ) ;and
    ) ;define

    ;; let 形式：绑定局部类型后在体内继续检查
    (define (walk-let rel expr env locals acc sequential?)
      (let* ((raw (cadr expr))
             (named? (and (symbol? raw) (pair? (cdr expr))))
             (bindings (if named? (caddr expr) raw))
             (body (if named? (cdddr expr) (cddr expr)))
             (acc1 (walk rel bindings env locals acc))
             (new-locals (let-bindings bindings env locals sequential?))
            ) ;
        (walk-body rel body env new-locals acc1)
      ) ;let*
    ) ;define

    ;; 递归遍历表达式，对环境内函数的调用做实参类型检查
    (define (walk rel expr env locals acc)
      (if (not (pair? expr))
        acc
        (let ((acc1
                (if (and (symbol? (car expr)) (assq (car expr) env))
                  (check-call rel expr env locals acc)
                  acc
                ) ;if
              ) ;acc1
             ) ;
          (cond ((eq? (car expr) 'quote) acc1)
                ((eq? (car expr) 'if) (walk-if walk rel expr env locals acc1))
                ((eq? (car expr) 'and) (walk-and walk rel expr env locals acc1))
                ((eq? (car expr) 'cond) (walk-cond walk walk-body rel expr env locals acc1))
                ;; when/unless 的 body 位于 test 的真/假分支，
                ;; 收窄信息由序列层（walk-body）传播，此处取违规部分
                ((memq (car expr) '(when unless))
                 (car (walk-when-unless rel expr env locals acc1 (car expr)))
                ) ;
                ((and (eq? (car expr) 'let) (pair? (cdr expr)) (pair? (cddr expr)))
                 (walk-let rel expr env locals acc #f)
                ) ;
                ((and (eq? (car expr) 'let*) (pair? (cdr expr)) (pair? (cddr expr)))
                 (walk-let rel expr env locals acc #t)
                ) ;
                (else
                  ;; 源码中可能存在非严格尾（如宏模板的点对
                  ;; (a . rest)），cdr 不是 pair 时停止迭代
                  (let loop
                    ((l expr) (a acc1))
                    (if (not (pair? l)) a (loop (cdr l) (walk rel (car l) env locals a)))
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; 遍历表达式序列；非 pair 尾停止（同 walk 的点对防护）。
    ;; (unless <test> (error ...)) 前置校验之后的表达式按 test 成立收窄。
    (define (walk-body rel exprs env locals acc)
      (let loop
        ((l exprs) (ls locals) (a acc))
        (if (not (pair? l))
          a
          (let ((r (walk-expr rel (car l) env ls a)))
            (loop (cdr l) (cdr r) (car r))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 单个表达式 -> (acc . 收窄后 locals)：when/unless 返回收窄信息，
    ;; 其余形式 locals 不变
    (define (walk-expr rel e env locals acc)
      (if (and (pair? e) (memq (car e) '(when unless)))
        (walk-when-unless rel e env locals acc (car e))
        (cons (walk rel e env locals acc) locals)
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

    ;; 关键字参数调用：实参与形参的位置对应关系不确定
    ;; （define* 的可选参数既可按位置传入，也可按关键字传入），
    ;; 保守跳过实参类型检查，避免误报。
    (define (call-has-keyword? expr)
      (and (proper-list? (cdr expr))
        (let loop
          ((args (cdr expr)))
          (cond ((null? args) #f)
                ((keyword? (car args)) #t)
                (else (loop (cdr args)))
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    ;; 构造一条实参类型冲突违规：定位到最小责任表达式（spec 8.8）
    (define (arg-violation rel line f i expected-type ti arg)
      (list 'type-hint
        rel
        line
        (string-append "argument "
          (number->string i)
          " of "
          (symbol->string f)
          " requires "
          (object->string expected-type)
          ", got "
          (object->string ti)
        ) ;string-append
        arg
      ) ;list
    ) ;define

    (define (check-call rel expr env locals acc)
      ;; 点对尾的调用形式（如宏模板 ((< . gens) ...)）不做检查
      (if
        (or (not (proper-list? (cdr expr)))
          (inline-assq-access? expr)
          (call-has-keyword? expr)
        ) ;or
        acc
        (let* ((f (car expr))
               (sigs (cdr (assq f env)))
               (alive (arity-match-sigs sigs (length (cdr expr))))
               (line (form-line expr))
              ) ;
          (let loop
            ((i 1) (args (cdr expr)) (cands alive))
            (if (or (null? args) (null? cands))
              acc
              (let* ((ti (infer-type (car args) env locals))
                     (ok-cands (pos-accept-sigs cands i ti))
                    ) ;
                (if (and ti (not (eq? ti 'unknown)) (null? ok-cands))
                  (cons (arg-violation rel line f i (sig-param-type (car alive) i) ti (car args))
                    acc
                  ) ;cons
                  (loop (+ i 1) (cdr args) (if (null? ok-cands) cands ok-cands))
                ) ;if
              ) ;let*
            ) ;if
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    ;; 遍历单个 define 的体表达式列表
    (define (walk-bodies orig-rel bodies env acc)
      (let loop
        ((bs bodies) (a acc))
        (if (null? bs) a (loop (cdr bs) (walk-bodies-one orig-rel (car bs) env a)))
      ) ;let
    ) ;define

    (define (walk-bodies-one orig-rel body env acc)
      (walk orig-rel body env '() acc)
    ) ;define

    ;; 逐 define 遍历函数体，收集实参类型冲突
    (define (walk-defines rel ds env acc)
      (let loop
        ((rest ds) (a acc))
        (if (null? rest)
          (reverse a)
          (loop (cdr rest) (walk-bodies rel (def-bodies (car rest)) env a))
        ) ;if
      ) ;let
    ) ;define

    (define (check-infer hint-file)
      "导出函数均有签名时检查全库，否则只检查有签名的函数。"
      (let ((orig (orig-lib-path hint-file)))
        (if (not (and orig (file-exists? orig)))
          '()
          (let ((oinfo (parse-library-file orig)))
            (if (not oinfo)
              '()
              (let* ((env (build-env oinfo hint-file))
                     (defs
                       (if (full-coverage? env oinfo)
                         (lib-defines oinfo)
                         (filter (lambda (d) (assq (def-name d) env)) (lib-defines oinfo))
                       ) ;if
                     ) ;defs
                    ) ;
                (walk-defines (relativize-path orig) defs env '())
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 任意源码文件的检查入口（spec 8.3/8.4）：
    ;; 按文件 import 建立类型环境，检查全部 define 的函数体。
    ;; hint 库自身与已有对应 hint 的原库由 check-infer 覆盖，这里跳过
    ;; 以免重复报告；本地 define 的名字从环境中剔除（定义遮蔽导入）。
    (define (%import-env info)
      (let* ((env (build-import-env (lib-imports info)))
             (own-names (map def-name (lib-defines info)))
            ) ;
        (filter
          (lambda (kv) (not (memq (car kv) own-names)))
          env
        ) ;filter
      ) ;let*
    ) ;define

    (define (check-file-infer file)
      "按 import 依赖的 hint 检查任意 define-library 源码文件。"
      (let ((info (cached-library-info file)))
        (if (not info)
          '()
          (let ((name (lib-name info)))
            (if
              (or (and (pair? name) (eq? (car name) 'hint))
                (hint-lib-file name (all-hint-roots))
              ) ;or
              '()
              (walk-defines (relativize-path file) (lib-defines info) (%import-env info) '())
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

;;
;; Copyright (C) 2026 Liii Network
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

;; md-stem 反向转换的输入契约：校验文档头、块、行内、列表和表格节点的
;; 形状、必填字段、枚举值及字符串参数，集中产生带节点类型的 md-stem-error。
;; 仅被 (liii cmark markdown ...) 内部模块使用，不对最终用户开放。
(define-library (liii cmark markdown validate)
  (import (scheme base))
  (import (liii error))
  (import (liii cmark markdown util))
  (export check-md-stem-document check-block-node check-inline-node
    check-string-field check-list-node check-table-node
  ) ;export
  (begin
    ;; ---- 基础断言 ----
    (define (%expect-string node field value)
      (unless (string? value)
        (md-stem-error (node-type node)
          (string-append "字段 " field " 必须是字符串，实际是: ")
          value
        ) ;md-stem-error
      ) ;unless
    ) ;define

    (define (%expect-nonneg-integer node field value)
      (unless (and (integer? value) (>= value 0))
        (md-stem-error (node-type node)
          (string-append "字段 " field " 必须是非负整数，实际是: ")
          value
        ) ;md-stem-error
      ) ;unless
    ) ;define

    (define (%expect-symbol-in node field value allowed)
      (unless (and (symbol? value) (memq value allowed))
        (md-stem-error (node-type node)
          (string-append "字段 " field " 必须是 ")
          allowed
          (string-append " 之一，实际是: ")
          value
        ) ;md-stem-error
      ) ;unless
    ) ;define

    ;; 行内内容项：纯字符串或合法行内节点
    (define (%expect-children-type node children)
      (let loop
        ((rem children))
        (cond ((null? rem) #t)
              ((string? (car rem)) (loop (cdr rem)))
              ((inline-node? (car rem)) (loop (cdr rem)))
              (else (md-stem-error (node-type node)
                      "行内内容中出现了非行内节点: "
                      (car rem)
                    ) ;md-stem-error
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; md-stem 节点必须是 (符号 . 内容) 形式的非空列表
    (define (%expect-node-shape node what)
      (unless (and (pair? node) (symbol? (car node)) (list? node))
        (md-stem-error 'md-stem
          (string-append what
            " 必须是 (类型 . 内容) 形式的列表，实际是: "
          ) ;string-append
          node
        ) ;md-stem-error
      ) ;unless
    ) ;define

    (define (node-type node)
      (car node)
    ) ;define

    ;; ---- 文档层 ----
    ;; 文档必须是 (md-stem 块 ...)：头为 md-stem 且子节点全部是合法块
    (define (check-md-stem-document doc)
      (%expect-node-shape doc "文档")
      (unless (eq? (node-type doc) 'md-stem)
        (md-stem-error (node-type doc)
          "缺少 md-stem 文档头，文档必须以 (md-stem ...) 开始"
        ) ;md-stem-error
      ) ;unless
      (for-each check-block-node (cdr doc))
      #t
    ) ;define

    ;; ---- 块级节点 ----
    (define (%check-block-node node)
      (%expect-node-shape node "块节点")
      (case (node-type node)
            ((p h1 h2 h3 h4 h5 h6) (%expect-children-type node (cdr node)))
            ((thematic-break)
             (unless (null? (cdr node))
               (md-stem-error 'thematic-break
                 "thematic-break 不携带字段，实际字段数: "
                 (- (length node) 1)
               ) ;md-stem-error
             ) ;unless
            ) ;
            ((soft-break line-break)
             (unless (null? (cdr node))
               (md-stem-error (node-type node)
                 "换行节点不携带字段，实际字段数: "
                 (- (length node) 1)
               ) ;md-stem-error
             ) ;unless
            ) ;
            ((code-block)
             (%expect-exact-arity node 3)
             (check-string-field node "info" (list-ref node 1))
             (check-string-field node "内容" (list-ref node 2))
            ) ;
            ((math-block)
             (%expect-exact-arity node 2)
             (check-string-field node "内容" (cadr node))
            ) ;
            ((raw-block)
             (%expect-exact-arity node 3)
             (check-string-field node "语言" (list-ref node 1))
             (check-string-field node "内容" (list-ref node 2))
            ) ;
            ((bullet-list ordered-list item blockquote alert) (check-list-node node))
            ((table) (check-table-node node))
            (else (md-stem-error (node-type node) "未知块节点类型: " (node-type node))
            ) ;else
      ) ;case
    ) ;define

    (define (check-block-node node)
      (%check-block-node node)
      #t
    ) ;define

    ;; 字段数量精确校验：节点总长度（含类型符号）必须等于 expected
    (define (%expect-exact-arity node expected)
      (unless (= (length node) expected)
        (md-stem-error (node-type node)
          (string-append "字段数量错误，期望 "
            (number->string (- expected 1))
            " 个字段，实际是 "
          ) ;string-append
          (- (length node) 1)
        ) ;md-stem-error
      ) ;unless
    ) ;define

    ;; ---- 列表与容器 ----
    (define (%check-list-node node)
      (case (node-type node)
            ((bullet-list)
             ;; (bullet-list tight/loose item ...)
             (unless (>= (length node) 2)
               (md-stem-error 'bullet-list "bullet-list 至少需要 tightness 字段")
             ) ;unless
             (%expect-symbol-in node "tightness" (cadr node) '(tight loose))
             (for-each check-block-node (cddr node))
            ) ;
            ((ordered-list)
             ;; (ordered-list start delimiter tight/loose item ...)
             (unless (>= (length node) 4)
               (md-stem-error 'ordered-list
                 "ordered-list 需要 start、delimiter、tightness 三个字段"
               ) ;md-stem-error
             ) ;unless
             (%expect-nonneg-integer node "start" (cadr node))
             (%expect-symbol-in node "delimiter" (list-ref node 2) '(period one-paren))
             (%expect-symbol-in node "tightness" (list-ref node 3) '(tight loose))
             (for-each check-block-node (list-tail node 4))
            ) ;
            ((item)
             ;; (item [checked|unchecked] 块 ...)
             (let ((rest (cdr node)))
               (cond
                ((and (pair? rest) (memq (car rest) '(checked unchecked)))
                 (for-each check-block-node (cdr rest))
                ) ;
                (else (for-each check-block-node rest))
               ) ;cond
             ) ;let
            ) ;
            ((blockquote) (for-each check-block-node (cdr node)))
            ((alert)
             ;; (alert note|tip|important|warning|caution 块 ...)
             (unless (>= (length node) 2)
               (md-stem-error 'alert "alert 需要 kind 字段")
             ) ;unless
             (%expect-symbol-in node
               "kind"
               (cadr node)
               '(note tip important warning caution)
             ) ;%expect-symbol-in
             (for-each check-block-node (cddr node))
            ) ;
            (else (md-stem-error (node-type node) "未知容器节点类型: " (node-type node))
            ) ;else
      ) ;case
    ) ;define

    (define (check-list-node node)
      (%check-list-node node)
      #t
    ) ;define

    ;; ---- 表格 ----
    ;; (table (align 对齐 ...) (thead (tr 单元格 ...)) (tbody (tr 单元格 ...) ...))
    (define (%check-table-node node)
      (%expect-exact-arity node 4)
      (let ((align (list-ref node 1)) (thead (list-ref node 2)) (tbody (list-ref node 3)))
        ;; 对齐行
        (unless (and (pair? align) (eq? (car align) 'align) (list? align))
          (md-stem-error 'table "table 第二个字段必须是 (align ...) 列表")
        ) ;unless
        (for-each (lambda (a) (%expect-symbol-in node "align" a '(default left
                                                                   center right)))
          (cdr align)
        ) ;for-each
        ;; 表头
        (unless (and (pair? thead) (eq? (car thead) 'thead) (list? thead) (= (length thead) 2))
          (md-stem-error 'table "table 第三个字段必须是 (thead (tr ...))")
        ) ;unless
        (check-table-rows 'thead (cadr thead))
        ;; 表体
        (unless (and (pair? tbody) (eq? (car tbody) 'tbody) (list? tbody))
          (md-stem-error 'table "table 第四个字段必须是 (tbody ...) 列表")
        ) ;unless
        (for-each (lambda (row) (check-table-rows 'tbody row)) (cdr tbody))
      ) ;let
    ) ;define

    (define (check-table-node node)
      (%check-table-node node)
      #t
    ) ;define

    (define (check-table-rows what rows)
      (let ((expected-cell (if (eq? what 'thead) 'th 'td)))
        (unless (and (pair? rows) (eq? (car rows) 'tr) (list? rows))
          (md-stem-error 'table
            (string-append "表头/表体行必须是 (tr ...)，出现在 "
              (symbol->string what)
            ) ;string-append
          ) ;md-stem-error
        ) ;unless
        (for-each
          (lambda (cell)
            (unless (and (pair? cell) (list? cell) (eq? (car cell) expected-cell))
              (md-stem-error 'table
                (string-append "表头单元格必须是 (th ...)、表体单元格必须是 (td ...)，出现在 "
                  (symbol->string what)
                ) ;string-append
              ) ;md-stem-error
            ) ;unless
            (%expect-children-type cell (cdr cell))
          ) ;lambda
          (cdr rows)
        ) ;for-each
      ) ;let
    ) ;define

    ;; ---- 行内节点 ----
    (define (inline-node? node)
      (and (pair? node)
        (symbol? (car node))
        (list? node)
        (memq (car node)
          '(em strong strikeout code link image soft-break line-break
             math-inline raw-inline emoji)
        ) ;memq
        #t
      ) ;and
    ) ;define

    (define (%check-inline-node node)
      (%expect-node-shape node "行内节点")
      (case (node-type node)
            ((em strong strikeout) (%expect-children-type node (cdr node)))
            ((code math-inline emoji)
             (%expect-exact-arity node 2)
             (check-string-field node "内容" (cadr node))
            ) ;
            ((soft-break line-break) (%expect-exact-arity node 1))
            ((link image)
             ;; (link url title 行内 ...) / (image url title alt ...)
             (unless (>= (length node) 3)
               (md-stem-error (node-type node)
                 "link/image 需要 url 与 title 两个前导字段"
               ) ;md-stem-error
             ) ;unless
             (check-string-field node "url" (cadr node))
             (check-string-field node "title" (list-ref node 2))
             (%expect-children-type node (cddr node))
            ) ;
            ((raw-inline)
             ;; (raw-inline 语言 内容)
             (%expect-exact-arity node 3)
             (check-string-field node "语言" (cadr node))
             (check-string-field node "内容" (list-ref node 2))
            ) ;
            (else (md-stem-error (node-type node) "未知行内节点类型: " (node-type node))
            ) ;else
      ) ;case
    ) ;define

    (define (check-inline-node node)
      (%check-inline-node node)
      #t
    ) ;define

    ;; 公开的字符串字段断言（供渲染器在取字段前复用）
    (define (check-string-field node field value)
      (%expect-string node field value)
      #t
    ) ;define
  ) ;begin
) ;define-library

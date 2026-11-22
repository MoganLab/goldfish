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

;; md-stem 反向渲染的容器层：渲染引用、Alert、有序/无序列表、任务项。
;; 只负责容器标记、空行、缩进和递归子块；通过 render-blocks 回调递归，
;; 不反向导入 blocks，消除容器与块级调度之间的循环依赖。
;; 仅被 (liii cmark markdown blocks) 使用，不对最终用户开放。
(define-library (liii cmark markdown containers)
  (import (scheme base))
  (import (liii cmark markdown util))
  (import (liii cmark markdown validate))
  (import (liii cmark markdown inline))
  (export render-container)
  (begin
    ;; 容器节点 → 行列表；render-blocks 为块级渲染回调。
    ;; 容器的每一层只负责自身前缀和空行，不产生额外尾随空行。
    (define (render-container node render-blocks)
      (case (car node)
            ((blockquote) (render-blockquote node render-blocks))
            ((alert) (render-alert node render-blocks))
            ((bullet-list) (render-bullet-list node render-blocks))
            ((ordered-list) (render-ordered-list node render-blocks))
            (else (md-stem-error (car node) "未知容器节点类型: " (car node)))
      ) ;case
    ) ;define

    ;; ---- 引用块 ----
    ;; > 行前缀；子块之间以空行分隔（空行也带 > 前缀保持引用结构）
    (define (render-blockquote node render-blocks)
      (prefix-lines "> " (interpose-blocks (cdr node) render-blocks))
    ) ;define

    ;; ---- Alert ----
    ;; > [!KIND] 首行 + 引用体
    (define (render-alert node render-blocks)
      (let ((marker (string-append "> [!" (alert-kind-label (cadr node)) "]")))
        (if (null? (cddr node))
          (list marker)
          (cons marker (prefix-lines "> " (interpose-blocks (cddr node) render-blocks)))
        ) ;if
      ) ;let
    ) ;define

    (define (alert-kind-label kind)
      (case kind
       ((note) "NOTE")
       ((tip) "TIP")
       ((important) "IMPORTANT")
       ((warning) "WARNING")
       ((caution) "CAUTION")
       (else (md-stem-error 'alert "未知 alert kind: " kind))
      ) ;case
    ) ;define

    ;; ---- 无序列表 ----
    ;; (bullet-list tight/loose item ...)
    (define (render-bullet-list node render-blocks)
      (render-list-items (cddr node) (cadr node) render-blocks)
    ) ;define

    ;; ---- 有序列表 ----
    ;; (ordered-list start delimiter tight/loose item ...)；
    ;; 编号从 start 起逐项递增
    (define (render-ordered-list node render-blocks)
      (render-numbered-items (list-tail node 4)
        (cadr node)
        (list-ref node 2)
        (list-ref node 3)
        render-blocks
      ) ;render-numbered-items
    ) ;define

    (define (render-numbered-items items start delimiter tightness render-blocks)
      (render-item-lists
        (let loop
          ((rem items) (n start) (acc '()))
          (if (null? rem)
            (reverse acc)
            (loop (cdr rem)
              (+ n 1)
              (cons (render-list-item (car rem)
                      (ordered-marker n delimiter)
                      tightness
                      render-blocks
                    ) ;render-list-item
                acc
              ) ;cons
            ) ;loop
          ) ;if
        ) ;let
        tightness
      ) ;render-item-lists
    ) ;define

    (define (ordered-marker n delimiter)
      (string-append (number->string n) (if (eq? delimiter 'one-paren) ") " ". "))
    ) ;define

    ;; ---- 列表项通用渲染 ----
    ;; marker 如 "- " / "1. "；后续行按 marker 宽度缩进
    (define (render-list-items items tightness render-blocks)
      (render-item-lists (map (lambda (item) (render-list-item item "- " tightness render-blocks)) items)
        tightness
      ) ;render-item-lists
    ) ;define

    ;; 各项行列表之间：loose 插入空行，tight 直接续接
    (define (render-item-lists item-line-lists tightness)
      (let ((sep (if (eq? tightness 'tight) '() (list ""))))
        (let loop
          ((rem item-line-lists) (acc '()) (first? #t))
          (cond ((null? rem) (apply append (reverse acc)))
                (first? (loop (cdr rem) (list (car rem)) #f))
                (else
                  (loop (cdr rem) (cons (append sep (car rem)) acc) #f)
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; 单个 item 节点 → 行列表
    (define (render-list-item item marker tightness render-blocks)
      (check-block-node item)
      (unless (eq? (car item) 'item)
        (md-stem-error 'item "列表子节点必须是 item，实际是: " (car item))
      ) ;unless
      (let* ((rest (cdr item))
             (task-status
               (if (and (pair? rest) (memq (car rest) '(checked unchecked))) (car rest) #f)
             ) ;task-status
             (blocks (if task-status (cdr rest) rest))
             (marker*
               (if task-status
                 (string-append marker (if (eq? task-status 'checked) "[x] " "[ ] "))
                 marker
               ) ;if
             ) ;marker*
            ) ;
        (let ((lines (render-item-blocks blocks tightness render-blocks)))
          (if (null? lines)
            (list marker*)
            (cons (string-append marker* (car lines))
              (indent-lines (string-length marker*) (cdr lines))
            ) ;cons
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    ;; 列表项内多个块：tight 无空行（纯换行续接），loose 块间空行
    (define (render-item-blocks blocks tightness render-blocks)
      (let ((sep (if (eq? tightness 'tight) '() (list ""))))
        (let loop
          ((rem blocks) (acc '()) (first? #t))
          (cond ((null? rem) (apply append (reverse acc)))
                (first?
                  (loop (cdr rem) (list (render-blocks (list (car rem)))) #f)
                ) ;first?
                (else
                  (loop (cdr rem) (cons (append sep (render-blocks (list (car rem)))) acc) #f)
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; ---- 子块间空行插入（引用/Alert 用） ----
    (define (interpose-blocks blocks render-blocks)
      (let loop
        ((rem blocks) (acc '()) (first? #t))
        (cond ((null? rem) (apply append (reverse acc)))
              (first?
                (loop (cdr rem) (list (render-blocks (list (car rem)))) #f)
              ) ;first?
              (else
                (loop (cdr rem) (cons (cons "" (render-blocks (list (car rem)))) acc) #f)
              ) ;else
        ) ;cond
      ) ;let
    ) ;define
  ) ;begin
) ;define-library

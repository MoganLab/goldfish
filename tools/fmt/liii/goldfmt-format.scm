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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldfmt-format)
  (export format-datum
          format-datum+node
          format-node
          format-string
          format-inline
          can-inline?
  ) ;export
  (import (liii base)
          (liii goldfmt-record)
          (liii goldfmt-rule)
          (liii goldfmt-scan)
  ) ;import

  (begin
    (define (write-to-string value)
      (let ((port (open-output-string)))
        (let-temporarily (((*s7* 'print-length) 9223372036854775807))
          (write value port)
        ) ;let-temporarily
        (get-output-string port)
      ) ;let
    ) ;define

    ;;; 检查是否为 quote 形式：(quote x) 或 (#_quote x)
    (define (quote-form? value)
      (and (pair? value)
           (not (null? value))
           (or (eq? (car value) 'quote)
               (and (syntax? (car value))
                    (string=? (object->string (car value) #f) "#_quote")
               ) ;and
           ) ;or
           (not (null? (cdr value)))
           (null? (cddr value))
       ) ;and
    ) ;define

    ;;; 将 quote 形式转换为字符串：'(quoted-content)
    (define (quote-form->string value)
      (let ((quoted-content (cadr value)))
        (if (pair? quoted-content)
            ; 如果是列表，递归处理内部元素
            (string-append "'" (format-inline-atom-or-quote quoted-content))
            ; 如果是 atom，使用 write-to-string
            (string-append "'" (format-inline-atom-or-quote quoted-content))
        ) ;if
      ) ;let
    ) ;define

    ;;; 格式化单个 atom 为字符串
    ;;; 注意：这里不处理 quote 形式，因为 quote 形式在 scan 阶段已经被识别为 env
    ;;; 内部的列表 (如 '(quote define) 中的 (quote define)) 应该作为普通列表输出
    (define (format-inline-atom-or-quote value)
      (write-to-string value)
    ) ;define

    (define (spaces n)
      (let loop ((i n) (result ""))
        (if (<= i 0)
            result
            (loop (- i 1) (string-append result " "))
        ) ;if
      ) ;let
    ) ;define

    ;;; 空 tag-name 的环境形如 ((x 1) (y 2))，第一个 child 前不能多输出空格。
    (define (child-separator parent child-index)
      (if (and (string=? (env-tag-name parent) "")
               (= child-index 0))
          ""
          " "
      ) ;if
    ) ;define

    (define (comment-node? node)
      (and (env? node)
           (string=? (env-tag-name node) "*comment*")
           (= (vector-length (env-children node)) 1)
           (let ((child (vector-ref (env-children node) 0)))
             (and (atom? child)
                  (string? (atom-value child))
             ) ;and
           ) ;let
      ) ;and
    ) ;define

    (define (comment-content node)
      (atom-value (vector-ref (env-children node) 0))
    ) ;define

    (define (format-comment-content content)
      (if (or (string=? content "")
              (char=? (string-ref content 0) #\space))
          (string-append ";;" content)
          (string-append ";; " content))
    ) ;define

    (define (contains-comment? node)
      (cond
        ((comment-node? node) #t)
        ((atom? node) #f)
        (else
          (let ((children (env-children node)))
            (let loop ((i 0))
              (cond
                ((>= i (vector-length children)) #f)
                ((contains-comment? (vector-ref children i)) #t)
                (else (loop (+ i 1)))
              ) ;cond
            ) ;let
          ) ;let
        ) ;else
      ) ;cond
    ) ;define

    ;;; 检查 env 是否为 quote 形式
    (define (quote-env? node)
      (and (env? node)
           (or (string=? (env-tag-name node) "quote")
               (string=? (env-tag-name node) "#_quote")
           ) ;or
       ) ;and
    ) ;define

    ;;; 从 quote env 的 value 中提取被引用的内容
    (define (quote-env-content node)
      (let ((value (env-value node)))
        (if (and (pair? value) (not (null? (cdr value))))
            (cadr value)
            '()))
    ) ;define

;;; format-inline 只做单行候选文本计算，不记录位置信息，也不写 writer。
    (define (format-inline node)
      (cond
        ((comment-node? node)
         (format-comment-content (comment-content node))
 ;
        ) ;
        ((atom? node)
         (format-inline-atom-or-quote (atom-value node))
 ;
        ) ;
        ((quote-env? node)
          ; quote 形式：'(content)
          ; 从 env-value 中提取被引用的内容并格式化
          (let ((content (quote-env-content node)))
            (string-append "'" (format-inline-atom-or-quote content)))
  ;
         ) ;
        (else
          (let ((children (env-children node)))
            (let loop ((i 0)
                       (result (string-append "(" (env-tag-name node))))
              (if (>= i (vector-length children))
                  (string-append result ")")
                  (let ((child (vector-ref children i)))
                    (loop (+ i 1)
                          (string-append result
                                         (child-separator node i)
                                         (format-inline child)
                          ) ;string-append
                    ) ;loop
                  ) ;let
              ) ;if
            ) ;let
          ) ;let
        ) ;else
      ) ;cond
    ) ;define

    (define (first-child-env? node)
      (let ((children (env-children node)))
        (and (> (vector-length children) 0)
             (env? (vector-ref children 0))
        ) ;and
      ) ;let
    ) ;define

    (define (can-inline? node)
      (cond
        ((comment-node? node) #t)
        ((atom? node) #t)
        ((contains-comment? node) #f)
        ((must-inline? (env-tag-name node)) #t)
        ((never-inline? (env-tag-name node)) #f)
        ((and (never-inline-when-first-child-env? (env-tag-name node))
              (first-child-env? node)
         ) ;and
         #f
 ;
        ) ;
        (else
          (<= (string-length (format-inline node)) max-inline-length)
        ) ;else
      ) ;cond
    ) ;define

    ;;; writer 状态记录当前输出端口、行号和列号。行号从 1 开始，列号从 0 开始。
    (define (make-writer initial-column)
      (vector (open-output-string) 1 initial-column)
    ) ;define

    (define (writer-port writer)
      (vector-ref writer 0)
    ) ;define

    (define (writer-line writer)
      (vector-ref writer 1)
    ) ;define

    (define (writer-column writer)
      (vector-ref writer 2)
    ) ;define

    (define (set-writer-line! writer line)
      (vector-set! writer 1 line)
    ) ;define

    (define (set-writer-column! writer column)
      (vector-set! writer 2 column)
    ) ;define

    (define (writer-result writer)
      (get-output-string (writer-port writer))
    ) ;define

    (define (emit-string! writer text)
      (display text (writer-port writer))
      (set-writer-column!
        writer
        (+ (writer-column writer) (string-length text))
      ) ;set-writer-column!
    ) ;define

    (define (emit-newline! writer)
      (display "\n" (writer-port writer))
      (set-writer-line! writer (+ (writer-line writer) 1))
      (set-writer-column! writer 0)
    ) ;define

    (define (emit-spaces! writer n)
      (emit-string! writer (spaces n))
    ) ;define

    (define (selected-child pair)
      (car pair)
    ) ;define

    (define (selected-column pair)
      (cdr pair)
    ) ;define

    ;;; 选择父环境第一行中保留的 children。
    ;;; 返回值是 ((child . column) ...)；column 是 child 左括号实际所在列。
    (define (select-first-line-children node first-column)
      (let ((children (env-children node))
            (limit (first-line-limit (env-tag-name node)))
            (allow-child-env? (allow-first-line-child-env? (env-tag-name node))))
        (let loop ((i 0)
                   (column first-column)
                   (direct-env-count 0)
                   (result '()))
          (if (or (>= i (vector-length children))
                  (>= (length result) limit))
              (reverse result)
              (let*
                ((child (vector-ref children i))
                 (child-is-env? (env? child))
                 (next-direct-env-count
                   (if child-is-env?
                       (+ direct-env-count 1)
                       direct-env-count
                   ) ;if
                 ) ;next-direct-env-count
 ;
                ) ;
                (if (or (comment-node? child)
                        (and child-is-env? (not allow-child-env?))
                        (> next-direct-env-count 1))
                    (reverse result)
                    (let* ((separator (child-separator node i))
                           (child-column (+ column (string-length separator)))
                           (next-result (cons (cons child child-column) result)))
                      (if (can-inline? child)
                          (loop (+ i 1)
                                (+ child-column
                                   (string-length (format-inline child))
                                ) ;+
                                next-direct-env-count
                                next-result
                          ) ;loop
                          (reverse next-result)
                      ) ;if
                    ) ;let*
                ) ;if
              ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (selected-count selected)
      (length selected)
    ) ;define

    (define (first-selected-env-column selected)
      (cond
        ((null? selected) #f)
        ((env? (selected-child (car selected)))
         (selected-column (car selected))
 ;
        ) ;
        (else (first-selected-env-column (cdr selected)))
      ) ;cond
    ) ;define

    (define (by-first-rest-child-indent parent-indent rest-start children)
      (let ((first (vector-ref children rest-start)))
        (if (and (env? first)
                 (string=? (env-tag-name first) ""))
            (+ parent-indent 1)
            (+ parent-indent 2)
        ) ;if
      ) ;let
    ) ;define

    ;;; 计算第一行之后的 children 缩进。let 这类 body form 由规则表固定为 parent+2；
    ;;; cond 和空 tag-name 环境则优先和第一行的第一个子环境对齐。
    (define (next-line-child-indent parent parent-indent selected rest-start)
      (let ((children (env-children parent))
            (strategy (rest-indent (env-tag-name parent))))
        (cond
          ((eq? strategy 'align-to-first-selected-env)
           (let ((column (first-selected-env-column selected)))
             (if column
                 column
                 (by-first-rest-child-indent parent-indent rest-start children)
             ) ;if
           ) ;let
 ;
          ) ;
          ((eq? strategy 'parent-plus2)
           (+ parent-indent 2)
 ;
          ) ;
          (else
            (by-first-rest-child-indent parent-indent rest-start children)
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (positioned-atom node indent left-line right-line)
      (make-atom :depth (atom-depth node)
                 :indent indent
                 :left-line left-line
                 :right-line right-line
                 :value (atom-value node)
      ) ;make-atom
    ) ;define

    (define (positioned-env node indent children left-line right-line)
      (make-env :tag-name (env-tag-name node)
                :depth (env-depth node)
                :indent indent
                :children children
                :left-line left-line
                :right-line right-line
                :value (env-value node)
      ) ;make-env
    ) ;define

    ;;; emit-comment! 将 scan 阶段的 (*comment* "...") 还原为 ;; 注释行。
    (define (emit-comment! node writer column)
      (let*
        ((left-line (writer-line writer))
         (content-node (vector-ref (env-children node) 0))
         (content (atom-value content-node))
         (content-indent
           (if (string=? content "")
               (+ column 2)
               (+ column 3)
           ) ;if
         ) ;content-indent
 ;
        ) ;
        (emit-string! writer (format-comment-content content))
        (positioned-env node
                        column
                        (vector (positioned-atom content-node
                                                 content-indent
                                                 left-line
                                                 (writer-line writer))
                        ) ;vector
                        left-line
                        (writer-line writer)
        ) ;positioned-env
      ) ;let*
    ) ;define

    ;;; emit-inline! 输出单行节点，并返回写入了位置信息的新节点。
    ;;; 原始 node 保持不变，formatter 的布局结果只存在于返回的新树中。
    (define (emit-inline! node writer column)
      (cond
        ((comment-node? node)
         (emit-comment! node writer column)
 ;
        ) ;
        ((atom? node)
         (let ((left-line (writer-line writer)))
           (emit-string! writer (format-inline-atom-or-quote (atom-value node)))
           (positioned-atom node column left-line (writer-line writer))
         ) ;let
 ;
        ) ;
        ((quote-env? node)
          ; quote 形式：'(content)
          ; 从 env-value 中提取被引用的内容并格式化
          (let ((left-line (writer-line writer))
                (content (quote-env-content node)))
            (emit-string! writer "'")
            (emit-string! writer (format-inline-atom-or-quote content))
            (positioned-env node
                            column
                            (vector)
                            left-line
                            (writer-line writer)
            ) ;positioned-env
          ) ;let
  ;
         ) ;
         (else
          (let ((children (env-children node)))
            (let ((left-line (writer-line writer)))
              (emit-string! writer "(")
              (emit-string! writer (env-tag-name node))
              (let loop ((i 0)
                         (new-children '()))
                (if (>= i (vector-length children))
                    (begin
                      (emit-string! writer ")")
                      (positioned-env node
                                      column
                                      (list->vector (reverse new-children))
                                      left-line
                                      (writer-line writer)
                      ) ;positioned-env
                    ) ;begin
                    (begin
                      (emit-string! writer (child-separator node i))
                      (let
                        ((new-child
                           (emit-inline! (vector-ref children i)
                                         writer
                                         (writer-column writer)
                           ) ;emit-inline!
                         ) ;new-child
 ;
                        ) ;
                        (loop (+ i 1)
                              (cons new-child new-children)
                        ) ;loop
                      ) ;let
                    ) ;begin
                ) ;if
              ) ;let
            ) ;let
          ) ;let
        ) ;else
      ) ;cond
    ) ;define

    ;;; walk 是核心 DFS 入口：从当前行的 column 列输出 node，并返回带位置信息的新 node。
    (define (walk! node writer column)
      (cond
        ((comment-node? node)
         (emit-comment! node writer column)
 ;
        ) ;
        ((atom? node)
         (emit-inline! node writer column)
 ;
        ) ;
        ((can-inline? node)
         (emit-inline! node writer column)
 ;
        ) ;
        (else
          (walk-env! node writer column)
        ) ;else
      ) ;cond
    ) ;define

    ;;; walk-env! 处理跨行环境。它按 DFS 顺序写出文本，同时收集每个 child
    ;;; 的新节点。进入时记录左括号位置，退出时将具名右侧标记输出回同一列。
    (define (walk-env! node writer column)
      (let ((env-indent column)
            (left-line (writer-line writer)))
        (if (quote-env? node)
            ; quote 形式跨行：'(content)
            ; 从 env-value 中提取被引用的内容并格式化
            (let ((content (quote-env-content node)))
              (emit-string! writer "'")
              (emit-string! writer (format-inline-atom-or-quote content))
              (positioned-env node
                              env-indent
                              (vector)
                              left-line
                              (writer-line writer)
              ) ;positioned-env
            ) ;let
            ; 普通 env
            (begin
              (emit-string! writer "(")
              (emit-string! writer (env-tag-name node))
              (let* ((first-column (writer-column writer))
                     (selected (select-first-line-children node first-column))
                     (rest-start (selected-count selected))
                     (children (env-children node)))
                (let
                  ((new-children
                     (let loop-selected ((items selected)
                                         (index 0)
                                         (result '()))
                       (if (null? items)
                           result
                           (begin
                             (emit-string! writer (child-separator node index))
                             (let
                               ((new-child
                                  (walk! (selected-child (car items))
                                         writer
                                         (selected-column (car items))
                                  ) ;walk!
                                ) ;new-child
 ;
                               ) ;
                               (loop-selected (cdr items)
                                              (+ index 1)
                                              (cons new-child result)
                               ) ;loop-selected
                             ) ;let
                           ) ;begin
                       ) ;if
                     ) ;let
                   ) ;new-children
 ;
                  ) ;
                  (let
                    ((new-children
                       (if (< rest-start (vector-length children))
                           (let ((child-indent
                                   (next-line-child-indent node env-indent selected rest-start)))
                             (let loop-rest ((i rest-start)
                                             (result new-children))
                               (if (>= i (vector-length children))
                                   result
                                   (begin
                                     (emit-newline! writer)
                                     (emit-spaces! writer child-indent)
                                     (let
                                       ((new-child
                                          (walk! (vector-ref children i)
                                                 writer
                                                 child-indent
                                          ) ;walk!
                                        ) ;new-child
 ;
                                       ) ;
                                       (loop-rest (+ i 1)
                                                  (cons new-child result)
                                       ) ;loop-rest
                                     ) ;let
                                   ) ;begin
                               ) ;if
                             ) ;let
                           ) ;let
                           new-children
                       ) ;if
                     ) ;new-children
 ;
                     ) ;
                     (emit-newline! writer)
                     (emit-spaces! writer env-indent)
                     (emit-string! writer ") ;")
                     (emit-string! writer (env-tag-name node))
                     (positioned-env node
                                     env-indent
                                     (list->vector (reverse new-children))
                                     left-line
                                     (writer-line writer)
                     ) ;positioned-env
                   ) ;let
                 ) ;let
              ) ;let*
            ) ;begin
        ) ;if
      ) ;let
    ) ;define
  ) ;begin

    ;;; format-node 返回两个值：
    ;;; 1. 格式化后的文本
    ;;; 2. 带 indent/left-line/right-line 信息的新 node
    (define (format-node node column)
      (let ((writer (make-writer column)))
        (let ((new-node (walk! node writer column)))
          (values (writer-result writer) new-node)
        ) ;let
      ) ;let
    ) ;define

    (define (format-datum+node datum)
      (format-node (scan datum) 0)
    ) ;define

    (define (format-datum datum)
      (call-with-values
        (lambda () (format-datum+node datum))
        (lambda (text new-node) text)
      ) ;call-with-values
    ) ;define

    (define (read-all port)
      (let loop ((result '()))
        (let ((datum (read port)))
          (if (eof-object? datum)
              (reverse result)
              (loop (cons datum result))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (join-top-level pieces)
      (let loop ((rest pieces) (result ""))
        (cond
          ((null? rest) result)
          ((string=? result "") (loop (cdr rest) (car rest)))
          (else
           (loop (cdr rest)
                 (string-append result "\n" (car rest))
           ) ;loop
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;;; 检查表达式是否是 define 形式
    (define (define-form? datum)
      (and (pair? datum)
           (not (null? datum))
           (eq? (car datum) 'define)))

    ;;; 判断是否需要空行分隔
    ;;; 如果当前是 define 且不是文件的第一个表达式，则返回 #t
    (define (needs-blank-line? datum is-first-expr)
      (and (define-form? datum)
           (not is-first-expr)))

    ;;; 格式化顶层表达式列表，在需要时插入空行
    (define (format-top-level datums)
      (let loop ((rest datums)
                 (is-first #t)
                 (result '()))
        (if (null? rest)
            (reverse result)
            (let* ((datum (car rest))
                   (needs-blank (needs-blank-line? datum is-first))
                   (formatted (format-datum datum))
                   (new-result
                     (if needs-blank
                         (cons formatted (cons "" result))
                         (cons formatted result))))
              (loop (cdr rest) #f new-result)))))

    (define (format-string source)
      (let* ((port (open-input-string source))
             (datums (read-all port))
             (pieces (format-top-level datums)))
        (join-top-level pieces)
      ) ;let*
    ) ;define
) ;define-library

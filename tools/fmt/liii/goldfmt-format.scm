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
    format-nodes
    format-inline
    can-inline?
  ) ;export
  (import (liii base)
    (liii goldfmt-record)
    (liii goldfmt-rule)
    (liii goldfmt-scan)
    (srfi srfi-13)
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

    (define (string-contains-newline? text)
      (let loop
        ((i 0))
        (cond ((>= i (string-length text)) #f)
              ((char=? (string-ref text i) #\newline) #t)
              (else (loop (+ i 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (format-atom-value value)
      (if (raw-string-literal? value)
        (raw-string-literal-source value)
        (if (char-literal? value) (char-literal-source value) (write-to-string value))
      ) ;if
    ) ;define

    (define (single-arg-symbol-form? value name)
      (and (pair? value)
        (eq? (car value) name)
        (pair? (cdr value))
        (null? (cddr value))
      ) ;and
    ) ;define

    ;; ; 检查是否为 quote 形式：(quote x) 或 (#_quote x)
    (define (quote-form? value)
      (and (pair? value)
        (not (null? value))
        (or (eq? (car value) 'quote)
          (and (syntax? (car value)) (string=? (object->string (car value) #f) "#_quote"))
        ) ;or
        (not (null? (cdr value)))
        (null? (cddr value))
      ) ;and
    ) ;define

    ;; ; 检查是否为 S7 #_quote 语法对象形式（非符号 quote）
    (define (quote-syntax-form? value)
      (and (pair? value)
        (not (null? value))
        (syntax? (car value))
        (string=? (object->string (car value) #f) "#_quote")
        (not (null? (cdr value)))
        (null? (cddr value))
      ) ;and
    ) ;define

    ;; ; 将 quote 形式转换为字符串：'(quoted-content)
    (define (quote-form->string value)
      (let ((quoted-content (cadr value)))
        (if (pair? quoted-content)
          (string-append "'" (format-inline-atom-or-quote quoted-content))
          (string-append "'" (format-inline-atom-or-quote quoted-content))
        ) ;if
      ) ;let
    ) ;define

    ;; ; 格式化单个 atom 为字符串
    ;; ; 注意：这里不处理 quote 形式，因为 quote 形式在 scan 阶段已经被识别为 env
    ;; ; 内部的列表 (如 '(quote define) 中的 (quote define)) 应该作为普通列表输出
    (define (format-inline-atom-or-quote value)
      (format-reader-datum value)
    ) ;define

    (define (format-inline-atom-or-quote-at value indent)
      (format-reader-datum-at value indent)
    ) ;define

    (define (spaces n)
      (if (<= n 0) "" (make-string n #\space))
    ) ;define

    ;; ; 空 tag-name 的环境形如 ((x 1) (y 2))，第一个 child 前不能多输出空格。
    (define (child-separator parent child-index)
      (if (and (string=? (env-tag-name parent) "") (= child-index 0)) "" " ")
    ) ;define

    (define (comment-node? node)
      (and (env? node)
        (string=? (env-tag-name node) "*comment*")
        (= (vector-length (env-children node)) 1)
        (let ((child (vector-ref (env-children node) 0)))
          (and (atom? child) (string? (atom-value child)))
        ) ;let
      ) ;and
    ) ;define

    (define (newline-node? node)
      (and (env? node)
        (string=? (env-tag-name node) "*newline*")
        (= (vector-length (env-children node)) 1)
        (let ((child (vector-ref (env-children node) 0)))
          (and (atom? child) (number? (atom-value child)))
        ) ;let
      ) ;and
    ) ;define

    (define (newline-count node)
      (atom-value (vector-ref (env-children node) 0))
    ) ;define

    (define (make-newlines n)
      (if (<= n 1) "" (make-string (- n 1) #\newline))
    ) ;define

    (define (comment-content node)
      (atom-value (vector-ref (env-children node) 0))
    ) ;define

    (define (format-comment-content content)
      (if (or (string=? content "")
            (char=? (string-ref content 0) #\space)
            (char=? (string-ref content 0) #\;)
          ) ;or
        (string-append ";;" content)
        (string-append ";; " content)
      ) ;if
    ) ;define

    (define (contains-comment? node)
      (cond ((comment-node? node) #t)
            ((atom? node) #f)
            (else (let ((children (env-children node)))
                    (let loop
                      ((i 0))
                      (cond ((>= i (vector-length children)) #f)
                            ((contains-comment? (vector-ref children i)) #t)
                            (else (loop (+ i 1)))
                      ) ;cond
                    ) ;let
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; ; 检查 env 是否为 quote 形式
    (define (quote-env? node)
      (and (env? node)
        (or (string=? (env-tag-name node) "quote")
          (string=? (env-tag-name node) "#_quote")
        ) ;or
      ) ;and
    ) ;define

    (define (reader-prefix-env? node)
      (and (env? node)
        (or (string=? (env-tag-name node) "quasiquote")
          (string=? (env-tag-name node) "unquote")
          (string=? (env-tag-name node) "unquote-splicing")
        ) ;or
      ) ;and
    ) ;define

    ;; ; 从 quote env 的 value 中提取被引用的内容
    (define (quote-env-content node)
      (let ((value (env-value node)))
        (if (and (pair? value) (not (null? (cdr value)))) (cadr value) '())
      ) ;let
    ) ;define

    (define (raw-string-datum? datum)
      (and (pair? datum)
        (eq? (car datum) '*raw-string*)
        (pair? (cdr datum))
        (string? (cadr datum))
        (pair? (cddr datum))
        (string? (caddr datum))
        (null? (cdddr datum))
      ) ;and
    ) ;define

    (define (newline-marker-datum? datum)
      (and (pair? datum)
        (eq? (car datum) '*newline*)
        (pair? (cdr datum))
        (number? (cadr datum))
        (null? (cddr datum))
      ) ;and
    ) ;define

    (define (comment-datum? datum)
      (and (pair? datum)
        (eq? (car datum) '*comment*)
        (pair? (cdr datum))
        (string? (cadr datum))
        (null? (cddr datum))
      ) ;and
    ) ;define

    (define (reader-newlines count)
      (let loop
        ((i count) (result ""))
        (if (<= i 0) result (loop (- i 1) (string-append result "\n")))
      ) ;let
    ) ;define

    (define (reader-datum-contains-newline-marker? datum)
      (cond ((newline-marker-datum? datum) #t)
            ((comment-datum? datum) #t)
            ((pair? datum)
             (or (reader-datum-contains-newline-marker? (car datum))
               (reader-datum-contains-newline-marker? (cdr datum))
             ) ;or
            ) ;
            ((or (vector? datum) (byte-vector? datum))
             (let loop
               ((i 0))
               (cond ((>= i (vector-length datum)) #f)
                     ((reader-datum-contains-newline-marker? (vector-ref datum i)) #t)
                     (else (loop (+ i 1)))
               ) ;cond
             ) ;let
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (reader-head-name head)
      (cond ((symbol? head) (symbol->string head))
            ((syntax? head)
             (let ((name (object->string head #f)))
               (if (string=? name "#_quote") "quote" name)
             ) ;let
            ) ;
            (else "default")
      ) ;cond
    ) ;define

    (define (reader-rule-head? head)
      (or (symbol? head) (syntax? head))
    ) ;define

    (define (reader-form-like? datum)
      (pair? datum)
    ) ;define

    (define (reader-format-selected-item item column)
      (format-reader-datum-at item column)
    ) ;define

    (define (reader-selected-item item)
      (car item)
    ) ;define
    (define (reader-selected-column item)
      (cadr item)
    ) ;define
    (define (reader-selected-text item)
      (caddr item)
    ) ;define

    (define (reader-select-first-line-items head-name rest start-column)
      (let ((limit (first-line-limit head-name))
            (allow-child-env? (allow-first-line-child-env? head-name))
           ) ;
        (let loop
          ((current rest)
           (column start-column)
           (selected-count 0)
           (direct-env-count 0)
           (result '())
          ) ;
          (if (or (not (pair? current))
                (>= selected-count limit)
                (newline-marker-datum? (car current))
              ) ;or
            (reverse result)
            (let* ((item (car current))
                   (item-is-env? (reader-form-like? item))
                   (next-direct-env-count (if item-is-env? (+ direct-env-count 1) direct-env-count)
                   ) ;next-direct-env-count
                  ) ;
              (if (or (and item-is-env? (not allow-child-env?)) (> next-direct-env-count 1))
                (reverse result)
                (let* ((item-column (+ column 1))
                       (item-text (reader-format-selected-item item item-column))
                       (next-result (cons (list item item-column item-text) result))
                       (next-column (+ item-column (string-length item-text)))
                      ) ;
                  (if (string-contains-newline? item-text)
                    (reverse next-result)
                    (loop (cdr current)
                      next-column
                      (+ selected-count 1)
                      next-direct-env-count
                      next-result
                    ) ;loop
                  ) ;if
                ) ;let*
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (reader-skip-selected rest selected)
      (let loop
        ((current rest) (remaining (length selected)))
        (if (or (<= remaining 0) (not (pair? current)))
          current
          (loop (cdr current) (- remaining 1))
        ) ;if
      ) ;let
    ) ;define

    (define (reader-first-selected-form-column selected)
      (cond ((null? selected) #f)
            ((reader-form-like? (reader-selected-item (car selected)))
             (reader-selected-column (car selected))
            ) ;
            (else (reader-first-selected-form-column (cdr selected)))
      ) ;cond
    ) ;define

    (define (reader-by-first-rest-child-indent parent-indent rest)
      (if (and (pair? rest)
            (reader-form-like? (car rest))
            (let ((name (reader-head-name (car (car rest)))))
              (string=? name "")
            ) ;let
          ) ;and
        (+ parent-indent 1)
        (+ parent-indent 2)
      ) ;if
    ) ;define

    (define (reader-rest-indent head-name parent-indent selected rest)
      (let ((strategy (rest-indent head-name)))
        (cond ((eq? strategy 'align-to-first-selected-env)
               (let ((column (reader-first-selected-form-column selected)))
                 (if column column (reader-by-first-rest-child-indent parent-indent rest))
               ) ;let
              ) ;
              ((eq? strategy 'parent-plus2) (+ parent-indent 2))
              (else (reader-by-first-rest-child-indent parent-indent rest))
        ) ;cond
      ) ;let
    ) ;define

    (define (format-reader-vector-inline datum)
      (let ((prefix (if (byte-vector? datum) "#u8(" "#(")))
        (let loop
          ((i 0) (pieces '()))
          (if (>= i (vector-length datum))
            (string-append prefix (string-join (reverse pieces) " ") ")")
            (loop (+ i 1) (cons (format-reader-datum-inline (vector-ref datum i)) pieces))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (reader-vector-prefix datum)
      (if (byte-vector? datum) "#u8(" "#(")
    ) ;define

    (define (reader-vector->list datum)
      (let loop
        ((i 0) (result '()))
        (if (>= i (vector-length datum))
          (reverse result)
          (loop (+ i 1) (cons (vector-ref datum i) result))
        ) ;if
      ) ;let
    ) ;define

    (define (format-reader-vector-multiline datum indent)
      (let* ((prefix (reader-vector-prefix datum))
             (item-indent (+ indent (string-length prefix)))
             (close-marker (string-append "\n" (spaces indent) ") ;#"))
            ) ;
        (let loop
          ((items (reader-vector->list datum)) (pieces (list prefix)) (prefix-ready? #t))
          (if (null? items)
            (apply string-append (reverse (cons close-marker pieces)))
            (let ((item (car items)))
              (if (newline-marker-datum? item)
                (loop (cdr items)
                  (cons (spaces item-indent) (cons (reader-newlines (cadr item)) pieces))
                  #t
                ) ;loop
                (let ((item-text (string-trim (format-reader-datum-at item item-indent))))
                  (loop (cdr items)
                    (cons item-text
                      (if prefix-ready?
                        pieces
                        (cons (string-append "\n" (spaces item-indent)) pieces)
                      ) ;if
                    ) ;cons
                    #f
                  ) ;loop
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (format-reader-vector-at datum indent)
      (let ((candidate (format-reader-vector-inline datum)))
        (if (and (not (reader-datum-contains-newline-marker? datum))
              (not (string-contains-newline? candidate))
              (<= (+ indent (string-length candidate)) max-inline-length)
            ) ;and
          candidate
          (format-reader-vector-multiline datum indent)
        ) ;if
      ) ;let
    ) ;define

    (define (format-reader-vector datum)
      (format-reader-vector-at datum 0)
    ) ;define

    (define (format-reader-pair-inline datum)
      (let loop
        ((current datum) (pieces '()))
        (cond ((pair? current)
               (loop (cdr current) (cons (format-reader-datum-inline (car current)) pieces))
              ) ;
              ((null? current) (string-append "(" (string-join (reverse pieces) " ") ")"))
              (else (string-append "("
                      (string-join (reverse pieces) " ")
                      " . "
                      (format-reader-datum-inline current)
                      ")"
                    ) ;string-append
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (reader-append-selected result selected)
      (let loop
        ((items selected) (text result))
        (if (null? items)
          text
          (loop (cdr items) (string-append text " " (reader-selected-text (car items))))
        ) ;if
      ) ;let
    ) ;define

    (define (last-line-column text)
      (let loop
        ((i 0) (column 0))
        (if (>= i (string-length text))
          column
          (loop (+ i 1) (if (char=? (string-ref text i) #\newline) 0 (+ column 1)))
        ) ;if
      ) ;let
    ) ;define

    (define (reader-append-close result close-indent)
      (if (string-suffix? ";#" result)
        (string-append result "\n" (spaces close-indent) ")")
        (string-append result ")")
      ) ;if
    ) ;define

    (define (reader-append-rest current result rest-indent prefix-ready? close-indent)
      (cond ((pair? current)
             (let ((item (car current)))
               (if (newline-marker-datum? item)
                 (reader-append-rest (cdr current)
                   (string-append result (reader-newlines (cadr item)) (spaces rest-indent))
                   rest-indent
                   #t
                   close-indent
                 ) ;reader-append-rest
                 (reader-append-rest (cdr current)
                   (string-append result
                     (if prefix-ready? "" (string-append "\n" (spaces rest-indent)))
                     (format-reader-datum-at item
                       (if prefix-ready? (last-line-column result) rest-indent)
                     ) ;format-reader-datum-at
                   ) ;string-append
                   rest-indent
                   #f
                   close-indent
                 ) ;reader-append-rest
               ) ;if
             ) ;let
            ) ;
            ((null? current) (reader-append-close result close-indent))
            (else (reader-append-close (let* ((prefix (if prefix-ready? "" (string-append "\n" (spaces rest-indent))))
                                              (before-tail (string-append result prefix ". "))
                                             ) ;
                                         (string-append before-tail
                                           (format-reader-datum-at current (last-line-column before-tail))
                                         ) ;string-append
                                       ) ;let*
                    close-indent
                  ) ;reader-append-close
            ) ;else
      ) ;cond
    ) ;define

    (define (format-reader-pair-multiline datum indent)
      (if (not (pair? datum))
        (format-atom-value datum)
        (let* ((head (car datum))
               (head-name (reader-head-name head))
               (rule-head? (reader-rule-head? head))
               (head-text (if rule-head?
                            (format-reader-datum-inline head)
                            (format-reader-datum-at head (+ indent 1))
                          ) ;if
               ) ;head-text
               (result (string-append "(" head-text))
               (head-end-column (+ indent 1 (string-length head-text)))
               (rest (cdr datum))
               (selected (if rule-head?
                           (reader-select-first-line-items head-name rest head-end-column)
                           '()
                         ) ;if
               ) ;selected
               (after-selected (reader-skip-selected rest selected))
               (with-selected (reader-append-selected result selected))
               (body-indent (if rule-head?
                              (reader-rest-indent head-name indent selected after-selected)
                              (+ indent 1)
                            ) ;if
               ) ;body-indent
              ) ;
          (reader-append-rest after-selected with-selected body-indent #f indent)
        ) ;let*
      ) ;if
    ) ;define

    (define (format-reader-pair-at datum indent)
      (let ((candidate (format-reader-pair-inline datum)))
        (if (and (not (reader-datum-contains-newline-marker? datum))
              (not (string-contains-newline? candidate))
              (<= (+ indent (string-length candidate)) max-inline-length)
            ) ;and
          candidate
          (format-reader-pair-multiline datum indent)
        ) ;if
      ) ;let
    ) ;define

    (define (format-reader-pair datum)
      (format-reader-pair-at datum 0)
    ) ;define

    (define (format-reader-datum-inline datum)
      (cond ((raw-string-literal? datum) (raw-string-literal-source datum))
            ((char-literal? datum) (char-literal-source datum))
            ((raw-string-datum? datum) (cadr datum))
            ((comment-datum? datum) (format-comment-content (cadr datum)))
            ((single-arg-symbol-form? datum 'quasiquote)
             (string-append "`" (format-reader-datum-inline (cadr datum)))
            ) ;
            ((single-arg-symbol-form? datum 'unquote)
             (string-append "," (format-reader-datum-inline (cadr datum)))
            ) ;
            ((single-arg-symbol-form? datum 'unquote-splicing)
             (string-append ",@" (format-reader-datum-inline (cadr datum)))
            ) ;
            ((quote-syntax-form? datum)
             (string-append "'" (format-reader-datum-inline (cadr datum)))
            ) ;
            ((pair? datum) (format-reader-pair-inline datum))
            ((or (vector? datum) (byte-vector? datum)) (format-reader-vector-inline datum))
            (else (format-atom-value datum))
      ) ;cond
    ) ;define

    (define (format-reader-datum-at datum indent)
      (cond ((raw-string-literal? datum) (raw-string-literal-source datum))
            ((char-literal? datum) (char-literal-source datum))
            ((raw-string-datum? datum) (cadr datum))
            ((comment-datum? datum) (format-comment-content (cadr datum)))
            ((single-arg-symbol-form? datum 'quasiquote)
             (string-append "`" (format-reader-datum-at (cadr datum) (+ indent 1)))
            ) ;
            ((single-arg-symbol-form? datum 'unquote)
             (string-append "," (format-reader-datum-at (cadr datum) (+ indent 1)))
            ) ;
            ((single-arg-symbol-form? datum 'unquote-splicing)
             (string-append ",@" (format-reader-datum-at (cadr datum) (+ indent 2)))
            ) ;
            ((quote-syntax-form? datum)
             (string-append "'" (format-reader-datum-at (cadr datum) (+ indent 1)))
            ) ;
            ((pair? datum) (format-reader-pair-at datum indent))
            ((or (vector? datum) (byte-vector? datum))
             (format-reader-vector-at datum indent)
            ) ;
            (else (format-atom-value datum))
      ) ;cond
    ) ;define

    (define (format-reader-datum datum)
      (format-reader-datum-at datum 0)
    ) ;define

    ;; ; format-inline 只做单行候选文本计算，不记录位置信息，也不写 writer。
    (define (format-inline node)
      (cond ((comment-node? node) (format-comment-content (comment-content node)))
            ((newline-node? node) "")
            ((atom? node) (format-inline-atom-or-quote (atom-value node)))
            ((quote-env? node)
             (let ((content (quote-env-content node)))
               (string-append "'" (format-inline-atom-or-quote content))
             ) ;let
            ) ;
            ((reader-prefix-env? node) (format-inline-atom-or-quote (env-value node)))
            (else (let ((children (env-children node)))
                    (let ((out (open-output-string)))
                      (display "(" out)
                      (display (env-tag-name node) out)
                      (let loop
                        ((i 0))
                        (if (>= i (vector-length children))
                          (begin
                            (display ")" out)
                            (get-output-string out)
                          ) ;begin
                          (begin
                            (display (child-separator node i) out)
                            (display (format-inline (vector-ref children i)) out)
                            (loop (+ i 1))
                          ) ;begin
                        ) ;if
                      ) ;let
                    ) ;let
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (first-child-env? node)
      (let ((children (env-children node)))
        (and (> (vector-length children) 0) (env? (vector-ref children 0)))
      ) ;let
    ) ;define

    (define (can-inline? node)
      (cond ((comment-node? node) #t)
            ((newline-node? node) #f)
            ((atom? node) (not (string-contains-newline? (format-inline node))))
            ((contains-comment? node) #f)
            ((must-inline? (env-tag-name node)) #t)
            ((never-inline? (env-tag-name node)) #f)
            ((and (never-inline-when-first-child-env? (env-tag-name node))
               (first-child-env? node)
             ) ;and
             #f
            ) ;
            (else (let ((candidate (format-inline node)))
                    (and (not (string-contains-newline? candidate))
                      (<= (string-length candidate) max-inline-length)
                    ) ;and
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; ; writer 状态记录当前输出端口、行号和列号。行号从 1 开始，列号从 0 开始。
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
      (let loop
        ((i 0) (line (writer-line writer)) (column (writer-column writer)))
        (if (>= i (string-length text))
          (begin
            (set-writer-line! writer line)
            (set-writer-column! writer column)
          ) ;begin
          (if (char=? (string-ref text i) #\newline)
            (loop (+ i 1) (+ line 1) 0)
            (loop (+ i 1) line (+ column 1))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (emit-newline! writer)
      (display "\n" (writer-port writer))
      (set-writer-line! writer (+ (writer-line writer) 1))
      (set-writer-column! writer 0)
    ) ;define

    (define (emit-spaces! writer n)
      (if (> n 0)
        (begin
          (display (make-string n #\space) (writer-port writer))
          (set-writer-column! writer (+ (writer-column writer) n))
        ) ;begin
      ) ;if
    ) ;define

    (define (selected-child pair)
      (car pair)
    ) ;define

    (define (selected-column pair)
      (cdr pair)
    ) ;define

    ;; ; 选择父环境第一行中保留的 children。
    ;; ; 返回值是 ((child . column) ...)；column 是 child 左括号实际所在列。
    (define (select-first-line-children node first-column)
      (let ((children (env-children node))
            (limit (first-line-limit (env-tag-name node)))
            (allow-child-env? (allow-first-line-child-env? (env-tag-name node)))
           ) ;
        (let loop
          ((i 0) (column first-column) (direct-env-count 0) (result '()))
          (if (or (>= i (vector-length children)) (>= (length result) limit))
            (reverse result)
            (let* ((child (vector-ref children i))
                   (child-is-env? (env? child))
                   (next-direct-env-count (if child-is-env? (+ direct-env-count 1) direct-env-count)
                   ) ;next-direct-env-count
                  ) ;
              (if (or (comment-node? child)
                    (newline-node? child)
                    (and child-is-env? (not allow-child-env?))
                    (> next-direct-env-count 1)
                  ) ;or
                (reverse result)
                (let* ((separator (child-separator node i))
                       (child-column (+ column (string-length separator)))
                       (next-result (cons (cons child child-column) result))
                      ) ;
                  (if (can-inline? child)
                    (loop (+ i 1)
                      (+ child-column (string-length (format-inline child)))
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
      (cond ((null? selected) #f)
            ((env? (selected-child (car selected))) (selected-column (car selected)))
            (else (first-selected-env-column (cdr selected)))
      ) ;cond
    ) ;define

    (define (by-first-rest-child-indent parent-indent rest-start children)
      (let ((first (vector-ref children rest-start)))
        (if (and (env? first) (string=? (env-tag-name first) ""))
          (+ parent-indent 1)
          (+ parent-indent 2)
        ) ;if
      ) ;let
    ) ;define

    ;; ; 计算第一行之后的 children 缩进。let 这类 body form 由规则表固定为 parent+2；
    ;; ; cond 和空 tag-name 环境则优先和第一行的第一个子环境对齐。
    (define (next-line-child-indent parent parent-indent selected rest-start)
      (let ((children (env-children parent))
            (strategy (rest-indent (env-tag-name parent)))
           ) ;
        (cond ((eq? strategy 'align-to-first-selected-env)
               (let ((column (first-selected-env-column selected)))
                 (if column
                   column
                   (by-first-rest-child-indent parent-indent rest-start children)
                 ) ;if
               ) ;let
              ) ;
              ((eq? strategy 'parent-plus2) (+ parent-indent 2))
              (else (by-first-rest-child-indent parent-indent rest-start children))
        ) ;cond
      ) ;let
    ) ;define

    (define (positioned-atom node indent left-line right-line)
      #t
    ) ;define

    (define (positioned-env node indent children left-line right-line)
      #t
    ) ;define

    ;; ; emit-comment! 将 scan 阶段的 (*comment* "...") 还原为 ;; 注释行。
    (define (emit-comment! node writer column)
      (let* ((left-line (writer-line writer))
             (content-node (vector-ref (env-children node) 0))
             (content (atom-value content-node))
             (content-indent (if (string=? content "") (+ column 2) (+ column 3)))
            ) ;
        (emit-string! writer (format-comment-content content))
        (positioned-env node
          column
          (vector (positioned-atom content-node content-indent left-line (writer-line writer))
          ) ;vector
          left-line
          (writer-line writer)
        ) ;positioned-env
      ) ;let*
    ) ;define

    ;; ; emit-inline! 输出单行节点，并返回写入了位置信息的新节点。
    ;; ; 原始 node 保持不变，formatter 的布局结果只存在于返回的新树中。
    (define (emit-inline! node writer column)
      (cond ((comment-node? node) (emit-comment! node writer column))
            ((atom? node)
             (let ((left-line (writer-line writer)))
               (emit-string! writer (format-inline-atom-or-quote (atom-value node)))
               (positioned-atom node column left-line (writer-line writer))
             ) ;let
            ) ;
            ((quote-env? node)
             (let ((left-line (writer-line writer)) (content (quote-env-content node)))
               (emit-string! writer "'")
               (emit-string! writer (format-inline-atom-or-quote-at content (+ column 1)))
               (positioned-env node column (vector) left-line (writer-line writer))
             ) ;let
            ) ;
            ((reader-prefix-env? node)
             (let ((left-line (writer-line writer)))
               (emit-string! writer (format-inline-atom-or-quote-at (env-value node) column))
               (positioned-env node column (vector) left-line (writer-line writer))
             ) ;let
            ) ;
            (else (let ((children (env-children node)))
                    (let ((left-line (writer-line writer)))
                      (emit-string! writer "(")
                      (emit-string! writer (env-tag-name node))
                      (let loop
                        ((i 0) (new-children '()))
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
                            (let ((new-child (emit-inline! (vector-ref children i) writer (writer-column writer)))
                                 ) ;
                              (loop (+ i 1) (cons new-child new-children))
                            ) ;let
                          ) ;begin
                        ) ;if
                      ) ;let
                    ) ;let
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; ; walk 是核心 DFS 入口：从当前行的 column 列输出 node，并返回带位置信息的新 node。
    (define (walk! node writer column)
      (cond ((comment-node? node) (emit-comment! node writer column))
            ((atom? node) (emit-inline! node writer column))
            ((can-inline? node) (emit-inline! node writer column))
            (else (walk-env! node writer column))
      ) ;cond
    ) ;define

    ;; ; walk-env! 处理跨行环境。它按 DFS 顺序写出文本，同时收集每个 child
    ;; ; 的新节点。进入时记录左括号位置，退出时将具名右侧标记输出回同一列。
    (define (walk-env! node writer column)
      (let ((env-indent column) (left-line (writer-line writer)))
        (cond ((newline-node? node)
               (let ((blank-lines (if (> (vector-length (env-children node)) 0)
                                    (let ((child (vector-ref (env-children node) 0)))
                                      (if (atom? child) (let ((val (atom-value child))) (if (number? val) val 1)) 1)
                                    ) ;let
                                    1
                                  ) ;if
                     ) ;blank-lines
                    ) ;
                 (emit-string! writer (make-newlines blank-lines))
                 (positioned-env node env-indent (vector) left-line (writer-line writer))
               ) ;let
              ) ;
              ((quote-env? node)
               (let ((content (quote-env-content node)))
                 (emit-string! writer "'")
                 (emit-string! writer (format-inline-atom-or-quote-at content (+ env-indent 1)))
                 (positioned-env node env-indent (vector) left-line (writer-line writer))
               ) ;let
              ) ;
              ((reader-prefix-env? node)
               (emit-string! writer
                 (format-inline-atom-or-quote-at (env-value node) env-indent)
               ) ;emit-string!
               (positioned-env node env-indent (vector) left-line (writer-line writer))
              ) ;
              (else (emit-string! writer "(")
                (emit-string! writer (env-tag-name node))
                (let* ((first-column (writer-column writer))
                       (selected (select-first-line-children node first-column))
                       (rest-start (selected-count selected))
                       (children (env-children node))
                      ) ;
                  (let ((new-children (let loop-selected
                                        ((items selected) (index 0) (result '()))
                                        (if (null? items)
                                          result
                                          (begin
                                            (emit-string! writer (child-separator node index))
                                            (let ((new-child (walk! (selected-child (car items)) writer (selected-column (car items)))
                                                  ) ;new-child
                                                 ) ;
                                              (loop-selected (cdr items) (+ index 1) (cons new-child result))
                                            ) ;let
                                          ) ;begin
                                        ) ;if
                                      ) ;let
                        ) ;new-children
                       ) ;
                    (let ((new-children (if (< rest-start (vector-length children))
                                          (let ((child-indent (next-line-child-indent node env-indent selected rest-start)))
                                            (let loop-rest
                                              ((i rest-start) (result new-children))
                                              (if (>= i (vector-length children))
                                                result
                                                (let ((child (vector-ref children i)))
                                                  (if (newline-node? child)
                                                    (begin
                                                      (emit-newline! writer)
                                                      (loop-rest (+ i 1) result)
                                                    ) ;begin
                                                    (begin
                                                      (emit-newline! writer)
                                                      (emit-spaces! writer child-indent)
                                                      (let ((new-child (walk! child writer child-indent)))
                                                        (loop-rest (+ i 1) (cons new-child result))
                                                      ) ;let
                                                    ) ;begin
                                                  ) ;if
                                                ) ;let
                                              ) ;if
                                            ) ;let
                                          ) ;let
                                          new-children
                                        ) ;if
                          ) ;new-children
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
              ) ;else
        ) ;cond
      ) ;let
    ) ;define
  ) ;begin

  ;; ; format-node 返回两个值：
  ;; ; 1. 格式化后的文本
  ;; ; 2. 带 indent/left-line/right-line 信息的新 node
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
    (call-with-values (lambda () (format-datum+node datum))
      (lambda (text new-node) text)
    ) ;call-with-values
  ) ;define

  (define (read-all port)
    (let loop
      ((result '()))
      (let ((datum (read port)))
        (if (eof-object? datum) (reverse result) (loop (cons datum result)))
      ) ;let
    ) ;let
  ) ;define

  (define (join-top-level pieces)
    (let ((out (open-output-string)))
      (let loop
        ((rest pieces) (first #t))
        (cond ((null? rest)
               (let ((result (get-output-string out)))
                 (if (and (not (string=? result "")) (not (string-suffix? "\n" result)))
                   (string-append result "\n")
                   result
                 ) ;if
               ) ;let
              ) ;
              (first
                (display (car rest) out)
                (loop (cdr rest) #f)
              ) ;
              (else
                (display "\n" out)
                (display (car rest) out)
                (loop (cdr rest) #f)
              ) ;
        ) ;cond
      ) ;let
    ) ;let
  ) ;define

  ;; ; 检查表达式是否是 define 形式
  (define (define-form? datum)
    (and (pair? datum) (not (null? datum)) (eq? (car datum) 'define))
  ) ;define

  (define (define-node? node)
    (and (env? node) (string=? (env-tag-name node) "define"))
  ) ;define

  ;; ; 检查表达式是否是 newline 形式：(*newline* n)
  (define (newline-form? datum)
    (and (pair? datum)
      (not (null? datum))
      (eq? (car datum) '*newline*)
      (not (null? (cdr datum)))
      (number? (cadr datum))
    ) ;and
  ) ;define

  ;; ; 获取 newline 形式的空行数量
  (define (newline-form-count datum)
    (cadr datum)
  ) ;define

  ;; ; 判断是否需要空行分隔
  ;; ; 如果当前是 define 且不是文件的第一个表达式，则返回 #t
  (define (needs-blank-line? datum is-first-expr)
    (and (define-form? datum) (not is-first-expr))
  ) ;define

  ;; ; 格式化顶层表达式列表，在需要时插入空行
  ;; ; 处理 (*newline* n) 形式，将其转换为实际空行
  (define (format-top-level datums)
    (let loop
      ((rest datums) (is-first #t) (result '()))
      (if (null? rest)
        (reverse result)
        (let ((datum (car rest)))
          (cond ((newline-form? datum)
                 (if is-first
                   (loop (cdr rest) #t result)
                   (let ((blank-lines (newline-form-count datum)))
                     (loop (cdr rest) #f (cons (make-newlines blank-lines) result))
                   ) ;let
                 ) ;if
                ) ;
                (else (let* ((needs-blank (needs-blank-line? datum is-first))
                             (formatted (format-datum datum))
                             (new-result (if needs-blank (cons formatted (cons "" result)) (cons formatted result))
                             ) ;new-result
                            ) ;
                        (loop (cdr rest) #f new-result)
                      ) ;let*
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;let
  ) ;define

  (define (format-top-level-nodes nodes)
    (let loop
      ((i 0) (is-first #t) (result '()))
      (if (>= i (vector-length nodes))
        (reverse result)
        (let ((node (vector-ref nodes i)))
          (if (newline-node? node)
            (loop (+ i 1) #f (cons (make-newlines (newline-count node)) result))
            (call-with-values (lambda () (format-node node 0))
              (lambda (formatted positioned-node)
                (let* ((prev-node (if (> i 0) (vector-ref nodes (- i 1)) #f))
                       (needs-blank (and (define-node? node)
                                      (not is-first)
                                      (not (and prev-node (newline-node? prev-node)))
                                    ) ;and
                       ) ;needs-blank
                       (next-result (if needs-blank (cons formatted (cons "" result)) (cons formatted result))
                       ) ;next-result
                      ) ;
                  (loop (+ i 1) #f next-result)
                ) ;let*
              ) ;lambda
            ) ;call-with-values
          ) ;if
        ) ;let
      ) ;if
    ) ;let
  ) ;define

  (define (format-string source)
    (let* ((nodes (scan-string source)) (pieces (format-top-level-nodes nodes)))
      (join-top-level pieces)
    ) ;let*
  ) ;define

  (define (format-nodes nodes)
    (join-top-level (format-top-level-nodes nodes))
  ) ;define
) ;define-library

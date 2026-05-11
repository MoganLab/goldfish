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

(define-library (liii goldfmt-scan)
  (export scan scan-string scan-file)
  (import (liii base)
    (liii path)
    (liii raw-string)
    (liii string)
    (liii unicode)
    (liii list)
    (liii goldfmt-record)
    (scheme char)
    (rename (liii goldfmt-tokenize)
      (tokenize source-tokenize)
      (tokens->string source-tokens->string)
    ) ;rename
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

    ;; ; 高效拼接字符串列表（O(n)，避免 string-join 的 O(n^2) 问题）
    (define (fast-string-join strings)
      (let ((out (open-output-string)))
        (let loop
          ((rest strings))
          (if (null? rest)
            (get-output-string out)
            (begin
              (display (car rest) out)
              (loop (cdr rest))
            ) ;begin
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (raw-string-form? datum)
      (and (pair? datum)
        (eq? (car datum) '*raw-string*)
        (pair? (cdr datum))
        (string? (cadr datum))
        (pair? (cddr datum))
        (string? (caddr datum))
        (null? (cdddr datum))
      ) ;and
    ) ;define

    (define (char-literal-form? datum)
      (and (pair? datum)
        (eq? (car datum) '*char-literal*)
        (pair? (cdr datum))
        (string? (cadr datum))
        (pair? (cddr datum))
        (char? (caddr datum))
        (null? (cdddr datum))
      ) ;and
    ) ;define

    ;; ; 辅助函数：安全地移除字符串尾部空格
    ;; ; 使用 utf8-string-trim-right 正确处理 Unicode 字符
    ;; ; 如果内容只有空格，则保留原样
    (define (trim-right-spaces str)
      (if (string-every (lambda (c) (or (char=? c #\space) (char=? c #\tab))) str)
        str
        (utf8-string-trim-right str)
      ) ;if
    ) ;define

    ;; ; atom? 辅助函数：判断是否为 Scheme 原子类型
    ;; ; 原子类型包括：symbol, number, string, boolean, char, 空列表, vector, byte-vector, eof-object,
    ;; ; 以及其他不可解析的对象，还包括 S7 reader syntax 对象（如 #_quote）
    ;; ; 和内部 procedure 对象（如 #_list-values）
    (define (atom? x)
      (or (symbol? x)
        (number? x)
        (string? x)
        (raw-string-literal? x)
        (char-literal? x)
        (boolean? x)
        (char? x)
        (null? x)
        (vector? x)
        (byte-vector? x)
        (eof-object? x)
        (syntax? x)
        (eq? 'undefined? (type-of x))
        (unspecified? x)
        (procedure? x)
      ) ;or
    ) ;define

    ;; ; scan 函数：将 s-exp 扫描成 env 或 atom
    ;; ; 输入 datum: s-exp（可以是 atom 或 list）
    ;; ; 返回: env 记录或 atom 记录
    (define (scan datum)
      (scan-datum (normalize-datum datum) 0)
    ) ;define

    ;; ; 辅助函数：扫描 datum，带深度参数
    ;; ; depth: 当前深度（根节点为 0）
    (define (scan-datum datum depth)
      (cond ((raw-string-form? datum)
             (make-atom :depth
               depth
               :value
               (make-raw-string-literal :source (cadr datum) :value (caddr datum))
             ) ;make-atom
            ) ;
            ((char-literal-form? datum)
             (make-atom :depth
               depth
               :value
               (make-char-literal :source (cadr datum) :value (caddr datum))
             ) ;make-atom
            ) ;
            ((atom? datum)
             ;; ; 如果是 atom，创建 atom 记录
             ;; left-line 和 right-line 使用默认值 0
             (make-atom :depth depth :value datum)
            ) ;
            (else
              ;; ; 如果是 list，需要遍历创建 env
              (scan-list datum depth)
            ) ;else
      ) ;cond
    ) ;define

    ;; ; 辅助函数：判断是否为有效的 tag-name
    ;; ; symbol 或 syntax（如 #_quote, #_quasiquote 等）都可以作为 tag
    (define (tag? x)
      (or (symbol? x) (and (syntax? x) #t))
    ) ;define

    ;; ; 将 tag 转换为字符串
    (define (tag->string x)
      (if (symbol? x) (symbol->string x) (object->string x #f))
    ) ;define

    (define (procedure-name value)
      (and (procedure? value) (object->string value #f))
    ) ;define

    (define (procedure-name=? value name)
      (let ((proc-name (procedure-name value)))
        (and proc-name (string=? proc-name name))
      ) ;let
    ) ;define

    ;; ; 辅助函数：判断是否为 quote 形式：(quote x) 或 (#_quote x)
    (define (quote-form? lst)
      (and (pair? lst)
        (not (null? lst))
        (or (eq? (car lst) 'quote)
          (and (syntax? (car lst)) (string=? (object->string (car lst) #f) "#_quote"))
        ) ;or
        (not (null? (cdr lst)))
        (null? (cddr lst))
      ) ;and
    ) ;define

    (define (internal-list-values-form? value)
      (and (pair? value) (procedure-name=? (car value) "#_list-values"))
    ) ;define

    (define (internal-apply-values-form? value)
      (and (pair? value) (procedure-name=? (car value) "#_apply-values"))
    ) ;define

    (define (internal-list-star-form? value)
      (and (pair? value)
        (let ((name (procedure-name (car value))))
          (and name
            (or (string=? name "<list*>") (string=? name "#_list") (string=? name "list*"))
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (quote-syntax? value)
      (or (eq? value 'quote)
        (and (syntax? value) (string=? (object->string value #f) "#_quote"))
      ) ;or
    ) ;define

    (define (internal-quote-builder-form? value)
      (and (internal-list-values-form? value)
        (pair? (cdr value))
        (pair? (cddr value))
        (null? (cdddr value))
        (quote-syntax? (cadr value))
      ) ;and
    ) ;define

    (define (make-dotted-list head-items tail)
      (if (null? head-items)
        tail
        (cons (car head-items) (make-dotted-list (cdr head-items) tail))
      ) ;if
    ) ;define

    (define (normalize-quasiquote-item datum)
      (cond ((quote-form? datum) (cadr datum))
            ((internal-quote-builder-form? datum)
             (list 'quote (normalize-quasiquote-item (caddr datum)))
            ) ;
            ((internal-list-values-form? datum) (normalize-quasiquote-list datum))
            ((internal-list-star-form? datum) (normalize-quasiquote-dotted-list datum))
            ((internal-apply-values-form? datum)
             (list 'unquote-splicing (normalize-datum (cadr datum)))
            ) ;
            (else (list 'unquote (normalize-datum datum)))
      ) ;cond
    ) ;define

    (define (normalize-quasiquote-list datum)
      (map normalize-quasiquote-item (cdr datum))
    ) ;define

    (define (normalize-quasiquote-dotted-list datum)
      (let* ((args (cdr datum))
             (prefix-form (if (pair? args) (car args) '()))
             (tail-form (if (pair? (cdr args)) (cadr args) '()))
             (head-items (cond ((internal-list-values-form? prefix-form)
                                (map normalize-quasiquote-item (cdr prefix-form))
                               ) ;
                               ((null? prefix-form) '())
                               (else (list (normalize-quasiquote-item prefix-form)))
                         ) ;cond
             ) ;head-items
             (tail (normalize-quasiquote-item tail-form))
            ) ;
        (make-dotted-list head-items tail)
      ) ;let*
    ) ;define

    (define (normalize-datum datum)
      (cond ((raw-string-form? datum)
             (make-raw-string-literal :source (cadr datum) :value (caddr datum))
            ) ;
            ((char-literal-form? datum)
             (make-char-literal :source (cadr datum) :value (caddr datum))
            ) ;
            ((quote-form? datum) (list (car datum) (normalize-datum (cadr datum))))
            ((internal-list-values-form? datum)
             (list 'quasiquote (normalize-quasiquote-list datum))
            ) ;
            ((internal-list-star-form? datum)
             (list 'quasiquote (normalize-quasiquote-dotted-list datum))
            ) ;
            ((pair? datum)
             (cons (normalize-datum (car datum)) (normalize-datum (cdr datum)))
            ) ;
            ((byte-vector? datum)
             (let ((result (make-byte-vector (vector-length datum) 0)))
               (let loop
                 ((i 0))
                 (if (>= i (vector-length datum))
                   result
                   (begin
                     (byte-vector-set! result i (normalize-datum (vector-ref datum i)))
                     (loop (+ i 1))
                   ) ;begin
                 ) ;if
               ) ;let
             ) ;let
            ) ;
            ((vector? datum) (list->vector (map normalize-datum (vector->list datum))))
            (else datum)
      ) ;cond
    ) ;define

    ;; ; 辅助函数：判断是否为 S7 内部形式（如 #_list-values）
    ;; ; 这些形式是 S7 内部表示，不应该被进一步扫描
    (define (s7-internal-form? lst)
      (and (pair? lst)
        (not (null? lst))
        (syntax? (car lst))
        (let ((name (object->string (car lst) #f)))
          (or (string=? name "#_list-values") (string=? name "#_list"))
        ) ;let
      ) ;and
    ) ;define

    ;; ; 辅助函数：将点对列表转换为普通列表，保留 . 符号
    ;; ; 例如：(x y . rest) -> (x y . rest)
    ;; ; 返回一个可以被 map 和 list->vector 正确处理的列表
    (define (dotted-list-elements lst)
      (let loop
        ((current lst) (result '()))
        (cond ((pair? current) (loop (cdr current) (cons (car current) result)))
              ((null? current) (reverse result))
              (else (reverse (cons current (cons (string->symbol ".") result))))
        ) ;cond
      ) ;let
    ) ;define

    ;; ; 辅助函数：扫描列表或点对，创建 env 结构
    ;; ; depth: 当前深度（根节点为 0）
    ;; ; 注意：空列表 '() 是 atom，不会进入此函数
    ;; ; 支持点对 (a . b) 的处理
    (define (scan-list lst depth)
      (let* ((first (car lst)) (rest (cdr lst)))
        ;; ; 处理 quote 形式：整个作为一个 env，没有 children
        (cond ((quote-form? lst)
               (make-env :tag-name
                 (if (syntax? first) "#_quote" (symbol->string first))
                 :depth
                 depth
                 :children
                 (vector)
                 :value
                 lst
               ) ;make-env
              ) ;
              ;; ; 处理 S7 内部形式：整个作为一个 env，没有 children
              ((s7-internal-form? lst)
               (make-env :tag-name
                 (object->string first #f)
                 :depth
                 depth
                 :children
                 (vector)
                 :value
                 lst
               ) ;make-env
              ) ;
              (else
                ;; ; 处理点对：使用 dotted-list? 检测点对
                ;; ; 点对的第一元素不作为 tag，整个列表作为无 tag env
                (let ((children-list (if (dotted-list? lst) (dotted-list-elements lst) (if (tag? first) rest lst))
                      ) ;children-list
                      (has-tag? (and (not (dotted-list? lst)) (tag? first)))
                     ) ;
                  (let* ((scanned-children (map (lambda (child) (scan-datum child (+ depth 1))) children-list)
                         ) ;scanned-children
                         (children-vec (list->vector scanned-children))
                        ) ;
                    (if has-tag?
                      (make-env :tag-name
                        (tag->string first)
                        :depth
                        depth
                        :children
                        children-vec
                        :value
                        lst
                      ) ;make-env
                      (make-env :tag-name "" :depth depth :children children-vec :value lst)
                    ) ;if
                  ) ;let*
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; ; scan-string 函数：从字符串扫描所有顶层表达式
    ;; ; 输入 str: Scheme 代码字符串（例如："(+ 1 2) (+ 3 4)"）
    ;; ; 返回: 扫描后的 env/atom 记录列表（vector）
    (define (string-prefix-at? str start prefix)
      (let ((prefix-len (string-length prefix)) (str-len (string-length str)))
        (and (<= (+ start prefix-len) str-len)
          (let loop
            ((i 0))
            (or (>= i prefix-len)
              (and (char=? (string-ref str (+ start i)) (string-ref prefix i)) (loop (+ i 1)))
            ) ;or
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (find-raw-string-literal-end str body-start delimiter)
      (let ((needle (string-append "\"" delimiter "\"")) (str-len (string-length str)))
        (let loop
          ((i body-start))
          (cond ((> (+ i (string-length needle)) str-len) #f)
                ((string-prefix-at? str i needle) (+ i (string-length needle)))
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (read-raw-string-literal-value literal)
      (let ((value (read (open-input-string literal))))
        (if (string? value)
          value
          (error 'value-error "rewrite-raw-string-literals: expected raw string literal")
        ) ;if
      ) ;let
    ) ;define

    (define (reader-delimiter? c)
      (or (char=? c #\space)
        (char=? c #\tab)
        (char=? c #\newline)
        (char=? c #\return)
        (char=? c #\()
        (char=? c #\))
        (char=? c #\[)
        (char=? c #\])
        (char=? c #\")
        (char=? c #\;)
        (char=? c #\')
        (char=? c #\`)
        (char=? c #\,)
      ) ;or
    ) ;define
    (define (hex-digit? c)
      (or (char-numeric? c)
        (and (char>=? c #\a) (char<=? c #\f))
        (and (char>=? c #\A) (char<=? c #\F))
      ) ;or
    ) ;define
    (define (find-char-literal-end source start)
      (let ((len (string-length source)))
        (if (>= start len)
          #f
          (let loop
            ((i (+ start 1)))
            (if (>= i len)
              len
              (let ((c (string-ref source i)))
                (if (reader-delimiter? c) i (loop (+ i 1)))
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (rewrite-reader-literals source)
      (let ((len (string-length source)))
        (let loop
          ((i 0)
           (in-string #f)
           (escaped #f)
           (in-line-comment #f)
           (in-block-comment #f)
           (result '())
          ) ;
          (if (>= i len)
            (fast-string-join (reverse! result))
            (let ((c (string-ref source i))
                  (next-c (if (< (+ i 1) len) (string-ref source (+ i 1)) #\nul))
                 ) ;
              (cond (in-line-comment (loop (+ i 1) #f #f (not (char=? c #\newline)) #f (cons (string c) result))
                    ) ;in-line-comment
                    (in-block-comment (if (and (char=? c #\|) (char=? next-c #\#))
                                        (loop (+ i 2) #f #f #f #f (cons "|#" (cons (string c) result)))
                                        (loop (+ i 1) #f #f #f #t (cons (string c) result))
                                      ) ;if
                    ) ;in-block-comment
                    (in-string (cond (escaped (loop (+ i 1) #t #f #f #f (cons (string c) result)))
                                     ((char=? c #\\) (loop (+ i 1) #t #t #f #f (cons (string c) result)))
                                     ((char=? c #\") (loop (+ i 1) #f #f #f #f (cons (string c) result)))
                                     (else (loop (+ i 1) #t #f #f #f (cons (string c) result)))
                               ) ;cond
                    ) ;in-string
                    ((and (char=? c #\;) (char=? next-c #\;))
                     (loop (+ i 2) #f #f #t #f (cons ";;" result))
                    ) ;
                    ((and (char=? c #\#) (char=? next-c #\|))
                     (loop (+ i 2) #f #f #f #t (cons "#|" result))
                    ) ;
                    ((char=? c #\") (loop (+ i 1) #t #f #f #f (cons (string c) result)))
                    ((and (char=? c #\#) (char=? next-c #\"))
                     (let ((delimiter-end (string-index source #\" (+ i 2))))
                       (if delimiter-end
                         (let* ((delimiter (substring source (+ i 2) delimiter-end))
                                (literal-end (find-raw-string-literal-end source (+ delimiter-end 1) delimiter))
                               ) ;
                           (if literal-end
                             (let* ((literal (substring source i literal-end))
                                    (value (read-raw-string-literal-value literal))
                                    (rewritten (string-append "(*raw-string* "
                                                 (write-to-string literal)
                                                 " "
                                                 (write-to-string value)
                                                 ")"
                                               ) ;string-append
                                    ) ;rewritten
                                   ) ;
                               (loop literal-end #f #f #f #f (cons rewritten result))
                             ) ;let*
                             (loop (+ i 1) #f #f #f #f (cons (string c) result))
                           ) ;if
                         ) ;let*
                         (loop (+ i 1) #f #f #f #f (cons (string c) result))
                       ) ;if
                     ) ;let
                    ) ;
                    ((and (char=? c #\#) (char=? next-c #\\))
                     (let ((literal-end (find-char-literal-end source (+ i 2))))
                       (if literal-end
                         (let* ((literal (substring source i literal-end))
                                (value (read (open-input-string literal)))
                                (rewritten (string-append "(*char-literal* " (write-to-string literal) " " literal ")")
                                ) ;rewritten
                               ) ;
                           (if (char? value)
                             (loop literal-end #f #f #f #f (cons rewritten result))
                             (loop (+ i 1) #f #f #f #f (cons (string c) result))
                           ) ;if
                         ) ;let*
                         (loop (+ i 1) #f #f #f #f (cons (string c) result))
                       ) ;if
                     ) ;let
                    ) ;
                    (else (loop (+ i 1) #f #f #f #f (cons (string c) result)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (scan-string str)
      (let ((port (open-input-string (rewrite-reader-literals str))))
        (let loop
          ((results '()))
          (let ((datum (read port)))
            (if (eof-object? datum)
              (list->vector (reverse results))
              (loop (cons (scan-datum (normalize-datum datum) 0) results))
            ) ;if
          ) ;let
        ) ;let
      ) ;let
    ) ;define
    (define (whitespace-char? c)
      (or (char=? c #\space)
        (char=? c #\tab)
        (char=? c #\newline)
        (char=? c #\return)
      ) ;or
    ) ;define
    (define (inside-string? str pos)
      (let loop
        ((i 0) (in-string #f) (escaped #f))
        (cond ((>= i pos) in-string)
              ((>= i (string-length str)) in-string)
              (else (let ((c (string-ref str i)))
                      (cond (escaped (loop (+ i 1) in-string #f))
                            ((char=? c #\\) (loop (+ i 1) in-string #t))
                            ((char=? c #\") (loop (+ i 1) (not in-string) #f))
                            (else (loop (+ i 1) in-string #f))
                      ) ;cond
                    ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define
    (define (inside-raw-string? str pos)
      (let loop
        ((i 0) (in-raw-string #f) (raw-delimiter ""))
        (cond ((>= i pos) in-raw-string)
              ((>= i (string-length str)) in-raw-string)
              ((and (< (+ i 1) (string-length str))
                 (char=? (string-ref str i) #\#)
                 (char=? (string-ref str (+ i 1)) #\")
               ) ;and
               (let ((end-pos (string-index str #\" (+ i 2))))
                 (if end-pos
                   (let ((delimiter (substring str (+ i 2) end-pos)))
                     (loop (+ end-pos 1) #t delimiter)
                   ) ;let
                   (loop (+ i 2) #t "")
                 ) ;if
               ) ;let
              ) ;
              (in-raw-string (let ((del-len (string-length raw-delimiter)))
                               (if (and (<= (+ i del-len) (string-length str))
                                     (string=? (substring str i (+ i del-len)) raw-delimiter)
                                     (< (+ i del-len) (string-length str))
                                     (char=? (string-ref str (+ i del-len)) #\")
                                   ) ;and
                                 (loop (+ i del-len 1) #f "")
                                 (loop (+ i 1) #t raw-delimiter)
                               ) ;if
                             ) ;let
              ) ;in-raw-string
              (else (loop (+ i 1) #f ""))
        ) ;cond
      ) ;let
    ) ;define
    (define (find-real-comment-start line)
      (let loop
        ((pos 0))
        (cond ((>= pos (- (string-length line) 1)) #f)
              ((and (char=? (string-ref line pos) #\;)
                 (char=? (string-ref line (+ pos 1)) #\;)
                 (not (inside-string? line pos))
                 (not (inside-raw-string? line pos))
               ) ;and
               pos
              ) ;
              (else (loop (+ pos 1)))
        ) ;cond
      ) ;let
    ) ;define
    (define (comment-line? line)
      (let ((comment-pos (find-real-comment-start line)))
        (if comment-pos
          (let ((prefix (substring line 0 comment-pos)))
            (if (or (string-null? prefix) (string-every whitespace-char? prefix))
              comment-pos
              #f
            ) ;if
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;define
    (define (extract-comment-content line comment-pos)
      (trim-right-spaces (substring line (+ comment-pos 2)))
    ) ;define
    (define (escape-comment-content content)
      (let loop
        ((chars (string->list content)) (result '()))
        (if (null? chars)
          (list->string (reverse result))
          (let ((c (car chars)))
            (cond ((char=? c #\\) (loop (cdr chars) (cons #\\ (cons #\\ result))))
                  ((char=? c #\") (loop (cdr chars) (cons #\" (cons #\\ result))))
                  (else (loop (cdr chars) (cons c result)))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (tokenize content)
      (let* ((lines (string-split content #\newline)) (tokens '()) (blank-line-count 0))
        (for-each (lambda (line)
                    (let ((comment-pos (comment-line? line)))
                      (cond ((or (string-null? line) (string-every whitespace-char? line))
                             (set! blank-line-count (+ blank-line-count 1))
                            ) ;
                            (comment-pos (when (> blank-line-count 0)
                                           (set! tokens (cons (cons 'newline blank-line-count) tokens))
                                           (set! blank-line-count 0)
                                         ) ;when
                              (let ((content (extract-comment-content line comment-pos)))
                                (set! tokens (cons (cons 'comment content) tokens))
                              ) ;let
                            ) ;comment-pos
                            (else (when (> blank-line-count 0)
                                    (set! tokens (cons (cons 'newline blank-line-count) tokens))
                                    (set! blank-line-count 0)
                                  ) ;when
                              (set! tokens (cons (cons 'code line) tokens))
                            ) ;else
                      ) ;cond
                    ) ;let
                  ) ;lambda
          lines
        ) ;for-each
        (reverse tokens)
      ) ;let*
    ) ;define
    (define (tokens->string tokens)
      (string-join (map (lambda (token)
                          (let ((type (car token)) (content (cdr token)))
                            (cond ((eq? type 'comment)
                                   (string-append "(*comment* \"" (escape-comment-content content) "\")")
                                  ) ;
                                  ((eq? type 'newline) (string-append "(*newline* " (number->string content) ")"))
                                  (else content)
                            ) ;cond
                          ) ;let
                        ) ;lambda
                     tokens
                   ) ;map
        "\n"
      ) ;string-join
    ) ;define
    (define (scan-file path)
      (let* ((raw-content (path-read-text path))
             (scanned (source-tokenize raw-content))
             ;; 处理文件开头空行
             (leading-blanks (let loop ((i 0) (count 0))
                               (if (>= i (string-length raw-content))
                                 count
                                 (let ((c (string-ref raw-content i)))
                                   (cond ((char=? c #\newline) (loop (+ i 1) (+ count 1)))
                                         ((char=? c #\return) (loop (+ i 1) count))
                                         ((or (char=? c #\space) (char=? c #\tab)) (loop (+ i 1) count))
                                         (else count)
                                   ) ;cond
                                 ) ;let
                               ) ;if
                             ) ;let
                           )
             (tokens-with-leading (if (> leading-blanks 0)
                                    (cons (cons 'newline leading-blanks) scanned)
                                    scanned))
             ;; 处理文件末尾换行符
             (tokens (if (and (not (null? tokens-with-leading))
                             (> (string-length raw-content) 0)
                             (char=? (string-ref raw-content (- (string-length raw-content) 1)) #\newline))
                            ;and
                         (append tokens-with-leading (list (cons 'newline 1)))
                         tokens-with-leading)
                        ;if
             ) ;tokens
             (processed-content (source-tokens->string tokens))
            ) ;
        (scan-string processed-content)
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library

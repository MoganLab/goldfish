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

(define-library (liii lang)
  (import (scheme base)
    (scheme char)
    (liii list)
    (liii vector)
    (liii string-cursor)
    (liii unicode)
    (liii sort)
    (liii option)
    (liii par)
    (liii error)
  ) ;import
  (export $)
  (begin

    (define (%$arity-error op usage-str)
      (value-error (string-append "\n======================================================================\n"
                     "[liii lang Arity Error] Operator :"
                     (symbol->string op)
                     " argument mismatch\n"
                     "\n  👉 Usage: "
                     usage-str
                     "\n"
                     "======================================================================"
                   ) ;string-append
      ) ;value-error
    ) ;define

    (define (%$dispatch-error op x)
      (let ((reason
              (cond ((number? x)
                     (string-append "Cannot chain collection operator ':"
                       (symbol->string op)
                       "' on a number ("
                       (number->string x)
                       ").\n"
                       "  💡 Tip: This number likely came from an earlier terminal operator like :sum, :count, :length or :index-of.\n"
                       "     Terminal operations return scalar numbers and cannot be chained further with collection operators."
                     ) ;string-append
                    ) ;
                    ((boolean? x)
                     (string-append "Cannot chain collection operator ':"
                       (symbol->string op)
                       "' on a boolean ("
                       (if x "#t" "#f")
                       ").\n"
                       "  💡 Tip: This boolean likely came from an earlier predicate operator like :empty?, :contains, :exists, :forall, :starts-with? or :ends-with?.\n"
                       "     Predicate operations return #t/#f and cannot be chained further with collection operators."
                     ) ;string-append
                    ) ;
                    ((option? x)
                     (string-append "Operator ':"
                       (symbol->string op)
                       "' is not supported on an option ("
                       (object->string x)
                       ").\n"
                       "  💡 Tip: Option does not support collection slicing, sorting, indexing, or summing (option has no :"
                       (symbol->string op)
                       ").\n"
                       "     To use collection operations, convert it first: ($ opt :to-list :"
                       (symbol->string op)
                       " ...)"
                     ) ;string-append
                    ) ;
                    (else (string-append "$: :"
                            (symbol->string op)
                            " expects a list, vector, string or option, got: "
                            (object->string x)
                          ) ;string-append
                    ) ;else
              ) ;cond
            ) ;reason
           ) ;
        (type-error (string-append "\n======================================================================\n"
                      "[liii lang Pipeline Type Error]\n" "  " reason "\n"
                      "======================================================================"
                    ) ;string-append
        ) ;type-error
      ) ;let
    ) ;define

    ;; ---- string 路径（UTF-8，基于 (liii string-cursor)） ----

    (define (%$strlen s)
      (string-cursor->index s (string-cursor-end s))
    ) ;define

    (define (%$chars->string chars)
      (utf8->string
        (apply bytevector-append
          (map (lambda (ch) (codepoint->utf8 (char->integer ch))) chars)
        ) ;apply
      ) ;utf8->string
    ) ;define

    ;; Scala 宽容边界：n 越界/为负时 clamp 到 [0, 字符数]
    (define (%$clamp n s)
      (max 0 (min n (%$strlen s)))
    ) ;define

    (define (%$stake n s)
      (string-take s (%$clamp n s))
    ) ;define

    (define (%$sdrop n s)
      (string-drop s (%$clamp n s))
    ) ;define

    (define (%$sreverse s)
      (%$chars->string (reverse (string->list/cursors s)))
    ) ;define

    (define (%$smap f s)
      (%$chars->string (map f (string->list/cursors s)))
    ) ;define

    (define (%$sum x)
      (cond ((list? x) (if (null? x) 0 (apply + x)))
            ((vector? x)
             (let loop
               ((i 0) (acc 0))
               (if (= i (vector-length x)) acc (loop (+ i 1) (+ acc (vector-ref x i))))
             ) ;let
            ) ;
            ((string? x)
             (type-error "$: :sum on a string is not supported (string has no :sum)")
            ) ;
            ((option? x)
             (type-error "$: :sum on an option is not supported (option has no :sum)")
            ) ;
            (else (%$dispatch-error 'sum x))
      ) ;cond
    ) ;define

    (define (%$map f x)
      (cond ((list? x) (map f x))
            ((vector? x) (vector-map f x))
            ((string? x) (%$smap f x))
            ((option? x) (%$omap f x))
            (else (%$dispatch-error 'map x))
      ) ;cond
    ) ;define

    (define (%$filter p x)
      (cond ((list? x) (filter p x))
            ((vector? x) (vector-filter p x))
            ((string? x) (string-filter p x))
            ((option? x) (%$ofilter p x))
            (else (%$dispatch-error 'filter x))
      ) ;cond
    ) ;define

    (define (%$count p x)
      (cond ((list? x) (count p x))
            ((vector? x) (vector-count p x))
            ((string? x) (string-count p x))
            (else (%$dispatch-error 'count x))
      ) ;cond
    ) ;define

    (define (%$length x)
      (cond ((list? x) (length x))
            ((vector? x) (vector-length x))
            ((string? x) (%$strlen x))
            (else (%$dispatch-error 'length x))
      ) ;cond
    ) ;define

    (define (%$reverse x)
      (cond ((list? x) (reverse x))
            ((vector? x) (vector-reverse-copy x))
            ((string? x) (%$sreverse x))
            (else (%$dispatch-error 'reverse x))
      ) ;cond
    ) ;define

    (define (%$take n x)
      (cond ((list? x) (list-take x n))
            ((vector? x) (vector-take x n))
            ((string? x) (%$stake n x))
            (else (%$dispatch-error 'take x))
      ) ;cond
    ) ;define

    (define (%$drop n x)
      (cond ((list? x) (list-drop x n))
            ((vector? x) (vector-drop x n))
            ((string? x) (%$sdrop n x))
            (else (%$dispatch-error 'drop x))
      ) ;cond
    ) ;define

    (define (%$stringify x)
      (cond ((string? x) x)
            ((char? x) (string x))
            ((number? x) (number->string x))
            ((symbol? x) (symbol->string x))
            ((boolean? x) (if x "true" "false"))
            (else (format #f "~a" x))
      ) ;cond
    ) ;define

    ;; ---- option 路径（(liii option)，对标 Scala Option 链式） ----
    ;; None 传播：map/filter/flat-map 遇空保持空（None 短路）

    (define (option-if v)
      (if v (option v) (none))
    ) ;define

    (define (%$empty-coll-error op)
      (value-error "\n======================================================================\n"
        "[liii lang] :" op " on an empty collection\n" "\n  💡 Tip:\n"
        "     Cannot compute min/max on an empty collection.\n"
        "     Use :min-option / :max-option instead for safe querying!\n"
        "======================================================================"
      ) ;value-error
    ) ;define

    ;; ---- 并行算子（(liii par) 多核 CSP，list/vector 专用） ----

    (define (%$par-map f x)
      (cond ((list? x) (par-map f x))
            ((vector? x) (vector-par-map f x))
            (else (%$dispatch-error 'par-map x))
      ) ;cond
    ) ;define

    (define (%$par-filter p x)
      (cond ((list? x) (par-filter p x))
            ((vector? x) (vector-par-filter p x))
            (else (%$dispatch-error 'par-filter x))
      ) ;cond
    ) ;define

    (define (%$par-for-each f x)
      (cond ((list? x) (par-for-each f x) x)
            ((vector? x) (vector-par-for-each f x) x)
            (else (%$dispatch-error 'par-for-each x))
      ) ;cond
    ) ;define

    (define (%$reduce f x)
      (define (err)
        (%$empty-coll-error 'reduce)
      ) ;define
      (cond
       ((list? x) (if (null? x) (err) (%$fold-left (car x) f (cdr x))))
       ((vector? x)
        (if (zero? (vector-length x))
          (err)
          (%$fold-left (vector-ref x 0) f (vector-drop x 1))
        ) ;if
       ) ;
       (else (%$dispatch-error 'reduce x))
      ) ;cond
    ) ;define

    (define (%$reduce-option f x)
      (define lst
        (cond ((list? x) x)
              ((vector? x) (vector->list x))
              (else (%$dispatch-error 'reduce-option x))
        ) ;cond
      ) ;define
      (if (null? lst) (none) (option (%$fold-left (car lst) f (cdr lst))))
    ) ;define

    (define (%$slice from until x)
      (define (clamp v lo hi)
        (max lo (min v hi))
      ) ;define
      (define (do-slice lo hi)
        (cond
         ((list? x) (if (>= lo hi) '() (list-drop (list-take x hi) lo)))
         ((vector? x) (if (>= lo hi) #() (vector-drop (vector-take x hi) lo)))
         ((string? x) (if (>= lo hi) "" (%$sdrop lo (%$stake hi x))))
         (else (%$dispatch-error 'slice x))
        ) ;cond
      ) ;define
      (let ((len (%$length x)))
        (do-slice (clamp from 0 len) (clamp until 0 len))
      ) ;let
    ) ;define

    (define (%$empty? x)
      (cond ((list? x) (null? x))
            ((vector? x) (zero? (vector-length x)))
            ((string? x) (zero? (%$strlen x)))
            ((option? x) (option-empty? x))
            (else (%$dispatch-error 'empty? x))
      ) ;cond
    ) ;define

    (define (%$non-empty x)
      (not (%$empty? x))
    ) ;define

    (define (%$head x)
      (define (err)
        (%$empty-coll-error 'head)
      ) ;define
      (cond ((list? x) (if (null? x) (err) (car x)))
            ((vector? x) (if (zero? (vector-length x)) (err) (vector-ref x 0)))
            ((string? x)
             (if (zero? (%$strlen x)) (err) (string-ref/cursor x (string-cursor-start x)))
            ) ;
            ((option? x) (if (option-empty? x) (err) (option-get x)))
            (else (%$dispatch-error 'head x))
      ) ;cond
    ) ;define

    (define (%$last x)
      (define (err)
        (%$empty-coll-error 'last)
      ) ;define
      (cond ((list? x) (if (null? x) (err) (%$last-list x)))
            ((vector? x)
             (let ((n (vector-length x)))
               (if (zero? n) (err) (vector-ref x (- n 1)))
             ) ;let
            ) ;
            ((string? x)
             (let ((n (%$strlen x)))
               (if (zero? n) (err) (string-ref/cursor x (string-index->cursor x (- n 1))))
             ) ;let
            ) ;
            (else (%$dispatch-error 'last x))
      ) ;cond
    ) ;define

    (define (%$last-list x)
      (if (null? (cdr x)) (car x) (%$last-list (cdr x)))
    ) ;define

    (define (%$head-option x)
      (option-if (%$safe-head x))
    ) ;define

    (define (%$safe-head x)
      (and (not (%$empty? x))
        (cond ((list? x) (car x))
              ((vector? x) (vector-ref x 0))
              ((string? x) (string-ref/cursor x (string-cursor-start x)))
              (else #f)
        ) ;cond
      ) ;and
    ) ;define

    (define (%$last-option x)
      (option-if (%$safe-last x))
    ) ;define

    (define (%$safe-last x)
      (cond
       ((and (list? x) (not (null? x))) (%$last-list x))
       ((and (vector? x) (positive? (vector-length x)))
        (vector-ref x (- (vector-length x) 1))
       ) ;
       ((and (string? x) (positive? (%$strlen x)))
        (string-ref/cursor x (string-index->cursor x (- (%$strlen x) 1)))
       ) ;
       (else #f)
      ) ;cond
    ) ;define

    (define (%$min x)
      (cond ((list? x) (if (null? x) (%$empty-coll-error 'min) (apply min x)))
            ((vector? x)
             (if (zero? (vector-length x))
               (%$empty-coll-error 'min)
               (%$min (vector->list x))
             ) ;if
            ) ;
            (else (%$dispatch-error 'min x))
      ) ;cond
    ) ;define

    (define (%$max x)
      (cond ((list? x) (if (null? x) (%$empty-coll-error 'max) (apply max x)))
            ((vector? x)
             (if (zero? (vector-length x))
               (%$empty-coll-error 'max)
               (%$max (vector->list x))
             ) ;if
            ) ;
            (else (%$dispatch-error 'max x))
      ) ;cond
    ) ;define

    (define (%$min-option x)
      (option-if (%$safe-minmax x min))
    ) ;define

    (define (%$max-option x)
      (option-if (%$safe-minmax x max))
    ) ;define

    (define (%$safe-minmax x cmp)
      (define lst
        (cond ((list? x) x)
              ((vector? x) (vector->list x))
              (else (%$dispatch-error 'min-option x))
        ) ;cond
      ) ;define
      (if (null? lst) #f (apply cmp lst))
    ) ;define

    (define (%$take-right n x)
      (cond ((list? x) (list-take-right x n))
            ((vector? x) (vector-take-right x n))
            ((string? x) (string-take-right x (%$clamp n x)))
            (else (%$dispatch-error 'take-right x))
      ) ;cond
    ) ;define

    (define (%$drop-right n x)
      (cond ((list? x) (list-drop-right x n))
            ((vector? x) (vector-drop-right x n))
            ((string? x) (string-drop-right x (%$clamp n x)))
            (else (%$dispatch-error 'drop-right x))
      ) ;cond
    ) ;define

    (define (%$tail x)
      (define (err)
        (value-error "\n======================================================================\n"
          "[liii lang] :tail on an empty collection\n" "\n  💡 Tip:\n"
          "     Cannot call :tail on an empty collection.\n"
          "     Use :drop 1 for lenient behavior, or check :empty? first.\n"
          "======================================================================"
        ) ;value-error
      ) ;define
      (cond ((list? x) (if (null? x) (err) (cdr x)))
            ((vector? x) (if (zero? (vector-length x)) (err) (vector-drop x 1)))
            ((string? x) (if (zero? (%$strlen x)) (err) (%$sdrop 1 x)))
            (else (%$dispatch-error 'tail x))
      ) ;cond
    ) ;define

    (define (%$omap f x)
      (if (option-empty? x) x (option (f (option-get x))))
    ) ;define

    (define (%$ofilter p x)
      (option-filter p x)
    ) ;define

    (define (%$oflat-map f x)
      (option-flat-map f x)
    ) ;define

    (define (%$oexists p x)
      (and (option-defined? x) (p (option-get x)))
    ) ;define

    (define (%$oforall p x)
      (or (option-empty? x) (p (option-get x)))
    ) ;define

    (define (%$ocontains e x)
      (and (option-defined? x) (equal? (option-get x) e))
    ) ;define

    (define (%$oget-or-else d x)
      (option-get-or-else d x)
    ) ;define

    (define (%$oor-else alt x)
      (if (option? x) (option-or-else alt x) (%$dispatch-error 'or-else x))
    ) ;define

    (define (%$sort-with cmp x)
      (cond ((list? x) (list-sort cmp x))
            ((vector? x) (vector-sort cmp x))
            ((string? x) (%$chars->string (list-sort cmp (string->list/cursors x))))
            (else (%$dispatch-error 'sort-with x))
      ) ;cond
    ) ;define

    (define (%$flat-map f x)
      (define (%$concat chunks out-kind)
        (cond ((eq? out-kind 'vector) (list->vector (apply append chunks)))
              ((eq? out-kind 'string) (%$chars->string (apply append chunks)))
              (else (apply append chunks))
        ) ;cond
      ) ;define
      (define (%$->list v)
        (cond ((list? v) v)
              ((vector? v) (vector->list v))
              ((string? v) (string->list/cursors v))
              (else (list v))
        ) ;cond
      ) ;define
      (cond
       ((list? x) (%$concat (map (lambda (e) (%$->list (f e))) x) 'list))
       ((vector? x)
        (%$concat
          (map (lambda (e) (%$->list (f e))) (vector->list x))
          'vector
        ) ;%$concat
       ) ;
       ((string? x)
        (%$concat
          (map (lambda (c) (%$->list (f c))) (string->list/cursors x))
          'string
        ) ;%$concat
       ) ;
       ((option? x) (%$oflat-map f x))
       (else (%$dispatch-error 'flat-map x))
      ) ;cond
    ) ;define

    (define (%$index-of elem x)
      (cond
       ((list? x) (or (list-index (lambda (e) (equal? e elem)) x) -1))
       ((vector? x) (or (vector-index (lambda (e) (equal? e elem)) x) -1))
       ((string? x)
        (if (char? elem)
          (let ((cur (string-index x (lambda (c) (char=? c elem)))))
            (if (eq? cur (string-cursor-end x)) -1 (string-cursor->index x cur))
          ) ;let
          -1
        ) ;if
       ) ;
       (else (%$dispatch-error 'index-of x))
      ) ;cond
    ) ;define

    (define (%$zip other x)
      (define (zip-lists a b)
        (if (or (null? a) (null? b))
          '()
          (cons (list (car a) (car b)) (zip-lists (cdr a) (cdr b)))
        ) ;if
      ) ;define
      (define (->list v)
        (cond ((list? v) v)
              ((vector? v) (vector->list v))
              ((string? v) (string->list/cursors v))
              (else (%$dispatch-error 'zip v))
        ) ;cond
      ) ;define
      (define (from-list lst out-kind)
        (cond
         ((eq? out-kind 'vector) (list->vector (map (lambda (p) (list->vector p)) lst)))
         ((eq? out-kind 'string)
          (%$stringify
            (map
              (lambda (p) (string-append (%$stringify (car p)) (%$stringify (cadr p))))
              lst
            ) ;map
          ) ;%$stringify
         ) ;
         (else lst)
        ) ;cond
      ) ;define
      (let ((a (->list x)) (b (->list other)))
        (let ((zipped (zip-lists a b)))
          (cond ((vector? x) (list->vector (map list->vector zipped)))
                ((string? x)
                 (%$chars->string
                   (apply append (map (lambda (p) (list (car p) (cadr p))) zipped))
                 ) ;%$chars->string
                ) ;
                (else zipped)
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%$to-list x)
      (cond ((list? x) x)
            ((vector? x) (vector->list x))
            ((string? x) (string->list/cursors x))
            ((option? x) (if (option-defined? x) (list (option-get x)) '()))
            (else (%$dispatch-error 'to-list x))
      ) ;cond
    ) ;define

    (define (%$to-vector x)
      (cond ((vector? x) x)
            ((list? x) (list->vector x))
            ((string? x) (list->vector (string->list/cursors x)))
            ((option? x) (if (option-defined? x) (vector (option-get x)) #()))
            (else (%$dispatch-error 'to-vector x))
      ) ;cond
    ) ;define

    (define (%$indices x)
      (cond ((list? x) (iota (length x)))
            ((vector? x) (list->vector (iota (vector-length x))))
            ((string? x) (iota (%$strlen x)))
            (else (%$dispatch-error 'indices x))
      ) ;cond
    ) ;define

    (define (%$starts-with prefix x)
      (cond ((string? x) (if (string? prefix) (string-prefix? prefix x) #f))
            ((list? x)
             (let ((plen (length prefix)) (xlen (length x)))
               (cond ((zero? plen) #t)
                     ((> plen xlen) #f)
                     (else (equal? (list-take x plen) prefix))
               ) ;cond
             ) ;let
            ) ;
            ((vector? x)
             (let ((plen (vector-length prefix)) (xlen (vector-length x)))
               (cond ((zero? plen) #t)
                     ((> plen xlen) #f)
                     (else (equal? (vector-take x plen) prefix))
               ) ;cond
             ) ;let
            ) ;
            (else (%$dispatch-error 'starts-with? x))
      ) ;cond
    ) ;define

    (define (%$ends-with suffix x)
      (cond ((string? x) (if (string? suffix) (string-suffix? suffix x) #f))
            ((list? x)
             (let ((slen (length suffix)) (xlen (length x)))
               (cond ((zero? slen) #t)
                     ((> slen xlen) #f)
                     (else (equal? (list-take-right x slen) suffix))
               ) ;cond
             ) ;let
            ) ;
            ((vector? x)
             (let ((slen (vector-length suffix)) (xlen (vector-length x)))
               (cond ((zero? slen) #t)
                     ((> slen xlen) #f)
                     (else (equal? (vector-take-right x slen) suffix))
               ) ;cond
             ) ;let
            ) ;
            (else (%$dispatch-error 'ends-with? x))
      ) ;cond
    ) ;define

    (define (%$split-at n x)
      (list (%$take n x) (%$drop n x))
    ) ;define

    (define (%$zip-with-index x)
      (define (zip-with-idx lst)
        (let loop
          ((rest lst) (i 0) (acc '()))
          (if (null? rest)
            (reverse acc)
            (loop (cdr rest) (+ i 1) (cons (list (car rest) i) acc))
          ) ;if
        ) ;let
      ) ;define
      (cond ((list? x) (zip-with-idx x))
            ((vector? x)
             (let* ((n (vector-length x)) (res (make-vector n)))
               (let loop
                 ((i 0))
                 (if (= i n)
                   res
                   (begin
                     (vector-set! res i (vector (vector-ref x i) i))
                     (loop (+ i 1))
                   ) ;begin
                 ) ;if
               ) ;let
             ) ;let*
            ) ;
            ((string? x) (zip-with-idx (string->list/cursors x)))
            (else (%$dispatch-error 'zip-with-index x))
      ) ;cond
    ) ;define

    (define (%$distinct x)
      (define (dedup lst)
        (let loop
          ((rest lst) (seen '()) (acc '()))
          (cond ((null? rest) (reverse acc))
                ((member (car rest) seen) (loop (cdr rest) seen acc))
                (else (loop (cdr rest) (cons (car rest) seen) (cons (car rest) acc)))
          ) ;cond
        ) ;let
      ) ;define
      (cond ((list? x) (dedup x))
            ((vector? x) (list->vector (dedup (vector->list x))))
            ((string? x) (%$chars->string (dedup (string->list/cursors x))))
            (else (%$dispatch-error 'distinct x))
      ) ;cond
    ) ;define

    (define (%$partition p x)
      (define (split-seq len ref make)
        (let loop
          ((i 0) (yes '()) (no '()))
          (if (= i len)
            (list (make (reverse yes)) (make (reverse no)))
            (if (p (ref x i))
              (loop (+ i 1) (cons (ref x i) yes) no)
              (loop (+ i 1) yes (cons (ref x i) no))
            ) ;if
          ) ;if
        ) ;let
      ) ;define
      (cond
       ((list? x) (split-seq (length x) list-ref (lambda (l) l)))
       ((vector? x) (split-seq (vector-length x) vector-ref list->vector))
       ((string? x)
        (let ((parts
                (split-seq (%$strlen x)
                  (lambda (s i) (string-ref/cursor s (string-index->cursor s i)))
                  (lambda (l) l)
                ) ;split-seq
              ) ;parts
             ) ;
          (list (%$chars->string (car parts)) (%$chars->string (cadr parts)))
        ) ;let
       ) ;
       (else (%$dispatch-error 'partition x))
      ) ;cond
    ) ;define

    (define (%$find p x)
      (cond ((list? x) (option-if (find p x)))
            ((vector? x)
             (let ((i (vector-index p x)))
               (if i (option (vector-ref x i)) (none))
             ) ;let
            ) ;
            ((string? x)
             (let ((cur (string-index x p)))
               (if (eq? cur (string-cursor-end x)) (none) (option (string-ref/cursor x cur)))
             ) ;let
            ) ;
            (else (%$dispatch-error 'find x))
      ) ;cond
    ) ;define

    (define (%$fold-left init f x)
      (cond
       ((list? x) (fold (lambda (e acc) (f acc e)) init x))
       ((vector? x) (vector-fold (lambda (e acc) (f acc e)) init x))
       ((string? x) (string-fold (lambda (c acc) (f acc c)) init x))
       (else (%$dispatch-error 'fold-left x))
      ) ;cond
    ) ;define

    (define (%$mk-string x . args)
      (define elems
        (cond ((list? x) (map %$stringify x))
              ((vector? x) (map %$stringify (vector->list x)))
              (else (%$dispatch-error 'mk-string x))
        ) ;cond
      ) ;define
      (cond ((null? args) (apply string-append elems))
            ((null? (cdr args))
             (let ((sep (car args)))
               (if (null? elems)
                 ""
                 (let loop
                   ((rest elems) (acc (car elems)))
                   (if (null? (cdr rest))
                     acc
                     (loop (cdr rest) (string-append acc sep (cadr rest)))
                   ) ;if
                 ) ;let
               ) ;if
             ) ;let
            ) ;
            ((and (= (length args) 3))
             (string-append (car args)
               (%$mk-string (cond ((list? x) x)
                                  ((vector? x) (vector->list x))
                                  (else x)
                            ) ;cond
                 (cadr args)
               ) ;%$mk-string
               (caddr args)
             ) ;string-append
            ) ;
            (else (value-error "$: :mk-string expects 0, 1 or 3 arguments"))
      ) ;cond
    ) ;define

    (define (%$take-while p x)
      (cond ((list? x) (take-while p x))
            ((vector? x)
             (let ((n
                     (let loop
                       ((i 0))
                       (if
                         (or (= i (vector-length x)) (not (p (vector-ref x i))))
                         i
                         (loop (+ i 1))
                       ) ;if
                     ) ;let
                   ) ;n
                  ) ;
               (vector-take x n)
             ) ;let
            ) ;
            ((string? x)
             (%$stake
               (string-cursor->index x (string-index x (lambda (c) (not (p c)))))
               x
             ) ;%$stake
            ) ;
            (else (%$dispatch-error 'take-while x))
      ) ;cond
    ) ;define

    (define (%$drop-while p x)
      (cond ((list? x) (drop-while p x))
            ((vector? x)
             (let ((n
                     (let loop
                       ((i 0))
                       (if
                         (or (= i (vector-length x)) (not (p (vector-ref x i))))
                         i
                         (loop (+ i 1))
                       ) ;if
                     ) ;let
                   ) ;n
                  ) ;
               (vector-drop x n)
             ) ;let
            ) ;
            ((string? x)
             (%$sdrop
               (string-cursor->index x (string-index x (lambda (c) (not (p c)))))
               x
             ) ;%$sdrop
            ) ;
            (else (%$dispatch-error 'drop-while x))
      ) ;cond
    ) ;define

    (define (%$contains elem x)
      (cond ((list? x) (and (member elem x) #t))
            ((vector? x) (vector-contains? x elem))
            ((option? x) (%$ocontains elem x))
            ((string? x)
             (if (char? elem)
               (let ((cur (string-index x (lambda (c) (char=? c elem)))))
                 (and (not (eq? cur (string-cursor-end x))) #t)
               ) ;let
               #f
             ) ;if
            ) ;
            (else (%$dispatch-error 'contains x))
      ) ;cond
    ) ;define

    (define (%$exists p x)
      (cond ((list? x) (any p x))
            ((vector? x) (vector-any p x))
            ((string? x) (string-any p x))
            ((option? x) (%$oexists p x))
            (else (%$dispatch-error 'exists x))
      ) ;cond
    ) ;define

    (define (%$forall p x)
      (cond ((list? x) (every p x))
            ((vector? x) (vector-every p x))
            ((string? x) (string-every p x))
            ((option? x) (%$oforall p x))
            (else (%$dispatch-error 'forall x))
      ) ;cond
    ) ;define

    (define (%$for-each f x)
      (cond ((list? x) (for-each f x))
            ((vector? x) (vector-for-each f x))
            ((string? x) (string-for-each-cursor f x))
            ((option? x) (option-for-each f x))
            (else (%$dispatch-error 'for-each x))
      ) ;cond
      x
    ) ;define

    (define (%$unknown-op op)
      (let ((op-str (if (symbol? op) (symbol->string op) (object->string op))))
        (if (symbol? op)
          (value-error (string-append "\n======================================================================\n"
                         "[liii lang] Unknown operator: " op-str "\n"
                         "\n  💡 Supported operators:\n"
                         "    :map :filter :for-each :take :drop :sum :count :length :reverse\n"
                         "    :exists :forall :contains :take-while :drop-while :mk-string\n"
                         "    :fold-left :fold :find :partition :distinct :zip :zip-with-index\n"
                         "    :index-of :starts-with? :ends-with? :indices :to-list :to-vector\n"
                         "    :flat-map :sort-with :get-or-else :or-else :tail :take-right :drop-right\n"
                         "    :min :max :min-option :max-option :empty? :not-empty?\n"
                         "    :head :last :head-option :last-option :reduce :reduce-option :slice\n"
                         "    :par-map :par-filter :par-for-each\n"
                         "======================================================================"
                       ) ;string-append
          ) ;value-error
          (value-error (string-append "\n======================================================================\n"
                         "[liii lang Syntax Error] Expected an operator keyword starting with ':', but got: "
                         op-str "\n" "\n  💡 Common causes:\n"
                         "     1. Passed extra arguments to a zero-argument operator (e.g. :sum (+ _ 1)).\n"
                         "     2. Forgot the leading colon ':' on an operator name.\n"
                         "======================================================================"
                       ) ;string-append
          ) ;value-error
        ) ;if
      ) ;let
    ) ;define

    ;; 谓词位 `_` 自动提升：含 _ 则包 lambda，否则原样传递
    (define-syntax %$lift
      (syntax-rules (_)
        ;; 扁平：_ 在第 1~5 实参位
        ((_ (op _ . m)) (lambda (_) (op _ . m)))
        ((_ (op a _ . m)) (lambda (_) (op a _ . m)))
        ((_ (op a b _ . m)) (lambda (_) (op a b _ . m)))
        ((_ (op a b c _ . m)) (lambda (_) (op a b c _ . m)))
        ((_ (op a b c d _ . m)) (lambda (_) (op a b c d _ . m)))
        ;; 深层：含 _ 的子表达式在第 1~3 位（子表达式内 _ 也在第 1~3 位）
        ((_ (op (q _ . qm) . m)) (lambda (_) (op (q _ . qm) . m)))
        ((_ (op (q qa _ . qm) . m)) (lambda (_) (op (q qa _ . qm) . m)))
        ((_ (op (q qa qb _ . qm) . m)) (lambda (_) (op (q qa qb _ . qm) . m)))
        ((_ (op a (q _ . qm) . m)) (lambda (_) (op a (q _ . qm) . m)))
        ((_ (op a (q qa _ . qm) . m)) (lambda (_) (op a (q qa _ . qm) . m)))
        ((_ (op a (q qa qb _ . qm) . m)) (lambda (_) (op a (q qa qb _ . qm) . m)))
        ((_ (op a b (q _ . qm) . m)) (lambda (_) (op a b (q _ . qm) . m)))
        ((_ (op a b (q qa _ . qm) . m)) (lambda (_) (op a b (q qa _ . qm) . m)))
        ((_ (op a b (q qa qb _ . qm) . m)) (lambda (_) (op a b (q qa qb _ . qm) . m)))
        ;; 恒等 / 原样
        ((_ _) (lambda (_) _))
        ((_ f) f)
      ) ;syntax-rules
    ) ;define-syntax

    (define-syntax $
      (syntax-rules (:map
                      :filter         :for-each
                      :take           :drop
                      :sum            :count
                      :length         :reverse
                      :exists         :forall
                      :contains       :take-while
                      :drop-while     :mk-string
                      :fold-left      :fold
                      :find           :partition
                      :distinct       :zip
                      :zip-with-index :index-of
                      :flat-map       :sort-with
                      :get-or-else    :tail
                      :take-right     :drop-right
                      :min            :max
                      :min-option     :max-option
                      :empty?         :not-empty?
                      :head           :last
                      :head-option    :last-option
                      :reduce         :reduce-option
                      :slice          :par-map
                      :par-filter     :par-for-each
                      :split-at       :starts-with?
                      :ends-with?     :indices
                      :or-else        :to-list
                      :to-vector
                    ) ;:map
        ((_ x) x)
        ((_ x :map f rest ...) ($ (%$map (%$lift f) x) rest ...))
        ((_ x :map) (%$arity-error 'map "($ coll :map f)"))
        ((_ x :filter p rest ...) ($ (%$filter (%$lift p) x) rest ...))
        ((_ x :filter) (%$arity-error 'filter "($ coll :filter p)"))
        ((_ x :for-each f rest ...) ($ (%$for-each (%$lift f) x) rest ...))
        ((_ x :for-each) (%$arity-error 'for-each "($ coll :for-each f)"))
        ((_ x :take n rest ...) ($ (%$take n x) rest ...))
        ((_ x :take) (%$arity-error 'take "($ coll :take n)"))
        ((_ x :drop n rest ...) ($ (%$drop n x) rest ...))
        ((_ x :drop) (%$arity-error 'drop "($ coll :drop n)"))
        ((_ x :sum rest ...) ($ (%$sum x) rest ...))
        ((_ x :count p rest ...) ($ (%$count (%$lift p) x) rest ...))
        ((_ x :length rest ...) ($ (%$length x) rest ...))
        ((_ x :reverse rest ...) ($ (%$reverse x) rest ...))
        ((_ x :exists p rest ...) ($ (%$exists (%$lift p) x) rest ...))
        ((_ x :forall p rest ...) ($ (%$forall (%$lift p) x) rest ...))
        ((_ x :contains e rest ...) ($ (%$contains e x) rest ...))
        ((_ x :contains) (%$arity-error 'contains "($ coll :contains elem)"))
        ((_ x :take-while p rest ...) ($ (%$take-while (%$lift p) x) rest ...))
        ((_ x :take-while) (%$arity-error 'take-while "($ coll :take-while p)"))
        ((_ x :drop-while p rest ...) ($ (%$drop-while (%$lift p) x) rest ...))
        ((_ x :drop-while) (%$arity-error 'drop-while "($ coll :drop-while p)"))
        ((_ x :mk-string) (%$mk-string x))
        ((_ x :mk-string sep) (%$mk-string x sep))
        ((_ x :mk-string start sep end) (%$mk-string x start sep end))
        ((_ x :fold-left init f) (%$fold-left init f x))
        ((_ x :fold-left) (%$arity-error 'fold-left "($ coll :fold-left init f)"))
        ((_ x :fold-left init) (%$arity-error 'fold-left "($ coll :fold-left init f)"))
        ((_ x :fold init f) (%$fold-left init f x))
        ((_ x :fold) (%$arity-error 'fold "($ coll :fold init f)"))
        ((_ x :fold init) (%$arity-error 'fold "($ coll :fold init f)"))
        ((_ x :find p rest ...) ($ (%$find (%$lift p) x) rest ...))
        ((_ x :find) (%$arity-error 'find "($ coll :find p)"))
        ((_ x :partition p) (%$partition (%$lift p) x))
        ((_ x :partition) (%$arity-error 'partition "($ coll :partition p)"))
        ((_ x :distinct rest ...) ($ (%$distinct x) rest ...))
        ((_ x :zip other) (%$zip other x))
        ((_ x :zip-with-index rest ...) ($ (%$zip-with-index x) rest ...))
        ((_ x :split-at n) (%$split-at n x))
        ((_ x :split-at) (%$arity-error 'split-at "($ coll :split-at n)"))
        ((_ x :starts-with? prefix) (%$starts-with prefix x))
        ((_ x :starts-with?)
         (%$arity-error 'starts-with? "($ coll :starts-with? prefix)")
        ) ;
        ((_ x :ends-with? suffix) (%$ends-with suffix x))
        ((_ x :ends-with?) (%$arity-error 'ends-with? "($ coll :ends-with? suffix)"))
        ((_ x :indices rest ...) ($ (%$indices x) rest ...))
        ((_ x :to-list rest ...) ($ (%$to-list x) rest ...))
        ((_ x :to-vector rest ...) ($ (%$to-vector x) rest ...))
        ((_ x :index-of e) (%$index-of e x))
        ((_ x :index-of) (%$arity-error 'index-of "($ coll :index-of elem)"))
        ((_ x :flat-map f rest ...) ($ (%$flat-map (%$lift f) x) rest ...))
        ((_ x :flat-map) (%$arity-error 'flat-map "($ coll :flat-map f)"))
        ((_ x :sort-with cmp rest ...) ($ (%$sort-with cmp x) rest ...))
        ((_ x :sort-with) (%$arity-error 'sort-with "($ coll :sort-with cmp)"))
        ((_ x :get-or-else d) (%$oget-or-else d x))
        ((_ x :get-or-else) (%$arity-error 'get-or-else "($ opt :get-or-else default)"))
        ((_ x :or-else alt rest ...) ($ (%$oor-else alt x) rest ...))
        ((_ x :or-else) (%$arity-error 'or-else "($ opt :or-else alt-option)"))
        ((_ x :tail rest ...) ($ (%$tail x) rest ...))
        ((_ x :take-right n rest ...) ($ (%$take-right n x) rest ...))
        ((_ x :take-right) (%$arity-error 'take-right "($ coll :take-right n)"))
        ((_ x :drop-right n rest ...) ($ (%$drop-right n x) rest ...))
        ((_ x :drop-right) (%$arity-error 'drop-right "($ coll :drop-right n)"))
        ((_ x :min rest ...) ($ (%$min x) rest ...))
        ((_ x :max rest ...) ($ (%$max x) rest ...))
        ((_ x :min-option rest ...) ($ (%$min-option x) rest ...))
        ((_ x :max-option rest ...) ($ (%$max-option x) rest ...))
        ((_ x :empty? rest ...) ($ (%$empty? x) rest ...))
        ((_ x :not-empty? rest ...) ($ (%$non-empty x) rest ...))
        ((_ x :head rest ...) ($ (%$head x) rest ...))
        ((_ x :last rest ...) ($ (%$last x) rest ...))
        ((_ x :head-option rest ...) ($ (%$head-option x) rest ...))
        ((_ x :last-option rest ...) ($ (%$last-option x) rest ...))
        ((_ x :reduce f rest ...) ($ (%$reduce f x) rest ...))
        ((_ x :reduce) (%$arity-error 'reduce "($ coll :reduce f)"))
        ((_ x :reduce-option f rest ...) ($ (%$reduce-option f x) rest ...))
        ((_ x :reduce-option)
         (%$arity-error 'reduce-option "($ coll :reduce-option f)")
        ) ;
        ((_ x :slice from until rest ...) ($ (%$slice from until x) rest ...))
        ((_ x :slice) (%$arity-error 'slice "($ coll :slice from until)"))
        ((_ x :slice from) (%$arity-error 'slice "($ coll :slice from until)"))
        ((_ x :par-map f rest ...) ($ (%$par-map (%$lift f) x) rest ...))
        ((_ x :par-map) (%$arity-error 'par-map "($ coll :par-map f)"))
        ((_ x :par-filter p rest ...) ($ (%$par-filter (%$lift p) x) rest ...))
        ((_ x :par-filter) (%$arity-error 'par-filter "($ coll :par-filter p)"))
        ((_ x :par-for-each f rest ...) ($ (%$par-for-each (%$lift f) x) rest ...))
        ((_ x :par-for-each) (%$arity-error 'par-for-each "($ coll :par-for-each f)"))
        ((_ x op . rest) (%$unknown-op 'op))
      ) ;syntax-rules
    ) ;define-syntax

  ) ;begin
) ;define-library

(define-library (liii string-cursor)
  (export
    ;; Cursor operations
    string-cursor?
    string-cursor-start
    string-cursor-end
    string-cursor-next
    string-cursor-prev
    string-cursor-forward
    string-cursor-back
    string-cursor=?
    string-cursor<?
    string-cursor>?
    string-cursor<=?
    string-cursor>=?
    string-cursor-diff
    string-cursor->index
    string-index->cursor

    ;; Predicates
    string-null?
    string-every
    string-any

    ;; Constructors
    string-tabulate
    string-unfold
    string-unfold-right

    ;; Conversion
    string->list/cursors
    string->vector/cursors
    reverse-list->string
    string-join

    ;; Selection
    string-ref/cursor
    substring/cursors
    string-copy/cursors
    string-take
    string-drop
    string-take-right
    string-drop-right
    string-pad
    string-pad-right
    string-trim
    string-trim-right
    string-trim-both

    ;; Prefixes & suffixes
    string-prefix-length
    string-suffix-length
    string-prefix?
    string-suffix?

    ;; Searching
    string-index
    string-index-right
    string-skip
    string-skip-right
    string-contains
    string-contains-right

    ;; The whole string
    string-reverse
    string-concatenate
    string-concatenate-reverse
    string-fold
    string-fold-right
    string-for-each-cursor
    string-replicate
    string-count
    string-replace
    string-split
    string-filter
    string-remove
  )

  (import (scheme base)
    (liii base)
    (liii error)
    (liii unicode)
  )

  (begin

    ;; ==== Internal data structures ====

    (define-record-type <string-offsets>
      (make-string-offsets bv positions)
      string-offsets?
      (bv string-offsets-bv)
      (positions string-offsets-positions))

    (define-record-type <string-cursor>
      (make-string-cursor-raw offsets char-index)
      string-cursor?
      (offsets string-cursor-offsets)
      (char-index string-cursor-char-index))

    ;; Pre-scan a UTF-8 bytevector to generate position vector
    (define (make-string-positions bv)
      (let ((len (bytevector-length bv)))
        (let loop ((pos 0) (result '(0)))
          (if (>= pos len)
              (list->vector (reverse result))
              (let ((next (bytevector-advance-utf8 bv pos len)))
                (loop next (cons next result)))))))

    ;; ==== Cursor operations ====

    (define (string-cursor-start str)
      (let* ((bv (string->utf8 str))
             (off (make-string-offsets bv (make-string-positions bv))))
        (make-string-cursor-raw off 0)))

    (define (string-cursor-end str)
      (let* ((bv (string->utf8 str))
             (off (make-string-offsets bv (make-string-positions bv)))
             (positions (string-offsets-positions off)))
        (make-string-cursor-raw off (- (vector-length positions) 1))))

    (define (string-cursor-next str cursor)
      (let* ((off (string-cursor-offsets cursor))
             (char-idx (string-cursor-char-index cursor))
             (positions (string-offsets-positions off))
             (max-idx (- (vector-length positions) 1)))
        (if (>= char-idx max-idx)
            (error 'value-error
              "string-cursor-next: already at end cursor")
            (make-string-cursor-raw off (+ char-idx 1)))))

    (define (string-cursor-prev str cursor)
      (let* ((off (string-cursor-offsets cursor))
             (char-idx (string-cursor-char-index cursor)))
        (if (<= char-idx 0)
            (error 'value-error
              "string-cursor-prev: already at start cursor")
            (make-string-cursor-raw off (- char-idx 1)))))

    (define (string-cursor-forward str cursor nchars)
      (let* ((off (string-cursor-offsets cursor))
             (char-idx (string-cursor-char-index cursor))
             (positions (string-offsets-positions off))
             (max-idx (- (vector-length positions) 1))
             (new-idx (+ char-idx nchars)))
        (if (or (< new-idx 0) (> new-idx max-idx))
            (error 'value-error
              "string-cursor-forward: result would be invalid cursor")
            (make-string-cursor-raw off new-idx))))

    (define (string-cursor-back str cursor nchars)
      (string-cursor-forward str cursor (- nchars)))

    (define (string-cursor=? cursor1 cursor2)
      (= (string-cursor-char-index cursor1)
         (string-cursor-char-index cursor2)))

    (define (string-cursor<? cursor1 cursor2)
      (< (string-cursor-char-index cursor1)
         (string-cursor-char-index cursor2)))

    (define (string-cursor>? cursor1 cursor2)
      (> (string-cursor-char-index cursor1)
         (string-cursor-char-index cursor2)))

    (define (string-cursor<=? cursor1 cursor2)
      (<= (string-cursor-char-index cursor1)
          (string-cursor-char-index cursor2)))

    (define (string-cursor>=? cursor1 cursor2)
      (>= (string-cursor-char-index cursor1)
          (string-cursor-char-index cursor2)))

    (define (string-cursor-diff str start end)
      (- (string-cursor-char-index end)
         (string-cursor-char-index start)))

    (define (string-cursor->index str cursor)
      (string-cursor-char-index cursor))

    (define (string-index->cursor str index)
      (let* ((bv (string->utf8 str))
             (off (make-string-offsets bv (make-string-positions bv)))
             (positions (string-offsets-positions off))
             (max-idx (- (vector-length positions) 1)))
        (if (or (< index 0) (> index max-idx))
            (error 'value-error
              "string-index->cursor: index out of range")
            (make-string-cursor-raw off index))))

    ;; ==== Selection ====

    (define (string-ref/cursor str cursor)
      (let* ((off (string-cursor-offsets cursor))
             (bv (string-offsets-bv off))
             (pos (string-offsets-positions off))
             (idx (string-cursor-char-index cursor))
             (start (vector-ref pos idx))
             (end (vector-ref pos (+ idx 1)))
             (char-bv (bytevector-copy bv start end)))
        (integer->char (utf8->codepoint char-bv))))

    (define (substring/cursors str start end)
      (let* ((start-off (if (string-cursor? start)
                            (string-cursor-offsets start)
                            (let ((bv (string->utf8 str)))
                              (make-string-offsets bv (make-string-positions bv)))))
             (end-off (if (string-cursor? end)
                          (string-cursor-offsets end)
                          start-off))
             (pos (string-offsets-positions start-off))
             (bv (string-offsets-bv start-off))
             (start-idx (if (string-cursor? start)
                            (string-cursor-char-index start)
                            start))
             (end-idx (if (string-cursor? end)
                          (string-cursor-char-index end)
                          end))
             (byte-start (vector-ref pos start-idx))
             (byte-end (vector-ref pos end-idx)))
        (utf8->string (bytevector-copy bv byte-start byte-end))))

    (define (string-copy/cursors str . maybe-start+end)
      (let* ((bv (string->utf8 str))
             (off (make-string-offsets bv (make-string-positions bv)))
             (positions (string-offsets-positions off))
             (len (- (vector-length positions) 1)))
        (if (null? maybe-start+end)
            (substring/cursors str
              (make-string-cursor-raw off 0)
              (make-string-cursor-raw off len))
            (let ((start (car maybe-start+end))
                  (rest (cdr maybe-start+end)))
              (let ((end (if (null? rest) len (car rest))))
                (substring/cursors str start end))))))

    ;; ==== String operations ====

    (define (string-take str nchars)
      (let ((end (string-index->cursor str nchars)))
        (substring/cursors str (string-cursor-start str) end)))

    (define (string-drop str nchars)
      (let ((start (string-index->cursor str nchars)))
        (substring/cursors str start (string-cursor-end str))))

    (define (string-take-right str nchars)
      (let* ((end (string-cursor-end str))
             (start (string-cursor-back str end nchars)))
        (substring/cursors str start end)))

    (define (string-drop-right str nchars)
      (let* ((end (string-cursor-end str))
             (new-end (string-cursor-back str end nchars)))
        (substring/cursors str (string-cursor-start str) new-end)))

    ;; ==== Predicates ====

    (define (string-null? str)
      (zero? (string-length str)))

    (define (string-every pred s . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest)))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end)))
        (let loop ((cur start-c))
          (if (string-cursor>=? cur end-c)
              #t
              (let ((result (pred (string-ref/cursor s cur))))
                (if result
                    (let ((next (string-cursor-next s cur)))
                      (if (string-cursor>=? next end-c)
                          result
                          (loop next)))
                    #f))))))

    (define (string-any pred s . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest)))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end)))
        (let loop ((cur start-c))
          (if (string-cursor>=? cur end-c)
              #f
              (let ((result (pred (string-ref/cursor s cur))))
                (if result
                    result
                    (loop (string-cursor-next s cur))))))))

    ;; ==== Fold and iteration ====

    (define (string-fold kons knil s . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest)))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end)))
        (let loop ((acc knil) (cur start-c))
          (if (string-cursor>=? cur end-c)
              acc
              (loop (kons (string-ref/cursor s cur) acc)
                    (string-cursor-next s cur))))))

    (define (string-fold-right kons knil s . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest)))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end)))
        (let loop ((cur start-c))
          (if (string-cursor>=? cur end-c)
              knil
              (kons (string-ref/cursor s cur)
                    (loop (string-cursor-next s cur)))))))

    (define (string-for-each-cursor proc s . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest)))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end)))
        (let loop ((cur start-c))
          (when (string-cursor<? cur end-c)
            (proc cur)
            (loop (string-cursor-next s cur))))))

    (define (string-count pred s . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest)))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end)))
        (let loop ((cur start-c) (count 0))
          (if (string-cursor>=? cur end-c)
              count
              (loop (string-cursor-next s cur)
                    (if (pred (string-ref/cursor s cur))
                        (+ count 1)
                        count))))))

    ;; ==== Searching ====

    (define (string-index s pred . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest)))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end)))
        (let loop ((cur start-c))
          (if (string-cursor>=? cur end-c)
              end-c
              (if (pred (string-ref/cursor s cur))
                  cur
                  (loop (string-cursor-next s cur)))))))

    (define (string-index-right s pred . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest)))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end)))
        (if (string-cursor=? start-c end-c)
            start-c
            (let loop ((cur (string-cursor-prev s end-c)))
              (cond ((pred (string-ref/cursor s cur))
                     (string-cursor-next s cur))
                    ((string-cursor=? cur start-c)
                     start-c
                     (else
                      (loop (string-cursor-prev s cur)))))))))

    (define (string-skip s pred . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest)))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end)))
        (let loop ((cur start-c))
          (if (string-cursor>=? cur end-c)
              end-c
              (if (not (pred (string-ref/cursor s cur)))
                  cur
                  (loop (string-cursor-next s cur)))))))

    (define (string-skip-right s pred . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest)))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end)))
        (if (string-cursor=? start-c end-c)
            start-c
             (let loop ((cur (string-cursor-prev s end-c)))
               (cond ((not (pred (string-ref/cursor s cur)))
                      (string-cursor-next s cur))
                     ((string-cursor=? cur start-c)
                      start-c)
                     (else
                      (loop (string-cursor-prev s cur))))))))

    ;; ==== Trim and Pad ====

    (define* (string-trim s (pred char-whitespace?) (start 0) (end #t))
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (end-idx (if (eq? end #t) char-len end))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end-idx)))
        (let ((trimmed-start (string-skip s pred start end-idx)))
          (substring/cursors s trimmed-start end-c))))

    (define* (string-trim-right s (pred char-whitespace?) (start 0) (end #t))
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (end-idx (if (eq? end #t) char-len end))
             (start-c (string-index->cursor s start))
             (end-c (string-index->cursor s end-idx)))
        (let ((trimmed-end (string-skip-right s pred start end-idx)))
          (substring/cursors s start-c trimmed-end))))

    (define* (string-trim-both s (pred char-whitespace?) (start 0) (end #t))
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (end-idx (if (eq? end #t) char-len end)))
        (let ((trimmed-start (string-skip s pred start end-idx))
              (trimmed-end (string-skip-right s pred start end-idx)))
          (if (string-cursor>=? trimmed-start trimmed-end)
              ""
              (substring/cursors s trimmed-start trimmed-end)))))

    (define* (string-pad s len (char #\space) (start 0) (end #t))
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (end-idx (if (eq? end #t) char-len end))
             (sub (substring/cursors s start end-idx))
             (sub-len (string-cursor-diff sub (string-cursor-start sub) (string-cursor-end sub))))
        (if (>= sub-len len)
            (string-take-right sub len)
            (string-append (make-string (- len sub-len) char) sub))))

    (define* (string-pad-right s len (char #\space) (start 0) (end #t))
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (end-idx (if (eq? end #t) char-len end))
             (sub (substring/cursors s start end-idx))
             (sub-len (string-cursor-diff sub (string-cursor-start sub) (string-cursor-end sub))))
        (if (>= sub-len len)
            (string-take sub len)
            (string-append sub (make-string (- len sub-len) char)))))

    ;; ==== Prefix and Suffix ====

    (define (string-prefix-length s1 s2 . maybe-start+end)
      (let* ((end1-c-raw (string-cursor-end s1))
             (char-len1 (string-cursor-char-index end1-c-raw))
             (end2-c-raw (string-cursor-end s2))
             (char-len2 (string-cursor-char-index end2-c-raw))
             (start1 (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest1 (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end1 (if (null? rest1) char-len1 (car rest1)))
             (rest2 (if (null? rest1) '() (cdr rest1)))
             (start2 (if (null? rest2) 0 (car rest2)))
             (rest3 (if (null? rest2) '() (cdr rest2)))
             (end2 (if (null? rest3) char-len2 (car rest3))))
        (let loop ((i start1) (j start2) (count 0))
          (if (or (>= i end1) (>= j end2))
              count
              (if (char=? (string-ref/cursor s1 (string-index->cursor s1 i))
                         (string-ref/cursor s2 (string-index->cursor s2 j)))
                  (loop (+ i 1) (+ j 1) (+ count 1))
                  count)))))

    (define (string-suffix-length s1 s2 . maybe-start+end)
      (let* ((end1-c-raw (string-cursor-end s1))
             (char-len1 (string-cursor-char-index end1-c-raw))
             (end2-c-raw (string-cursor-end s2))
             (char-len2 (string-cursor-char-index end2-c-raw))
             (start1 (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest1 (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end1 (if (null? rest1) char-len1 (car rest1)))
             (rest2 (if (null? rest1) '() (cdr rest1)))
             (start2 (if (null? rest2) 0 (car rest2)))
             (rest3 (if (null? rest2) '() (cdr rest2)))
             (end2 (if (null? rest3) char-len2 (car rest3))))
        (let loop ((i (- end1 1)) (j (- end2 1)) (count 0))
          (if (or (< i start1) (< j start2))
              count
              (if (char=? (string-ref/cursor s1 (string-index->cursor s1 i))
                         (string-ref/cursor s2 (string-index->cursor s2 j)))
                  (loop (- i 1) (- j 1) (+ count 1))
                  count)))))

    (define (string-prefix? s1 s2 . maybe-start+end)
      (let* ((end1-c-raw (string-cursor-end s1))
             (char-len1 (string-cursor-char-index end1-c-raw))
             (end2-c-raw (string-cursor-end s2))
             (char-len2 (string-cursor-char-index end2-c-raw))
             (start1 (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest1 (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end1 (if (null? rest1) char-len1 (car rest1)))
             (rest2 (if (null? rest1) '() (cdr rest1)))
             (start2 (if (null? rest2) 0 (car rest2)))
             (rest3 (if (null? rest2) '() (cdr rest2)))
             (end2 (if (null? rest3) char-len2 (car rest3))))
        (let ((len1 (- end1 start1)))
          (and (<= len1 (- end2 start2))
               (= (string-prefix-length s1 s2 start1 end1 start2 end2) len1)))))

    (define (string-suffix? s1 s2 . maybe-start+end)
      (let* ((end1-c-raw (string-cursor-end s1))
             (char-len1 (string-cursor-char-index end1-c-raw))
             (end2-c-raw (string-cursor-end s2))
             (char-len2 (string-cursor-char-index end2-c-raw))
             (start1 (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest1 (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end1 (if (null? rest1) char-len1 (car rest1)))
             (rest2 (if (null? rest1) '() (cdr rest1)))
             (start2 (if (null? rest2) 0 (car rest2)))
             (rest3 (if (null? rest2) '() (cdr rest2)))
             (end2 (if (null? rest3) char-len2 (car rest3))))
        (let ((len1 (- end1 start1)))
          (and (<= len1 (- end2 start2))
               (= (string-suffix-length s1 s2 start1 end1 start2 end2) len1)))))

    ;; ==== Contains ====

    (define (string-contains s1 s2 . maybe-start+end)
      (let* ((end1-c-raw (string-cursor-end s1))
             (char-len1 (string-cursor-char-index end1-c-raw))
             (end2-c-raw (string-cursor-end s2))
             (char-len2 (string-cursor-char-index end2-c-raw))
             (start1 (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest1 (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end1 (if (null? rest1) char-len1 (car rest1)))
             (rest2 (if (null? rest1) '() (cdr rest1)))
             (start2 (if (null? rest2) 0 (car rest2)))
             (rest3 (if (null? rest2) '() (cdr rest2)))
             (end2 (if (null? rest3) char-len2 (car rest3))))
        (let ((s2-len (- end2 start2)))
          (if (zero? s2-len)
              (string-index->cursor s1 start1)
              (let loop ((i start1))
                (if (> (+ i s2-len) end1)
                    #f
                    (if (string-prefix? (substring/cursors s1 i (+ i s2-len)) (substring/cursors s2 start2 end2))
                        (string-index->cursor s1 i)
                        (loop (+ i 1)))))))))

    (define (string-contains-right s1 s2 . maybe-start+end)
      (let* ((end1-c-raw (string-cursor-end s1))
             (char-len1 (string-cursor-char-index end1-c-raw))
             (end2-c-raw (string-cursor-end s2))
             (char-len2 (string-cursor-char-index end2-c-raw))
             (start1 (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest1 (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end1 (if (null? rest1) char-len1 (car rest1)))
             (rest2 (if (null? rest1) '() (cdr rest1)))
             (start2 (if (null? rest2) 0 (car rest2)))
             (rest3 (if (null? rest2) '() (cdr rest2)))
             (end2 (if (null? rest3) char-len2 (car rest3))))
        (let ((s2-len (- end2 start2)))
          (if (zero? s2-len)
              (string-index->cursor s1 end1)
              (let loop ((i (- end1 s2-len)))
                (if (< i start1)
                    #f
                    (if (string-prefix? (substring/cursors s1 i (+ i s2-len)) (substring/cursors s2 start2 end2))
                        (string-index->cursor s1 i)
                        (loop (- i 1)))))))))

    ;; ==== String manipulation ====

    (define (string-reverse s . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest))))
        (let loop ((i start) (result '()))
          (if (>= i end)
              (list->string result)
              (loop (+ i 1) (cons (string-ref/cursor s (string-index->cursor s i)) result))))))

    (define (string-concatenate string-list)
      (let loop ((lst string-list) (result ""))
        (if (null? lst)
            result
            (loop (cdr lst) (string-append result (car lst))))))

    (define (string-concatenate-reverse string-list . maybe-final+end)
      (let* ((final (if (null? maybe-final+end) "" (car maybe-final+end)))
             (rest (if (null? maybe-final+end) '() (cdr maybe-final+end)))
             (end (if (null? rest) (string-length final) (car rest)))
             (final-part (substring/cursors final 0 end)))
        (let loop ((lst (reverse string-list)) (result final-part))
          (if (null? lst)
              result
              (loop (cdr lst) (string-append (car lst) result))))))

    (define (string-replicate s from . maybe-to+start+end)
      (let* ((start (if (null? maybe-to+start+end) 0 (cadr maybe-to+start+end)))
             (rest1 (if (null? maybe-to+start+end) '() (cddr maybe-to+start+end)))
             (end (if (null? rest1) (string-length s) (car rest1)))
             (to (if (null? maybe-to+start+end) (+ from (- end start)) (car maybe-to+start+end)))
             (slen (- end start))
             (anslen (- to from)))
        (cond ((zero? anslen) "")
              ((zero? slen) (error 'value-error "Cannot replicate empty substring"))
              (else
               (let loop ((i 0) (result '()))
                 (if (>= i anslen)
                     (list->string (reverse result))
                     (let* ((src-idx (+ start (modulo (+ from i) slen)))
                            (ch (string-ref/cursor s (string-index->cursor s src-idx))))
                       (loop (+ i 1) (cons ch result)))))))))

    (define (string-replace s1 s2 start1 end1 . maybe-start+end)
      (let* ((end2-c-raw (string-cursor-end s2))
             (char-len2 (string-cursor-char-index end2-c-raw))
             (start2 (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end2 (if (null? rest) char-len2 (car rest)))
             (before (substring/cursors s1 0 start1))
             (middle (substring/cursors s2 start2 end2))
             (after (substring/cursors s1 end1 (string-length s1))))
        (string-append before middle after)))

    (define (string-split s delimiter . args)
      (let* ((slen (string-length s))
             (grammar (if (null? args) 'infix (car args)))
             (rest1 (if (null? args) '() (cdr args)))
             (limit (if (null? rest1) #f (car rest1)))
             (rest2 (if (null? rest1) '() (cdr rest1)))
             (start (if (null? rest2) 0 (car rest2)))
             (rest3 (if (null? rest2) '() (cdr rest2)))
             (end (if (null? rest3) slen (car rest3))))
        (cond ((= start end)
               (if (eq? grammar 'strict-infix)
                   (error 'value-error "empty string cannot be split with strict-infix grammar")
                   '()))
              ((string-null? delimiter)
               (let loop ((i start) (result '()) (n 0))
                 (cond ((= i end) (reverse result))
                       ((and limit (>= n limit))
                        (reverse (cons (substring/cursors s i end) result)))
                       (else (loop (+ i 1)
                                  (cons (string (string-ref/cursor s (string-index->cursor s i))) result)
                                  (+ n 1))))))
              (else
               (let ((dlen (string-length delimiter)))
                 (define (finish r c)
                   (let ((rest-str (substring/cursors s c end)))
                     (if (and (eq? grammar 'suffix) (string-null? rest-str))
                         (reverse r)
                         (reverse (cons rest-str r)))))
                 (define (scan r c n)
                   (if (and limit (>= n limit))
                       (finish r c)
                       (let ((i (string-contains s delimiter c end)))
                         (if i
                             (let ((fragment (substring/cursors s c (string-cursor->index s i))))
                               (if (and (= n 0) (eq? grammar 'prefix) (string-null? fragment))
                                   (scan r (+ (string-cursor->index s i) dlen) (+ n 1))
                                   (scan (cons fragment r)
                                         (+ (string-cursor->index s i) dlen)
                                         (+ n 1))))
                             (finish r c)))))
                 (scan '() start 0))))))

    (define (string-filter pred s . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest))))
        (let loop ((i start) (result '()))
          (if (>= i end)
              (list->string (reverse result))
              (let ((ch (string-ref/cursor s (string-index->cursor s i))))
                (if (pred ch)
                    (loop (+ i 1) (cons ch result))
                    (loop (+ i 1) result)))))))

    (define (string-remove pred s . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest))))
        (let loop ((i start) (result '()))
          (if (>= i end)
              (list->string (reverse result))
              (let ((ch (string-ref/cursor s (string-index->cursor s i))))
                (if (pred ch)
                    (loop (+ i 1) result)
                    (loop (+ i 1) (cons ch result))))))))

    ;; ==== Constructors ====

    (define (string-tabulate proc len)
      (let loop ((i 0) (result '()))
        (if (>= i len)
            (list->string (reverse result))
            (loop (+ i 1) (cons (proc i) result)))))

    (define (string-unfold p f g seed . base+make-final)
      (let* ((base (if (null? base+make-final) "" (car base+make-final)))
             (rest (if (null? base+make-final) '() (cdr base+make-final)))
             (make-final (if (null? rest) (lambda (x) "") (car rest))))
        (let loop ((seed seed) (result '()))
          (if (p seed)
              (string-append base (list->string (reverse result)) (make-final seed))
              (loop (g seed) (cons (f seed) result))))))

    (define (string-unfold-right p f g seed . base+make-final)
      (let* ((base (if (null? base+make-final) "" (car base+make-final)))
             (rest (if (null? base+make-final) '() (cdr base+make-final)))
             (make-final (if (null? rest) (lambda (x) "") (car rest))))
        (let loop ((seed seed) (result '()))
          (if (p seed)
              (string-append (make-final seed) (list->string result) base)
              (loop (g seed) (cons (f seed) result))))))

    ;; ==== Conversion ====

    (define (string->list/cursors s . maybe-start+end)
      (let* ((end-c-raw (string-cursor-end s))
             (char-len (string-cursor-char-index end-c-raw))
             (start (if (null? maybe-start+end) 0 (car maybe-start+end)))
             (rest (if (null? maybe-start+end) '() (cdr maybe-start+end)))
             (end (if (null? rest) char-len (car rest))))
        (let loop ((i start) (result '()))
          (if (>= i end)
              (reverse result)
              (loop (+ i 1) (cons (string-ref/cursor s (string-index->cursor s i)) result))))))

    (define (string->vector/cursors s . maybe-start+end)
      (list->vector (apply string->list/cursors s maybe-start+end)))

    (define (reverse-list->string clist)
      (list->string (reverse clist)))

    (define* (string-join string-list (delimiter " ") (grammar 'infix))
      (cond ((null? string-list)
             (if (eq? grammar 'strict-infix)
                 (error 'value-error "string-join: empty list with strict-infix")
                 ""))
            ((eq? grammar 'infix)
             (let loop ((lst string-list) (result ""))
               (if (null? lst)
                   result
                   (if (null? (cdr lst))
                       (loop (cdr lst) (string-append result (car lst)))
                       (loop (cdr lst) (string-append result (car lst) delimiter))))))
            ((eq? grammar 'strict-infix)
             (if (null? string-list)
                 (error 'value-error "string-join: empty list with strict-infix")
                 (let loop ((lst string-list) (result ""))
                   (if (null? lst)
                       result
                       (if (null? (cdr lst))
                           (loop (cdr lst) (string-append result (car lst)))
                           (loop (cdr lst) (string-append result (car lst) delimiter)))))))
            ((eq? grammar 'suffix)
             (let loop ((lst string-list) (result ""))
               (if (null? lst)
                   result
                   (loop (cdr lst) (string-append result (car lst) delimiter)))))
            ((eq? grammar 'prefix)
             (let loop ((lst string-list) (result ""))
               (if (null? lst)
                   result
                   (loop (cdr lst) (string-append result delimiter (car lst))))))
            (else
             (error 'value-error "string-join: invalid grammar"))))

  )
)


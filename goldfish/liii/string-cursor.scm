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

    ;; ==== Placeholder for remaining functions ====
    ;; These will be implemented in subsequent commits

  )
)

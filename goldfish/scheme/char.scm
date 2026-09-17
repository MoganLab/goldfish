;; Character and case-insensitive string operations.
;;
;; Unicode properties belong to the runtime boundary.  Keeping the tables in
;; C++ makes this library cheap to load and, more importantly, avoids a large
;; Scheme-side initialization program in the native artifact.

(define-library (scheme char)
  (import (scheme base) (liii unicode) (goldfish))
  (export char-upcase char-downcase char-foldcase
          char-upper-case? char-lower-case? digit-value char-numeric?
          char-alphabetic? char-whitespace?
          char-ci=? char-ci<? char-ci>? char-ci<=? char-ci>=?
          string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
          string-upcase string-downcase string-foldcase)
  (begin
    (define char-upcase g_char-upcase)
    (define char-downcase g_char-downcase)
    (define char-foldcase
      (if (defined? 'g_char-foldcase)
          g_char-foldcase
          (lambda (ch)
            (if (char=? ch #\x3C2) #\x3C3 (char-downcase ch)))))
    (define char-alphabetic? g_char-alphabetic?)
    (define char-numeric?
      (if (defined? 'g_char-numeric?)
          g_char-numeric?
          (lambda (ch) (not (eqv? (digit-value ch) #f)))))
    (define char-whitespace?
      (if (defined? 'g_char-whitespace?)
          g_char-whitespace?
          (lambda (ch)
            (memv (char->integer ch)
                  '(9 10 11 12 13 32 133 160 5760 8192 8193 8194
                    8195 8196 8197 8198 8199 8200 8201 8202 8232 8233
                    8239 8287 12288)))))
    (define char-upper-case? g_char-upper-case?)
    (define char-lower-case? g_char-lower-case?)

    (define (digit-value ch)
      (unless (char? ch)
        (error 'type-error "digit-value: parameter must be character"))
      (case ch
        ((#\0 #\x3007 #\x96F6 #\xC601) 0)
        ((#\1 #\x4E00 #\x58F1 #\x58F9 #\xC77C #\x20B20) 1)
        ((#\2 #\x4E8C #\x5F10 #\x8D30 #\xC774 #\x20129) 2)
        ((#\3 #\x4E09 #\x53C2 #\x53C1 #\xC0BC #\x20027) 3)
        ((#\4 #\x56DB #\x8086 #\xC0AC #\x2629A) 4)
        ((#\5 #\x4E94 #\x4F0D #\xC624 #\x2013C) 5)
        ((#\6 #\x516D #\x9678 #\x9646 #\xC721 #\x264B9) 6)
        ((#\7 #\x4E03 #\x67D2 #\xCE60 #\x26271) 7)
        ((#\8 #\x516B #\x634C #\xD314 #\x20969) 8)
        ((#\9 #\x4E5D #\x7396 #\xAD6C #\x200E9) 9)
        (else #f)))

    (define (char-ci=? a b . rest)
      (let loop ((last (char-foldcase a))
                 (next b) (more rest))
        (unless (char? next) (error 'type-error "char-ci=?: parameter must be character"))
        (let ((folded (char-foldcase next)))
          (and (char=? last folded)
               (if (null? more) #t
                   (loop folded (car more) (cdr more)))))))

    (define (char-ci<? a b . rest)
      (let loop ((last (char-foldcase a)) (next b) (more rest))
        (unless (char? next) (error 'type-error "char-ci<?: parameter must be character"))
        (let ((folded (char-foldcase next)))
          (and (char<? last folded)
               (if (null? more) #t (loop folded (car more) (cdr more)))))))

    (define (char-ci>? a b . rest)
      (let loop ((last (char-foldcase a)) (next b) (more rest))
        (unless (char? next) (error 'type-error "char-ci>?: parameter must be character"))
        (let ((folded (char-foldcase next)))
          (and (char>? last folded)
               (if (null? more) #t (loop folded (car more) (cdr more)))))))

    (define (char-ci<=? a b . rest)
      (let loop ((last (char-foldcase a)) (next b) (more rest))
        (unless (char? next) (error 'type-error "char-ci<=?: parameter must be character"))
        (let ((folded (char-foldcase next)))
          (and (char<=? last folded)
               (if (null? more) #t (loop folded (car more) (cdr more)))))))

    (define (char-ci>=? a b . rest)
      (let loop ((last (char-foldcase a)) (next b) (more rest))
        (unless (char? next) (error 'type-error "char-ci>=?: parameter must be character"))
        (let ((folded (char-foldcase next)))
          (and (char>=? last folded)
               (if (null? more) #t (loop folded (car more) (cdr more)))))))

    (define (utf8-string-map proc str)
      (let* ((bv (string->utf8 str)) (len (bytevector-length bv)))
        (let loop ((pos 0) (result '()))
          (if (>= pos len)
              (apply utf8-string (reverse result))
              (let ((next (bytevector-advance-utf8 bv pos len)))
                (loop next
                      (cons (proc (integer->char (utf8->codepoint-at bv pos)))
                            result)))))))

    (define (string-upcase str) (utf8-string-map char-upcase str))
    (define (string-downcase str) (utf8-string-map char-downcase str))
    (define (string-foldcase str) (utf8-string-map char-foldcase str))

    (define (string-ci=? a b . rest)
      (let loop ((last (string-foldcase a)) (next b) (more rest))
        (let ((folded (string-foldcase next)))
          (and (string=? last folded)
               (if (null? more) #t (loop folded (car more) (cdr more)))))))
    (define (string-ci<? a b . rest)
      (let loop ((last (string-foldcase a)) (next b) (more rest))
        (let ((folded (string-foldcase next)))
          (and (string<? last folded)
               (if (null? more) #t (loop folded (car more) (cdr more)))))))
    (define (string-ci>? a b . rest)
      (let loop ((last (string-foldcase a)) (next b) (more rest))
        (let ((folded (string-foldcase next)))
          (and (string>? last folded)
               (if (null? more) #t (loop folded (car more) (cdr more)))))))
    (define (string-ci<=? a b . rest)
      (let loop ((last (string-foldcase a)) (next b) (more rest))
        (let ((folded (string-foldcase next)))
          (and (string<=? last folded)
               (if (null? more) #t (loop folded (car more) (cdr more)))))))
    (define (string-ci>=? a b . rest)
      (let loop ((last (string-foldcase a)) (next b) (more rest))
        (let ((folded (string-foldcase next)))
          (and (string>=? last folded)
               (if (null? more) #t (loop folded (car more) (cdr more)))))))
  ))

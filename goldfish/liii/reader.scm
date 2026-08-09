(import (liii string-cursor))

(define (read-hash-procedure ch)
  ;; TODO
  #f)

(define (delimiter? ch)
  (case ch
    ((#\( #\) #\[ #\]
      #\; #\" #\space #\return #\xc #\newline #\tab)
     #t)
    (else #f)))

(define* (read (port (current-input-port)))
  (define filename (port-filename port))
  (define (next) (read-char port))
  (define (peek) (peek-char port))

  (define (take-until first pred)
    (let lp ((out (list first)))
      (let ((ch (peek)))
        (if (or (eof-object? ch) (pred ch))
          (reverse-list->string out)
          (begin
            (next)
            (lp (cons ch out)))))))

  (define (read-parenthesized rdelim)
    (let loop ((ch (next-non-whitespace)))
      (when (eof-object? ch)
        (error "unexpected end of input" rdelim))
      (cond
        ((eqv? ch rdelim) '())
        ((or (eqv? ch #\)) (eqv? ch #\]))
         (error "mismatched close paren" ch))
        (else (cons (read-expr ch)
                    (loop (next-non-whitespace)))))))

  (define (read-token ch)
    (take-until ch delimiter?))

  (define (read-symbol ch)
    (string->symbol (read-token ch)))

  (define (read-string rdelim)
    (let loop ((out '()))
      (let ((ch (next)))
        (cond
          ((eof-object? ch)
           (error "unexpected end of input while reading string"))
          ((eqv? ch rdelim)
           (reverse-list->string out))
          ((eqv? ch #\\)
           (let ((ch (next)))
             (when (eof-object? ch)
               (error "unexpected end of input while reading string"))
             (cond
               ((eqv? ch #\newline)
                (when (hungry-eol-escapes?)
                  (let skip ()
                    (let ((ch (peek)))
                      (when (and (not (eof-object? ch))
                                 (or (eqv? ch #\tab)
                                     (eq? (char-general-category ch) 'Zs)))
                        (next)
                        (skip)))))
                (loop out))
               ((eqv? ch rdelim)
                (loop (cons rdelim out)))
               (else
                (loop
                  (cons
                    (case ch
                      ((#\| #\\ #\()) ch
                      ((#\0)          #\null)
                      ((#\f)          #\xc) ; #\ff
                      ((#\n)          #\newline)
                      ((#\r)          #\return)
                      ((#\t)          #\tab)
                      ((#\a)          #\alarm)
                      ((#\v)          #\xb) ; #\vtab
                      ((#\b)          #\backspace)
                      ((#\x)          (read-hex-escape))
                      (else           (error "invalid character in escape sequence" ch))))
                  out))))))
        (else
          (loop (cons ch out))))))

  (define (read-number ch)
    (let ((str (read-token ch)))
      (or (string->number str)
          (string->symbol str))))

  (define (read-boolean ch)
    (case ch
      ((#\t #\T) #t)
      ((#\f #\F) #f)))

  (define (parse-number-prefix str)
    ;; Parse the leading radix/exactness prefixes of a "#..." string.
    ;; Returns (list body-start radix exactness) or #f if the prefix is invalid.
    ;; exactness: 0 = none, 1 = #e, 2 = #i
    (let ((len (string-length str)))
      (let loop ((i 1) (radix 0) (exact 0))
        (if (>= i len)
          #f
          (let ((ch (string-ref str i)))
            (cond
              ((eqv? ch #\#)
               (if (and (< (+ i 1) len)
                        (memv (string-ref str (+ i 1))
                              '(#\b #\B #\o #\O #\d #\D #\x #\X #\e #\E #\i #\I)))
                 (loop (+ i 1) radix exact)
                 #f))
              ((memv ch '(#\b #\B))
               (if (not (= radix 0)) #f (loop (+ i 1) 2 exact)))
              ((memv ch '(#\o #\O))
               (if (not (= radix 0)) #f (loop (+ i 1) 8 exact)))
              ((memv ch '(#\d #\D))
               (if (not (= radix 0)) #f (loop (+ i 1) 10 exact)))
              ((memv ch '(#\x #\X))
               (if (not (= radix 0)) #f (loop (+ i 1) 16 exact)))
              ((memv ch '(#\e #\E))
               (if (not (= exact 0)) #f (loop (+ i 1) radix 1)))
              ((memv ch '(#\i #\I))
               (if (not (= exact 0)) #f (loop (+ i 1) radix 2)))
              (else
               (list i (if (= radix 0) 10 radix) exact))))))))

  (define (string->prefixed-number str)
    (let ((p (parse-number-prefix str)))
      (and p
        (let* ((body (substring str (car p) (string-length str)))
               (n (string->number body (cadr p)))
               (exactness (caddr p)))
          (and n
            (cond
              ((= exactness 1) (exact n))
              ((= exactness 2) (inexact n))
              (else n)))))))

  (define (read-prefixed-number ch)
    (let ((str (string-append "#" (take-until ch delimiter?))))
      (or (string->prefixed-number str)
          (error 'read-error "invalid number" str))))

  (define (read-bytevector)
    (let loop ((ch (next-non-whitespace)) (acc '()))
      (if (eof-object? ch)
        (error 'read-error "unexpected end of input in bytevector")
        (if (eqv? ch #\))
          (apply bytevector (reverse acc))
          (let ((b (read-expr ch)))
            (if (and (exact-integer? b) (<= 0 b 255))
              (loop (next-non-whitespace) (cons b acc))
              (error 'read-error "bytevector element out of range" b)))))))

  (define char-names
    (list (cons "alarm" #\alarm)
          (cons "backspace" #\backspace)
          (cons "delete" #\delete)
          (cons "escape" #\escape)
          (cons "newline" #\newline)
          (cons "null" #\null)
          (cons "return" #\return)
          (cons "space" #\space)
          (cons "tab" #\tab)))

  (define (char-hex-digit? ch)
    (let ((n (char->integer ch)))
      (or (<= (char->integer #\0) n (char->integer #\9))
          (<= (char->integer #\a) n (char->integer #\f))
          (<= (char->integer #\A) n (char->integer #\F)))))

  (define (char-letter? ch)
    (let ((n (char->integer ch)))
      (or (<= (char->integer #\a) n (char->integer #\z))
          (<= (char->integer #\A) n (char->integer #\Z)))))

  (define (hex-digit-value ch)
    (let ((n (char->integer ch)))
      (cond
        ((<= (char->integer #\0) n (char->integer #\9))
         (- n (char->integer #\0)))
        ((<= (char->integer #\a) n (char->integer #\f))
         (+ (- n (char->integer #\a)) 10))
        (else
         (+ (- n (char->integer #\A)) 10)))))

  (define (read-hex-char)
    (let loop ((n 0))
      (let ((ch (peek)))
        (if (and (not (eof-object? ch)) (char-hex-digit? ch))
          (begin
            (next)
            (loop (+ (* n 16) (hex-digit-value ch))))
          (integer->char n)))))

  (define (read-character)
    (let ((ch (next)))
      (cond
        ((eof-object? ch)
         (error 'read-error "unexpected end of input in character"))
        ((memv ch '(#\x #\X))
         (if (and (not (eof-object? (peek))) (char-hex-digit? (peek)))
           (read-hex-char)
           ch))
        ((char-letter? ch)
         (let ((token (take-until ch delimiter?)))
           (cond
             ((assoc token char-names) => cdr)
             ((= (string-length token) 1) ch)
             (else (error 'read-error "invalid character" token)))))
        (else ch))))

  (define (read-subexpression what)
    (let ((ch (next-non-whitespace)))
      (if (eof-object? ch)
        (error 'read-error "unexpected end of input" what)
        (read-expr ch))))

  (define (read-vector)
    (let loop ((ch (next-non-whitespace)) (acc '()))
      (if (eof-object? ch)
        (error 'read-error "unexpected end of input in vector")
        (if (eqv? ch #\))
          (list->vector (reverse acc))
          (let ((elem (read-expr ch)))
            (loop (next-non-whitespace) (cons elem acc)))))))

  (define (read-sharp)
    (let ((ch (next)))
      (cond
        ((eof-object? ch)
         (error "unexpected end of input after #"))
        ((read-hash-procedure ch)
         => (lambda (proc) (proc ch port)))
        (else
          (case ch
            ((#\\) (read-character))
            ((#\() (read-vector))
            ((#\t #\T #\f #\F) (read-boolean ch))
            ((#\b #\B #\o #\O #\d #\D #\x #\X #\e #\E #\i #\I)
             (read-prefixed-number ch))
            ((#\u)
             (if (not (eqv? (peek) #\8))
               (error 'read-error "invalid #u8")
               (begin
                 (next)
                 (if (not (eqv? (peek) #\())
                   (error 'read-error "invalid #u8")
                   (begin
                     (next)
                     (read-bytevector))))))
            (else
              (error "Unknown # object" (string #\# ch))))))))

  (define (read-expr ch)
    (case ch
      ((#\[) (read-parenthesized #\]))
      ((#\() (read-parenthesized #\)))
      ((#\") (read-string))
      ((#\|) (string->symbol (read-string ch)))
      ((#\') (list 'quote (read-subexpression "quoted expression")))
      ((#\`) (list 'quasiquote (read-subexpression "quasiquoted expression")))
      ((#\,)
       (cond
         ((eq? #\@ (peek))
          (next)
          (list 'unquote-splicing (read-subexpression "subexpression of ,@")))
         (else
           (list 'unquote (read-subexpression "unquoted expression")))))
      ((#\#) (read-sharp))
      ((#\)) (error "unexpected \")\""))
      ((#\]) (error "unexpected \"]\""))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\- #\.)
       (read-number ch))
      (else (read-symbol ch))))

  (define (skip-comment)
    (let ((ch (next)))
      (cond
        ((eof-object? ch)   ch)
        ((eq? ch #\newline) (next))
        (else               (skip-comment)))))

  (define (next-non-whitespace)
    (let loop ((ch (next)))
      (case ch
        ((#\;) (loop (skip-whitespace)))
        ((#\#)
         (case (peek)
           ;; TODO: shebang?
           ;;       #;<datum> comment
           ((#\|)
            (cond
              ((read-hash-procedure #\|) ch)
              (else (next)
                    (skip-block-comment)
                    (next-non-whitespace))))
           (else ch)))
        ;; TOOD: #\xc: #\ff Form Feed, ASCII 12
        ((#\space #\return #\xc #\newline #\tab)
         (next-non-whitespace))
        (else ch))))

  (let ((ch (next-non-whitespace)))
    (if (eof-object? ch)
      ch
      (read-expr ch))))

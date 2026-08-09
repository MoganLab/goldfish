(import (liii string-cursor))

(define (read-hash-procedure ch)
  ;; TODO
  #f)

(define (delimiter? ch)
  (case ch
    ((#\( #\) #\[ #\]
      #\; #\" #\|
      #\space #\return #\xc #\newline #\tab)
     #t)
    (else #f)))

(define fold-case-ports '())

(define (del-eqv key alist)
  (let loop ((l alist) (acc '()))
    (if (null? l)
      (reverse acc)
      (if (eqv? (caar l) key)
        (loop (cdr l) acc)
        (loop (cdr l) (cons (car l) acc))))))

(define (fold-string str)
  ;; ASCII case folding (R7RS string-foldcase, restricted to ASCII)
  (let loop ((i 0) (out '()))
    (if (= i (string-length str))
      (reverse-list->string out)
      (let ((n (char->integer (string-ref str i))))
        (loop (+ i 1)
              (cons (if (<= (char->integer #\A) n (char->integer #\Z))
                      (integer->char (+ n 32))
                      (integer->char n))
                    out))))))

(define* (read (port (current-input-port)))
  (define filename (port-filename port))
  (define (next) (read-char port))
  (define (peek) (peek-char port))
  (define labels '())
  (define pending '())

  (define (fold-case?)
    (let ((e (assv port fold-case-ports)))
      (and e (cdr e))))

  (define (set-fold-case! v)
    (set! fold-case-ports
      (cons (cons port v) (del-eqv port fold-case-ports))))

  (define (del-assv n alist)
    (let loop ((l alist) (acc '()))
      (if (null? l)
        (reverse acc)
        (if (= (caar l) n)
          (loop (cdr l) acc)
          (loop (cdr l) (cons (car l) acc))))))

  (define (digit-value ch)
    (- (char->integer ch) (char->integer #\0)))

  (define (substitute! obj target repl)
    ;; replace every occurrence of the placeholder target inside obj with repl
    (let walk ((o obj) (visited '()))
      (cond
        ((eq? o target) repl)
        ((memq o visited) o)
        ((pair? o)
         (set! visited (cons o visited))
         (when (eq? (car o) target) (set-car! o repl))
         (when (eq? (cdr o) target) (set-cdr! o repl))
         (walk (car o) visited)
         (walk (cdr o) visited)
         o)
        ((vector? o)
         (set! visited (cons o visited))
         (let ((len (vector-length o)))
           (do ((i 0 (+ i 1)))
               ((= i len))
             (let ((e (vector-ref o i)))
               (if (eq? e target)
                 (vector-set! o i repl)
                 (walk e visited)))))
         o)
        (else o))))

  (define (read-label n)
    (let ((ch (next)))
      (cond
        ((eqv? ch #\=)
         (let* ((placeholder (vector 'reader-placeholder n)))
           (set! pending (cons (cons n placeholder) pending))
           (let ((dch (next-non-whitespace)))
             (if (eof-object? dch)
               (error 'read-error "label with no datum")
               (let ((datum (read-expr dch)))
                 (if (eq? datum placeholder)
                   (error 'read-error "self-referential label" n)
                   (begin
                     (substitute! datum placeholder datum)
                     (set! labels (cons (cons n datum) labels))
                     (set! pending (del-assv n pending))
                     datum)))))))
        ((eqv? ch #\#)
         (let ((lbl (assv n labels)))
           (if lbl
             (cdr lbl)
             (let ((pend (assv n pending)))
               (if pend
                 (cdr pend)
                 (error 'read-error "forward reference to label" n))))))
        (else
         (error 'read-error "invalid label" n)))))

  (define (read-label-number first-digit)
    (let loop ((n (digit-value first-digit)))
      (let ((ch (peek)))
        (if (char-digit? ch)
          (begin
            (next)
            (loop (+ (* n 10) (digit-value ch))))
          (read-label n)))))

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
        (error 'read-error "unexpected end of input" rdelim))
      (cond
        ((eqv? ch rdelim) '())
        ((or (eqv? ch #\)) (eqv? ch #\]))
         (error 'read-error "mismatched close paren" ch))
        ((and (eqv? ch #\.) (or (eof-object? (peek)) (delimiter? (peek))))
         (let ((tail-ch (next-non-whitespace)))
           (if (eof-object? tail-ch)
             (error 'read-error "unexpected end of input" rdelim)
             (let ((tail (read-expr tail-ch)))
               (let ((close (next-non-whitespace)))
                 (if (eqv? close rdelim)
                   tail
                   (error 'read-error "expected closing delimiter" rdelim)))))))
        (else (cons (read-expr ch)
                    (loop (next-non-whitespace)))))))

  (define (read-token ch)
    (take-until ch delimiter?))

  (define (read-symbol ch)
    (string->symbol (read-token ch)))

  (define (read-string . args)
    (let ((rdelim (if (null? args) #\" (car args))))
      (let loop ((out '()))
        (let ((ch (next)))
          (cond
            ((eof-object? ch)
             (error 'read-error "unexpected end of input while reading string"))
            ((eqv? ch rdelim)
             (reverse-list->string out))
            ((eqv? ch #\\)
             (let ((ch (next)))
               (when (eof-object? ch)
                 (error 'read-error "unexpected end of input while reading string"))
               (cond
                 ((or (eqv? ch #\newline) (eqv? ch #\return))
                  (let skip ()
                    (let ((p (peek)))
                      (when (and (not (eof-object? p))
                                 (or (eqv? p #\space) (eqv? p #\tab)))
                        (next)
                        (skip))))
                  (loop out))
                 ((eqv? ch rdelim)
                  (loop (cons rdelim out)))
                 (else
                  (loop
                    (cons
                      (case ch
                        ((#\a) #\alarm)
                        ((#\b) #\backspace)
                        ((#\t) #\tab)
                        ((#\n) #\newline)
                        ((#\r) #\return)
                        ((#\" #\\ #\|) ch)
                        ((#\x) (read-hex-escape))
                        (else (error 'read-error "invalid character in escape sequence" ch)))
                      out))))))
            (else
              (loop (cons ch out))))))))

  (define (read-symbol ch)
    (let ((str (read-token ch)))
      (if (valid-identifier? str)
        (string->symbol (if (fold-case?) (fold-string str) str))
        (error 'read-error "invalid token" str))))

  (define (char-digit? ch)
    (and (char? ch) (char<=? #\0 ch #\9)))

  (define (identifier-initial? ch)
    (or (char-letter? ch)
        (memv ch '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\@ #\^ #\_ #\~))))

  (define (identifier-subsequent? ch)
    (or (identifier-initial? ch)
        (char-digit? ch)
        (memv ch '(#\+ #\- #\. #\@))))

  (define (sign-subsequent? ch)
    (or (identifier-initial? ch)
        (memv ch '(#\+ #\- #\@))))

  (define (dot-subsequent? ch)
    (or (sign-subsequent? ch) (eqv? ch #\.)))

  (define (valid-identifier? str)
    ;; R7RS 7.1.1 <identifier>, for tokens not beginning with a vertical line
    (let ((len (string-length str)))
      (and (> len 0)
        (let ((c0 (string-ref str 0)))
          (cond
            ((identifier-initial? c0)
             (let loop ((i 1))
               (if (= i len)
                 #t
                 (and (identifier-subsequent? (string-ref str i))
                      (loop (+ i 1))))))
            ((memv c0 '(#\+ #\-))
             (if (= len 1)
               #t
               (let ((c1 (string-ref str 1)))
                 (cond
                   ((eqv? c1 #\.)
                    (and (> len 2)
                         (dot-subsequent? (string-ref str 2))
                         (let loop ((i 3))
                           (if (= i len)
                             #t
                             (and (identifier-subsequent? (string-ref str i))
                                  (loop (+ i 1)))))))
                   ((sign-subsequent? c1)
                    (let loop ((i 2))
                      (if (= i len)
                        #t
                        (and (identifier-subsequent? (string-ref str i))
                             (loop (+ i 1))))))
                   (else #f)))))
            ((eqv? c0 #\.)
             (and (> len 1)
                  (dot-subsequent? (string-ref str 1))
                  (let loop ((i 2))
                    (if (= i len)
                      #t
                      (and (identifier-subsequent? (string-ref str i))
                           (loop (+ i 1)))))))
            (else #f))))))

  (define (pure-imaginary-number str)
    ;; +i -i +2i -2i ... : real part omitted
    (let ((len (string-length str)))
      (and (> len 0)
        (let ((last (string-ref str (- len 1))))
          (and (or (eqv? last #\i) (eqv? last #\I))
            (let ((prefix (substring str 0 (- len 1))))
              (cond
                ((string=? prefix "+") (make-rectangular 0 1))
                ((string=? prefix "-") (make-rectangular 0 -1))
                ((and (> (string-length prefix) 0)
                      (or (eqv? (string-ref prefix 0) #\+)
                          (eqv? (string-ref prefix 0) #\-))
                      (string->number prefix))
                 => (lambda (n) (make-rectangular 0 n)))
                (else #f))))))))

  (define (polar-number str)
    ;; r@theta
    (let scan ((i 0))
      (if (= i (string-length str))
        #f
        (if (eqv? (string-ref str i) #\@)
          (let ((r (string->number (substring str 0 i)))
                (theta (string->number (substring str (+ i 1) (string-length str)))))
            (and r theta (real? r) (real? theta) (make-polar r theta)))
          (scan (+ i 1))))))

  (define (read-number ch)
    (let ((str (read-token ch)))
      (cond
        ((polar-number str) => (lambda (n) n))
        ((pure-imaginary-number str) => (lambda (n) n))
        ((string->number str) => (lambda (n) n))
        ((valid-identifier? str)
         (string->symbol (if (fold-case?) (fold-string str) str)))
        (else (error 'read-error "invalid token" str)))))

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

  (define (read-hex-escape)
    (let loop ((n 0) (any #f))
      (let ((ch (peek)))
        (if (and (not (eof-object? ch)) (char-hex-digit? ch))
          (begin
            (next)
            (loop (+ (* n 16) (hex-digit-value ch)) #t))
          (if any
            (if (eqv? (peek) #\;)
              (begin
                (next)
                (integer->char n))
              (error 'read-error "hex escape missing semicolon"))
            (error 'read-error "invalid hex escape"))))))

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
             ((assoc (if (fold-case?) (fold-string token) token) char-names) => cdr)
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
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
             (read-label-number ch))
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

  (define (skip-line-comment)
    ;; skip until (and including) the line ending
    (let ((ch (next)))
      (cond
        ((eof-object? ch)   #f)
        ((or (eq? ch #\newline) (eq? ch #\return)) #f)
        (else               (skip-line-comment)))))

  (define (skip-block-comment)
    ;; skip a properly nested #| ... |# comment (the opening #| is consumed)
    (let loop ((depth 1))
      (let ((ch (next)))
        (cond
          ((eof-object? ch)
           (error 'read-error "unterminated block comment"))
          ((eqv? ch #\#)
           (if (eqv? (peek) #\|)
             (begin (next) (loop (+ depth 1)))
             (loop depth)))
          ((eqv? ch #\|)
           (if (eqv? (peek) #\#)
             (begin
               (next)
               (if (= depth 1) #f (loop (- depth 1))))
             (loop depth)))
          (else (loop depth))))))

  (define (next-non-whitespace)
    (let loop ((ch (next)))
      (case ch
        ((#\;)
         (skip-line-comment)
         (next-non-whitespace))
        ((#\#)
         (case (peek)
           ((#\!)
            (next)
            (let ((tok (take-until #\! delimiter?)))
              (cond
                ((string=? tok "!fold-case") (set-fold-case! #t))
                ((string=? tok "!no-fold-case") (set-fold-case! #f))
                (else (error 'read-error "unknown directive" tok)))
              (next-non-whitespace)))
           ((#\|)
            (cond
              ((read-hash-procedure #\|) ch)
              (else (next)
                    (skip-block-comment)
                    (next-non-whitespace))))
           ((#\;)
            (next)
            (let ((dch (next-non-whitespace)))
              (if (eof-object? dch)
                (error 'read-error "datum comment has no datum")
                (read-expr dch)))
            (next-non-whitespace))
           (else ch)))
        ;; TOOD: #\xc: #\ff Form Feed, ASCII 12
        ((#\space #\return #\xc #\newline #\tab)
         (next-non-whitespace))
        (else ch))))

  (let ((ch (next-non-whitespace)))
    (if (eof-object? ch)
      ch
      (read-expr ch))))

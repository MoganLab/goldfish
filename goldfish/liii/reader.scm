
;;; let*-values : bind to the values of a producer, sequentially.
;;; Defined here (cf. Guile's ice-9/read.scm) so the reader is self-contained
;;; and uses the expander's define-syntax: the reader loads through the
;;; expander right after the artifact (kernel only), so it uses a lambda
;;; transformer rather than syntax-rules (syntax-rules comes with the lib
;;; layer, which loads after the reader -- the reader is needed to parse the
;;; lib-layer files' `(X ...)' ellipsis syntax, which s7's tiny reader
;;; collapses).
(define-syntax let*-values
  (lambda (stx)
    (let ((form (syntax->datum stx)))
      (let ((clauses (cadr form))
            (body (cddr form)))
        (let loop ((cls clauses))
          (datum->syntax
            stx
            (if (null? cls)
              `(let () ,@body)
              `(call-with-values
                 (lambda () ,(cadr (car cls)))
                 (lambda ,(car (car cls))
                   ,(loop (cdr cls)))))))))))

;; R7RS 7.1.1: a <delimiter> is whitespace, ( ) " or ;.  In particular a
;; vertical bar is NOT a delimiter, so `foo|bar|` is one (invalid) token.
;; Delegates to C++ (g-delimiter?): the delimiter set is the single source
;; of truth shared with g-read-token, so token boundaries never diverge.
(define (delimiter? ch)
  (g-delimiter? ch))

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
      (list->string (reverse out))
      (let ((n (char->integer (string-ref str i))))
        (loop (+ i 1)
              (cons (if (<= (char->integer #\A) n (char->integer #\Z))
                      (integer->char (+ n 32))
                      (integer->char n))
                    out))))))

;; per-datum state; read is not reentrant, so module-level is safe
(define labels '())
(define pending '())

(define (next port) (read-char port))
(define (peek port) (peek-char port))

(define (fold-case? port)
  (if (null? fold-case-ports)
    #f
    (let ((e (assv port fold-case-ports)))
      (and e (cdr e)))))

(define (set-fold-case! port v)
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

;;; Precomputed ASCII character-class tables for identifier parsing
;;; (R7RS 7.1.1 <identifier>).  Chars >= 128 are valid in identifiers
;;; (S7 extension), so the tables only cover 0-127 and the callers test
;;; n >= 128 first.  Building the tables once at load time replaces a
;;; per-character memv over a literal list -- a major reader hot path:
;;; valid-identifier? classifies every character of every cached .gfo
;;; record at startup (~500K calls).

(define *identifier-initial-table* (make-vector 128 #f))
(define *identifier-subsequent-table* (make-vector 128 #f))
(define (id-class-init! tbl chars)
  (for-each (lambda (ch)
              (vector-set! tbl (char->integer ch) #t))
            (string->list chars)))
(id-class-init! *identifier-initial-table*
                "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!$%&*/:<=>?@^_~")
(id-class-init! *identifier-subsequent-table*
                "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!$%&*/:<=>?@^_~+-.@")

(define (char-digit? ch)
  (and (char? ch) (char<=? #\0 ch #\9)))

(define (identifier-initial? ch)
  (let ((n (char->integer ch)))
    (or (>= n 128) (vector-ref *identifier-initial-table* n))))

(define (identifier-subsequent? ch)
  (let ((n (char->integer ch)))
    (or (>= n 128) (vector-ref *identifier-subsequent-table* n))))

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

(define (pure-imaginary-number str radix)
  ;; +i -i +2i -2i 2i 1.5i ... : real part omitted (s7 writes a bare
  ;; imaginary as "2i" with no leading sign; R7RS uses "+2i" / "-2i")
  (let ((len (string-length str)))
    (and (> len 0)
      (let ((last (string-ref str (- len 1))))
        (and (or (eqv? last #\i) (eqv? last #\I))
          (let ((prefix (substring str 0 (- len 1))))
            (cond
              ((string=? prefix "+") (make-rectangular 0 1))
              ((string=? prefix "-") (make-rectangular 0 -1))
              ((string=? prefix "") #f)
              (else
               (let ((n (string->number prefix radix)))
                 (and n (real? n) (make-rectangular 0 n)))))))))))

(define (polar-number str radix)
  ;; r@theta
  (let ((at (char-position #\@ str)))
    (and at
      (let ((r (string->number (substring str 0 at) radix))
            (theta (string->number (substring str (+ at 1) (string-length str)) radix)))
        (and r theta (real? r) (real? theta) (make-polar r theta))))))

;; parse a number body (the part after any #b/#o/#d/#x/#e/#i prefixes)
;; in the given radix; returns the number or #f.
;; S7's string->number cannot parse a bare imaginary (+2i, -1.5i) nor a polar
;; number (1@2 is misread as 100.0), so those forms are handled here.
(define (parse-number-body body radix)
  (if (char-position #\@ body)
    (polar-number body radix)
    (let ((len (string-length body)))
      (if (and (> len 0) (memv (string-ref body (- len 1)) '(#\i #\I)))
        (or (pure-imaginary-number body radix)
            (string->number body radix))
        (string->number body radix)))))

(define (parse-number-prefix str)
  ;; Parse the leading radix/exactness prefixes of a "#..." string.
  ;; A prefix unit is "#" followed by exactly one prefix letter (#x #e ...);
  ;; consecutive units are #x#e10 / #e#x10.  A prefix letter only starts a
  ;; unit if it directly follows a "#"; a letter inside the body (e.g. the b
  ;; of #xbf) is not a prefix.  The body starts right after the last unit.
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
            ((memv ch '(#\b #\B #\o #\O #\d #\D #\x #\X #\e #\E #\i #\I))
             (if (eqv? (string-ref str (- i 1)) #\#)
               (let* ((next (and (< (+ i 1) len) (string-ref str (+ i 1))))
                      (r (case ch
                           ((#\b #\B) (if (= radix 0) 2 #f))
                           ((#\o #\O) (if (= radix 0) 8 #f))
                           ((#\d #\D) (if (= radix 0) 10 #f))
                           ((#\x #\X) (if (= radix 0) 16 #f))
                           (else radix)))
                      (e (case ch
                           ((#\e #\E) (if (= exact 0) 1 #f))
                           ((#\i #\I) (if (= exact 0) 2 #f))
                           (else exact))))
                 (if (or (not r) (not e))
                   #f
                   (if (eqv? next #\#)
                     (loop (+ i 1) r e)
                      (list (+ i 1) (if (= r 0) 10 r) e))))
                (list i (if (= radix 0) 10 radix) exact)))
            (else
             (list i (if (= radix 0) 10 radix) exact))))))))
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
  (or (char<=? #\a ch #\z)
      (char<=? #\A ch #\Z)))

(define (hex-digit-value ch)
  (let ((n (char->integer ch)))
    (cond
      ((<= (char->integer #\0) n (char->integer #\9))
       (- n (char->integer #\0)))
      ((<= (char->integer #\a) n (char->integer #\f))
       (+ (- n (char->integer #\a)) 10))
      (else
       (+ (- n (char->integer #\A)) 10)))))

;; ---------------------------------------------------------------------------

(define (read-token port ch)
  ;; Delegate the character pump to C++ (g-read-token): it reads the same
  ;; delimiter set as `delimiter?' below, so tokens are identical; only the
  ;; byte-by-byte loop moves to native code.  Interpretation (number vs
  ;; symbol, case folding) stays here.
  (g-read-token port ch))

(define (read-symbol port ch)
  (let ((str (read-token port ch)))
    (if (valid-identifier? str)
      (string->symbol (if (fold-case? port) (fold-string str) str))
      (error 'read-error "invalid token" str))))

(define (read-number port ch)
  (let ((str (read-token port ch)))
    (let ((n (parse-number-body str 10)))
      (if n
        n
        (if (valid-identifier? str)
          (string->symbol (if (fold-case? port) (fold-string str) str))
          (error 'read-error "invalid token" str))))))

(define (read-boolean port ch)
  (let ((tok (read-token port ch)))
    (cond
      ((string=? tok "t") #t)
      ((string=? tok "f") #f)
      ((string=? tok "true") #t)
      ((string=? tok "false") #f)
      (else (error 'read-error "invalid boolean" tok)))))

(define (read-prefixed-number port ch)
  (let* ((str (string-append "#" (read-token port ch)))
         (p (parse-number-prefix str)))
    (if (not p)
      (error 'read-error "invalid number" str)
      (let* ((body (substring str (car p) (string-length str)))
             (n (parse-number-body body (cadr p)))
             (exactness (caddr p)))
        (if (not n)
          (error 'read-error "invalid number" str)
          ;; S7 cannot represent an exact complex, so for #e/#i on a non-real
          ;; number the exactness prefix is best-effort (the number is returned
          ;; unchanged instead of raising a foreign inexact->exact error).
          (if (real? n)
            (case exactness
              ((1) (exact n))
              ((2) (inexact n))
              (else n))
            n))))))

(define (read-hex-char port)
  (let loop ((n 0))
    (let ((ch (peek port)))
      (if (and (not (eof-object? ch)) (char-hex-digit? ch))
        (begin
          (next port)
          (loop (+ (* n 16) (hex-digit-value ch))))
        (integer->char n)))))

(define (read-quoted-string port . args)
  ;; Delegate to C++ (g-read-string): it implements the same semantics --
  ;; line-ending continuation, the R7RS + S7 escape set, \xHH; as UTF-8 --
  ;; so the byte pump and escapes run natively.  The opening rdelim (the
  ;; char after the quote / |) is already consumed.
  (let ((rdelim (if (null? args) #\" (car args))))
    (g-read-string port rdelim)))

;; decode a UTF-8 codepoint from the port; the leading byte b1 has already
;; been read.  S7 ports are byte-oriented (read-char returns one byte), so a
;; non-ASCII character literal must be decoded explicitly.
(define (read-utf8-char port b1)
  (define (byte)
    (let ((c (next port)))
      (if (eof-object? c)
        (error 'read-error "invalid UTF-8 sequence in character")
        (let ((b (char->integer c)))
          (if (<= #x80 b #xbf)
            b
            (error 'read-error "invalid UTF-8 sequence in character"))))))
  (let ((v (cond
             ((<= b1 #xdf)
              (+ (* (- b1 #xc0) 64) (- (byte) #x80)))
             ((<= b1 #xef)
              (+ (* (- b1 #xe0) 4096) (* (- (byte) #x80) 64) (- (byte) #x80)))
             (else
              (+ (* (- b1 #xf0) 262144) (* (- (byte) #x80) 4096)
                 (* (- (byte) #x80) 64) (- (byte) #x80))))))
    (integer->char v)))

(define (read-character port)
  (let ((ch (next port)))
    (cond
      ((eof-object? ch)
       (error 'read-error "unexpected end of input in character"))
      ((memv ch '(#\x #\X))
       (if (and (not (eof-object? (peek port))) (char-hex-digit? (peek port)))
         (read-hex-char port)
         ch))
      ((char-letter? ch)
       (let* ((token (read-token port ch))
              (key (if (fold-case? port) (fold-string token) token))
              (entry (assoc key char-names)))
         (cond
           (entry (cdr entry))
           ((= (string-length token) 1) ch)
           (else (error 'read-error "invalid character" token)))))
      ((>= (char->integer ch) #x80)
       (read-utf8-char port (char->integer ch)))
      (else ch))))

(define (read-label port n)
  (let ((ch (next port)))
    (cond
      ((eqv? ch #\=)
       (let* ((placeholder (vector 'reader-placeholder n)))
         (set! pending (cons (cons n placeholder) pending))
         (let ((dch (next-non-whitespace port)))
           (if (eof-object? dch)
             (error 'read-error "label with no datum")
             (let ((datum (read-expr port dch)))
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

(define (read-label-number port first-digit)
  (let loop ((n (digit-value first-digit)))
    (let ((ch (peek port)))
      (if (char-digit? ch)
        (begin
          (next port)
          (loop (+ (* n 10) (digit-value ch))))
        (read-label port n)))))

(define (read-bytevector port)
  (let loop ((ch (next-non-whitespace port)) (acc '()))
    (if (eof-object? ch)
      (error 'read-error "unexpected end of input in bytevector")
      (if (eqv? ch #\))
        (apply bytevector (reverse acc))
        (let ((b (read-expr port ch)))
          (if (and (exact-integer? b) (<= 0 b 255))
            (loop (next-non-whitespace port) (cons b acc))
            (error 'read-error "bytevector element out of range" b)))))))

(define (read-subexpression port what)
  (let ((ch (next-non-whitespace port)))
    (if (eof-object? ch)
      (error 'read-error "unexpected end of input" what)
      (read-expr port ch))))

(define (read-vector port)
  (when (> *depth* *max-vector-depth*)
    (error 'read-error "maximum nesting depth exceeded"))
  (let loop ((ch (next-non-whitespace port)) (acc '()))
    (if (eof-object? ch)
      (error 'read-error "unexpected end of input in vector")
      (if (eqv? ch #\))
        (list->vector (reverse acc))
        (let ((elem (read-expr port ch)))
          (loop (next-non-whitespace port) (cons elem acc)))))))

(define (read-parenthesized port rdelim)
  (let loop ((ch (next-non-whitespace port)))
    (when (eof-object? ch)
      (error 'read-error "unexpected end of input" rdelim))
    (cond
      ((eqv? ch rdelim) '())
      ((or (eqv? ch #\)) (eqv? ch #\]))
       (error 'read-error (if (eqv? ch #\)) "unexpected close paren: \")" "unexpected close paren: \"]")))
      ((and (eqv? ch #\.) (or (eof-object? (peek port)) (delimiter? (peek port))))
       (let ((tail-ch (next-non-whitespace port)))
         (if (eof-object? tail-ch)
           (error 'read-error "unexpected end of input" rdelim)
           (let ((tail (read-expr port tail-ch)))
             (let ((close (next-non-whitespace port)))
               (if (eqv? close rdelim)
                 tail
                 (error 'read-error "expected closing delimiter" rdelim)))))))
      (else (cons (read-expr port ch)
                  (loop (next-non-whitespace port)))))))

;; internal S7 objects: #<eof>, #<unspecified>, #<undefined>; any other
;; #<...> is read as the symbol #<...> (used by e.g. the case* macro patterns)
(define (read-angle-token port)
  ;; chars up to (and including) the closing >
  (let loop ((acc '()))
    (let ((ch (next port)))
      (cond
        ((eof-object? ch) (error 'read-error "unterminated #< object"))
        ((eqv? ch #\>) (list->string (reverse acc)))
        (else (loop (cons ch acc)))))))

(define (read-internal-object port)
  (let ((tok (read-angle-token port)))
    (cond
      ((string=? tok "eof") (eof-object))
      ((string=? tok "unspecified") (if #f #f))
      ((string=? tok "undefined") (g-undefined))
      ;; any other #<name> is a named undefined matching S7 (the name keeps
      ;; its ">" so that object->string prints "#<name>"); used e.g. by the
      ;; case* macro patterns
      (else (g-undefined (string-append "<" tok ">"))))))

;; SRFI-267 raw strings: #"delimiter"body"delimiter" (delimiter may be empty).
;; The closing marker is " + delimiter + "; it needs (dlen + 2) chars of
;; lookahead, which is maintained by the fill/scan loop below.
(define (read-raw-delimiter port)
  ;; the opening " has been consumed; the delimiter runs to the next "
  (let loop ((acc '()))
    (let ((ch (next port)))
      (cond
        ((eof-object? ch) (error 'read-error "unterminated raw string delimiter"))
        ((eqv? ch #\") (list->string (reverse acc)))
        (else (loop (cons ch acc)))))))

(define (raw-closing? la close)
  ;; does la start with the closing marker close?
  (let loop ((la la) (cl close))
    (cond
      ((null? cl) #t)
      ((or (null? la) (not (eqv? (car la) (car cl)))) #f)
      (else (loop (cdr la) (cdr cl))))))

(define (read-raw-body port delim)
  (let* ((dlen (string-length delim))
         (need (+ dlen 2))
         (close (cons #\" (append (string->list delim) (list #\")))))
    (letrec ((fill (lambda (la n)
                     (if (= n need)
                       la
                       (let ((ch (peek port)))
                         (if (eof-object? ch)
                           la
                           (begin
                             (next port)
                             (fill (append la (list ch)) (+ n 1))))))))
             (scan (lambda (la buf)
                     (if (raw-closing? la close)
                       (list->string (reverse buf))
                       (if (< (length la) need)
                         (error 'read-error "unterminated raw string")
                         (scan (fill (cdr la) (- need 1))
                               (cons (car la) buf)))))))
      (scan (fill '() 0) '()))))

(define (read-sharp port)
  (let ((ch (next port)))
    (cond
      ((eof-object? ch)
       (error 'read-error "unexpected end of input after #"))
      (else
        (case ch
          ((#\\)
           (read-character port))
          ;; S7 caret notation: #^A is Ctrl-A (char code XOR 0x40)
          ((#\^)
           (let ((c (next port)))
             (if (eof-object? c)
               (error 'read-error "unexpected end of input after #^")
               (integer->char (logxor (char->integer c) #x40)))))
          ((#\<)
           (read-internal-object port))
          ((#\")
           (let ((delim (read-raw-delimiter port)))
             (read-raw-body port delim)))
          ((#\() (read-vector port))
          ((#\t #\T #\f #\F) (read-boolean port ch))
          ((#\b #\B #\o #\O #\d #\D #\x #\X #\e #\E #\i #\I)
           (read-prefixed-number port ch))
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (read-label-number port ch))
          ((#\u)
           (cond
             ((eqv? (peek port) #\8)
              (next port)
              (if (eqv? (peek port) #\()
                (begin (next port) (read-bytevector port))
                (error 'read-error "invalid #u8")))
             ;; S7 also accepts #u(...) as a shorthand for #u8(...)
             ((eqv? (peek port) #\()
              (next port)
              (read-bytevector port))
             (else
               (error 'read-error "invalid #u8"))))
          ;; Goldfish record serialization: #g(tag field ...) written by
          ;; write-roundtrip.  Rebuilds the vector-layout record so the
          ;; object round-trips with its record type intact.
          ((#\g)
           (if (eqv? (peek port) #\()
             (begin
               (next port)
               (read-goldfish-record port))
             (error 'read-error "invalid #g object")))
          ;; R7RS abbreviated forms of the syntax-object forms:
          ;;   #'X    -> (syntax X)
          ;;   #`X    -> (quasisyntax X)
          ;;   #,X    -> (unsyntax X)
          ;;   #,@X   -> (unsyntax-splicing X)
          ((#\')
           (list 'syntax (read-subexpression port "syntax expression")))
          ((#\`)
           (list 'quasisyntax (read-subexpression port "quasisyntax expression")))
          ((#\,)
           (cond
             ((eqv? (peek port) #\@)
              (next port)
              (list 'unsyntax-splicing (read-subexpression port "subexpression of #,@"))
             )
             (else
               (list 'unsyntax (read-subexpression port "unsyntax expression")))))
          (else
            (error 'read-error "Unknown # object" (string #\# ch))))))))

;; Nesting depth guard.  read-expr recurses once per nesting level (lists,
;; vectors, quote/backquote abbreviations, datum comments); the Scheme stack
;; segfaults at roughly 60k-70k nested levels on default C stacks, so refuse
;; to go deeper with a catchable read-error instead of crashing.
(define *max-depth* 40000)
;; The vector path (read-expr -> read-sharp -> read-vector) burns more C
;; stack per level than the list path (read-expr -> read-parenthesized), so
;; a deep #(...) overflows before the *max-depth* check trips (deep-vectors
;; 41000 segfaulted).  Cap it lower so vectors fail with read-error too;
;; 30000 is well under the measured overflow point (~38k) and above every
;; legitimate use.
(define *max-vector-depth* 30000)
(define *depth* 0)

(define (read-expr port ch)
  (set! *depth* (+ *depth* 1))
  (when (> *depth* *max-depth*)
    (set! *depth* (- *depth* 1))
    (error 'read-error "maximum nesting depth exceeded"))
  (let ((result
         (case ch
           ((#\[) (read-parenthesized port #\]))
           ((#\() (read-parenthesized port #\)))
           ((#\") (read-quoted-string port))
           ((#\|) (string->symbol (read-quoted-string port ch)))
           ((#\') (list 'quote (read-subexpression port "quoted expression")))
           ((#\`) (list 'quasiquote (read-subexpression port "quasiquoted expression")))
           ((#\,)
            (cond
              ((eq? #\@ (peek port))
               (next port)
               (list 'unquote-splicing (read-subexpression port "subexpression of ,@")))
              (else
                (list 'unquote (read-subexpression port "unquoted expression")))))
           ((#\#) (read-sharp port))
           ((#\)) (error 'read-error "unexpected close paren: \")"))
           ((#\]) (error 'read-error "unexpected close paren: \"]"))
           ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\- #\.)
            (read-number port ch))
           (else (read-symbol port ch)))))
    (set! *depth* (- *depth* 1))
    result))

(define (skip-line-comment port)
  ;; skip until (and including) the line ending
  (let ((ch (next port)))
    (cond
      ((eof-object? ch)   #f)
      ((or (eq? ch #\newline) (eq? ch #\return)) #f)
      (else               (skip-line-comment port)))))

(define (skip-block-comment port)
  ;; skip a properly nested #| ... |# comment (the opening #| is consumed)
  (let loop ((depth 1))
    (let ((ch (next port)))
      (cond
        ((eof-object? ch)
         (error 'read-error "unterminated block comment"))
        ((eqv? ch #\#)
         (if (eqv? (peek port) #\|)
           (begin (next port) (loop (+ depth 1)))
           (loop depth)))
        ((eqv? ch #\|)
         (if (eqv? (peek port) #\#)
           (begin
             (next port)
             (if (= depth 1) #f (loop (- depth 1))))
           (loop depth)))
        (else (loop depth))))))

(define (next-non-whitespace port)
  (let loop ((ch (next port)))
    (case ch
      ((#\;)
       (skip-line-comment port)
       (next-non-whitespace port))
      ((#\#)
       (case (peek port)
         ((#\!)
          (next port)
          (let ((tok (read-token port #\!)))
            (cond
              ((string=? tok "!fold-case") (set-fold-case! port #t))
              ((string=? tok "!no-fold-case") (set-fold-case! port #f))
              (else (error 'read-error "unknown directive" tok)))
            (next-non-whitespace port)))
         ((#\|)
          (next port)
          (skip-block-comment port)
          (next-non-whitespace port))
         ((#\;)
          (next port)
          (let ((dch (next-non-whitespace port)))
            (if (eof-object? dch)
              (error 'read-error "datum comment has no datum")
              (read-expr port dch)))
          (next-non-whitespace port))
         (else ch)))
      ((#\space #\return #\xc #\newline #\tab)
       (next-non-whitespace port))
      (else ch))))

(define (port-pos port)
  ;; byte offset of the port, or -1 when not positionable
  (catch #t
    (lambda () (port-position port))
    (lambda args -1)))

(define (read-error-with-pos port args)
  ;; re-raise a read-error carrying the port position, so that errors from a
  ;; file load can be located; args is the error's argument list
  (let ((pos (port-pos port)))
    (if (and (pair? args) (string? (car args)))
      (apply error 'read-error
             (cons (string-append (car args) " (at byte " (number->string pos) ")")
                   (cdr args)))
      (apply error 'read-error args))))

(define (read . args)
  (let ((port (if (pair? args) (car args) (current-input-port))))
    (set! labels '())
    (set! pending '())
    (set! *depth* 0)
    (let ((ch (next-non-whitespace port)))
      (if (eof-object? ch)
        (begin
          ;; a port at EOF keeps no directive state; drop it so that ports used
          ;; with #!fold-case do not accumulate in fold-case-ports
          (when (assv port fold-case-ports)
            (set! fold-case-ports (del-eqv port fold-case-ports)))
          ch)
        (catch 'read-error
          (lambda () (read-expr port ch))
          (lambda (tag . errs)
            (read-error-with-pos port (if (pair? errs) (car errs) '()))))))))

;; Replace S7's load: read the file through this Scheme reader and expand /
;; evaluate each form with the expander (expand-eval), so macros defined in
;; loaded files register in the shared base library.  Searches *load-path*
;; like S7's load did.

(define (auto-compile-enabled?)
  (let ((v (getenv "GOLDFISH_AUTO_COMPILE")))
    (not (and v (member v '("0" "no" "false" "off"))))))

(define (contains-macro-def? form)
  (and (pair? form)
       (let ((h (car form)))
         (cond ((memq h '(define-syntax define-macro)) #t)
               ((eq? h 'begin) (any-macro-def? (cdr form)))
               ((eq? h 'define-library) (any-macro-def? (cddr form)))
               (else #f)))))

(define (any-macro-def? forms)
  (cond ((null? forms) #f)
        ((contains-macro-def? (car forms)) #t)
        (else (any-macro-def? (cdr forms)))))

(define (collect-module-refs sexp)
  (let loop ((x sexp) (acc '()))
    (cond
      ((and (pair? x) (eq? (car x) 'module-ref))
       (let ((rest (cdr x)))
         (loop (cdr x)
               (if (and (pair? rest) (pair? (car rest)) (eq? (caar rest) 'quote))
                 (let ((lib (cadar rest)))
                   (if (member lib acc) acc (cons lib acc)))
                 acc))))
      ((pair? x)
       (loop (car x) (loop (cdr x) acc)))
      (else acc))))

;;; formals->names : formals -> (list symbol)
;;; Turn a lambda formals list into the list of parameter names, handling
;;; dotted formals (lambda (x . rest) ...).

(define (formals->names f)
  (cond ((null? f) '())
        ((symbol? f) (list f))
        ((pair? f) (cons (car f) (formals->names (cdr f))))
        (else (list f))))

(define (cacheable-expansion? sexp)
  (let* ((defined-names
          (let loop ((x sexp) (acc '()))
            (cond
              ((pair? x)
               (if (and (eq? (car x) 'define) (pair? (cdr x)) (symbol? (cadr x)))
                 (loop (cdr x) (cons (cadr x) acc))
                 (loop (car x) (loop (cdr x) acc))))
              (else acc))))
         (local-define-names
          ;; Names defined directly in a lambda body (lowered core bodies
          ;; are (begin (define ...) ...)) are in scope for the rest of the
          ;; body; collect them so they are not mistaken for unresolved
          ;; free symbols.
          (lambda (body)
            (let loop ((x body) (acc '()))
              (cond
                ((and (pair? x) (eq? (car x) 'begin)) (loop (cdr x) acc))
                ((and (pair? x) (eq? (car x) 'define)
                      (pair? (cdr x)) (symbol? (cadr x)))
                 (cons (cadr x) acc))
                (else acc)))))
         (known (lambda (s) (or (member s defined-names) (defined? s)))))
    (let check ((x sexp) (params '()))
      (cond
        ((symbol? x) (or (memq x params) (known x)))
        ((pair? x)
         (let ((h (car x)))
           (cond
             ((eq? h 'quote) #t)
             ((eq? h 'lambda)
              (let* ((f (cadr x))
                     (body (caddr x))
                     (ps (append (local-define-names body)
                                 (append (formals->names f) params))))
                (check body ps)))
             ((memq h '(let let* letrec letrec*))
              (if (and (pair? (cadr x)) (pair? (caadr x)))
                ;; let/let*/letrec/letrec*: binding names are in scope for
                ;; the body.  letrec/letrec* values see all the names too
                ;; (a binding can recursively reference itself or a sibling,
                ;; e.g. (letrec ((loop ...)) (loop ...))), so their value
                ;; expressions are checked in the extended scope; let/let*
                ;; values are checked in the outer scope.  (Named lets fall
                ;; through to the recursive case and are treated
                ;; conservatively.)
                (let* ((names (map car (cadr x)))
                       (vals-ok
                        (let loop ((binds (cadr x)))
                          (cond
                            ((null? binds) #t)
                            ((and (pair? (car binds)) (pair? (cdar binds)))
                             (and (check (cadar binds)
                                         (if (memq h '(letrec letrec*))
                                           (append names params)
                                           params))
                                  (loop (cdr binds))))
                            (else #f)))))
                  (and vals-ok (check (caddr x) (append names params))))
                (and (check (car x) params) (check (cdr x) params))))
             ((eq? h 'module-ref) #t)
             (else (and (check (car x) params) (check (cdr x) params))))))
        (else #t)))))

;; library-names-in : (list datum) -> (list lib-name)
;; The library names declared by (define-library (name) ...) forms in a
;; loaded file.  `load' routes files that declare libraries through the
;; library loader so they share its cache with `import'.

(define (library-names-in forms)
  (let loop ((fs forms) (acc '()))
    (if (null? fs)
      (reverse acc)
      (let ((f (car fs)))
        (if (and (pair? f)
                 (eq? (car f) 'define-library)
                 (pair? (cdr f)))
          (loop (cdr fs) (cons (cadr f) acc))
          (loop (cdr fs) acc))))))

(define (load file)
  (define dirs (if (list? *load-path*) *load-path* (list *load-path*)))
  (define (load-forms-sequentially forms)
    (for-each (lambda (d)
                ;; Use the expander whenever it is up: the s7-eval fallback
                ;; only applies to the seed phase (before this reader defines
                ;; expand-eval).
                (if (defined? 'expand-eval)
                  (expand-eval d)
                  (eval d (rootlet))))
              forms))
  (let loop ((cands (cons file (map (lambda (d) (string-append d "/" file)) dirs))))
    (cond
      ((null? cands)
       (error 'io-error (string-append "cannot load: " file)))
      ((file-exists? (car cands))
       (let ((path (car cands)))
         (let ((forms (call-with-input-file path
                        (lambda (port)
                          (catch 'read-error
                            (lambda () (read-forms port))
                            (lambda (tag . errs)
                              (close-input-port port)
                              (if (and (pair? errs) (pair? (car errs)) (string? (caar errs)))
                                (error 'read-error
                                       (string-append (caar errs) " in " path))
                                (apply error 'read-error errs))))))))
            (if (not (null? (library-names-in forms)))
              ;; A library file: load it through the library machinery so
              ;; `load' and `import' share the SAME library cache (one
              ;; expansion, one cached artifact) instead of compiling the
              ;; file twice with different engines.  The defs evaluate into
              ;; the rootlet, mirroring the old per-file compile.
              (for-each load-library! (library-names-in forms))
              (if (and (auto-compile-enabled?)
                       (not (any-macro-def? forms)))
                ;; Compile the file once and execute the compiled artifact
                ;; (the compile-cache hot and cold paths agree; Guile-style:
                ;; eval-when (expand) side effects run once, at compile time).
                ;; A non-cacheable artifact (unresolved free symbols) or an
                ;; artifact that fails to eval falls back to per-form loading.
                (let ((sexp (compile-file-cached path)))
                  (if (cacheable-expansion? sexp)
                    (catch #t
                      (lambda ()
                        (for-each (lambda (lib)
                                    (if (not (runtime-registered? lib))
                                      (load-library! lib)))
                                  (collect-module-refs sexp))
                        ;; Evaluate the compiled artifact in
                        ;; the-expander-library, not the rootlet: the
                        ;; lowered defs reference library bindings by
                        ;; gensym (e.g. load-library!:40), which only
                        ;; resolve in the-expander-library.
                        (eval sexp the-expander-library))
                      (lambda (type info)
                        (load-forms-sequentially forms)))
                    (load-forms-sequentially forms)))
                (load-forms-sequentially forms))))))
      (else (loop (cdr cands))))))
;; Rebind read-forms to the R7RS reader now that `read' is ours: the seed
;; (boot.scm) definition captured the bootstrap reader, which is minimal and
;; is only meant for reading the three bootstrap files.

(define (read-forms port)
  (let loop ((d (read port)) (acc '()))
    (if (eof-object? d)
      (reverse acc)
      (loop (read port) (cons d acc)))))

;;; read-goldfish-record : port -> record
;;; Read #g(tag field ...) -- the write-roundtrip serialization of a
;;; vector-layout record -- and rebuild the record with its type intact.
;;; Field values are ordinary data (read recursively); exp-library bindings
;;; are an alist (name . binding) pairs, read as proper lists and converted
;;; back to dotted pairs.

(define (read-goldfish-record port)
  ;; Read one field: skip whitespace/comments and read via read-expr, NOT
  ;; via read -- read resets the label tables (labels/pending), which would
  ;; orphan the #n= placeholders of an enclosing shared record.
  (define (read-field)
    (let ((ch (next-non-whitespace port)))
      (if (eof-object? ch)
        (error 'read-error "unexpected end of input in #g record")
        (read-expr port ch))))
  (let ((tag (read-field)))
    (case tag
      ((syntax)
       (let ((form (read-field))
             (ctx (read-field))
             (lib (read-field)))
         (if (eqv? (peek port) #\))
           (begin (next port) (make-syntax form ctx lib))
           (error 'read-error "malformed #g(syntax ...)"))))
      ((exp-library)
       (let ((name (read-field))
             (bindings (read-field)))
         (if (eqv? (peek port) #\))
           (begin
             (next port)
             (let ((lib (make-exp-library name)))
               (for-each (lambda (e)
                           (if (pair? e)
                             (exp-library-define! lib (car e) (cdr e))))
                         bindings)
               lib))
           (error 'read-error "malformed #g(exp-library ...)"))))
      ((binding)
       (let ((kind (read-field))
             (value (read-field)))
         (if (eqv? (peek port) #\))
           (begin (next port) (make-binding kind value))
           (error 'read-error "malformed #g(binding ...)"))))
      ((toplevel-ref)
       (let ((gensym (read-field))
             (home (read-field))
             (original (read-field))
             (exported? (read-field)))
         (if (eqv? (peek port) #\))
           (begin
             (next port)
             (make-toplevel-ref gensym home original exported?))
           (error 'read-error "malformed #g(toplevel-ref ...)"))))
      (else
       (error 'read-error "unknown #g record" tag)))))

;;; expand-eval : datum -> value
;;; The one-time eval entry: expand a top-level form with the Sets-of-Scopes
;;; expander (registering macros in the shared base library) and evaluate the
;;; lowered core in the-expander-library.  This is the replacement for s7's
;;; plain `eval' once the eval switch is flipped.  The free identifiers here
;;; (wrap-expression, expand-library-body, ...) resolve dynamically from the
;;; rootlet, so this definition predates the expander load.
;;;
;;; A plain recursive helper (not a named let) evaluates the defs: s7's
;;; named-let (tail) context misbehaves with `eval' on lower's output.

(define (eval-defs defs env)
  (if (null? defs)
    '(if #f #f)
    (let ((r (eval (lower (car defs)) env)))
      (if (null? (cdr defs))
        r
        (eval-defs (cdr defs) env)))))

;; optimize-expansion-defs : (list syntax) context -> (list sexp)
;; Apply the active compiler pipeline to expanded defs (the per-form /
;; REPL path, mirroring the cached paths).  The defs are un-lowered syntax
;; objects, so the pipeline runs via syntax->ir, which keeps the
;; binding-kind information (primitive references).  compile-defs-on-load
;; lives in the (goldfish)
;; library, not in this early seed's rootlet, so it is fetched via
;; module-ref; unavailable or failing, defs pass through unchanged
;; (optimization is optional).

(define (optimize-expansion-defs defs ctx)
  (let ((f (catch
             #t
             (lambda () (module-ref the-expander-library 'compile-defs-on-load))
             (lambda (type info) #f))))
    (if (procedure? f) (f defs ctx) (map lower defs))))

(define *eval-ctx* #f)

(define (expand-eval expr)
  ;; Top-level program forms expand against the session PROGRAM library
  ;; (R7RS 5.1: the program's environment starts empty and accumulates its
  ;; imports).  The base library is NOT ambient here: an identifier that
  ;; resolves nowhere is an error (expand.scm).  --mode imports and any
  ;; user (import ...) forms land in this library.
  (let* ((lib (program-library))
         (stx (stx-set-library (wrap-expression expr) lib))
         (ctx (or *eval-ctx* (initial-context)))
         (form (syntax-form stx))
         (head (and (pair? form) (car form))))
    (if (and (identifier? head)
             (let*-values (((name binding) (resolve-identifier head ctx)))
               (and binding (module-form-binding? binding))))
      (let*-values (((name binding) (resolve-identifier head ctx))
                    ((defs ctx1) ((binding-value binding) stx ctx)))
        (set! *eval-ctx* ctx1)
        (eval-defs (optimize-expansion-defs defs ctx1) the-expander-library))
      (let*-values (((defs ctx1)
                     (expand-library-body (list stx) lib ctx)))
        (set! *eval-ctx* ctx1)
        (eval-defs (optimize-expansion-defs defs ctx1) the-expander-library)))))

;;; ------------------------------------------------------------------------
;;; write-roundtrip : datum port -> void
;;; A writer whose output the R7RS reader here can read back to an equal
;;; value (read/write duality).  s7's write targets s7's own reader, so
;;; several types do not round-trip through read-forms:
;;;   - a symbol with a quote (hello') is written bare and unreadable;
;;;   - a symbol with whitespace/delimiters is written as (symbol "a b"),
;;;     which reads back as a list, not a symbol;
;;;   - records print as #(#(record-type <name> (fields...)) ...) whose
;;;     record type identity is lost on read.
;;; Symbols that are not valid R7RS identifiers are written in |...|
;;; vertical-bar notation; records are written as #g(tag fields...) and
;;; rebuilt by read-sharp's #g dispatch.  Vectors, bytevectors, strings,
;;; characters, numbers (including complex/imaginary/inf/nan) and booleans
;;; delegate to write (the reader accepts their output).

;;; write-roundtrip-symbol : symbol port -> void

(define (write-roundtrip-symbol x p)
  (let ((s (symbol->string x)))
    (if (valid-identifier? s)
      (write x p)
      (begin
        (display #\| p)
        (string-for-each (lambda (ch)
                           (cond
                             ((or (eqv? ch #\|) (eqv? ch #\\))
                              (display #\\ p))
                             (else #f))
                           (write-char ch p))
                         s)
        (display #\| p)))))

;;; write-roundtrip : datum port -> void
;;; Graph-aware writer: the two-pass Racket print-graph scheme.  Pass 1
;;; walks the datum (with an eq? table) counting how many times each
;;; container -- pair, vector, or vector-layout record -- is referenced;
;;; containers referenced more than once (or self-referentially, e.g. an
;;; exp-library whose toplevel-ref bindings point back at it) get a #n= on
;;; first output and #n# afterwards, which read-label/read-sharp parse back
;;; to the shared object.  This is essential for exp-library records: their
;;; bindings alist's toplevel-ref homes refer back to the library itself,
;;; so a naive recursive writer loops forever.

;;; has-record? : datum -> bool
;;; Quick scan whether a datum contains any vector-layout record.  Cached
;;; expansions (lower core) are plain data -- symbols, pairs, vectors,
;;; strings, numbers -- with no records, so write-roundtrip can take the
;;; fast single-pass path (no graph bookkeeping) for them; only macro
;;; compile products (which embed exp-library/binding records) need the
;;; graph-aware two-pass writer.

(define (has-record? x)
  (let walk ((v x) (seen '()))
    (cond
      ((record-instance? v)
       (if (not (assq v seen))
         (let ((seen* (cons (cons v #t) seen)))
           ;; A record is itself a record; also check its fields for
           ;; nested records.
           (let loop ((i 1))
             (if (< i (vector-length v))
               (or (walk (vector-ref v i) seen*)
                   (loop (+ i 1)))
               #t)))
         #t))
      ((pair? v)
       (and (not (assq v seen))
            (let ((seen* (cons (cons v #t) seen)))
              (or (walk (car v) seen*) (walk (cdr v) seen*)))))
      ((and (vector? v) (not (bytevector? v)))
       (if (not (assq v seen))
         (let ((seen* (cons (cons v #t) seen)))
           (let loop ((i 0))
             (if (< i (vector-length v))
               (or (walk (vector-ref v i) seen*) (loop (+ i 1)))
               #f)))
         #f))
      (else #f))))

;;; write-roundtrip : datum port -> void
;;; A writer whose output the R7RS reader (this file) reads back to an equal
;;; value, including records and shared/cyclic structure.  Plain data (no
;;; records) is written single-pass; data containing records -- syntax
;;; objects, exp-libraries, bindings, toplevel-refs -- uses the two-pass
;;; graph writer below, which emits #n=/#n# labels (read-label/read-sharp
;;; parse them back to shared objects).  The graph pass is required for
;;; exp-library: its bindings' toplevel-ref homes refer back to the library
;;; itself, so a naive recursive writer loops forever.

;;; exp-library-record? : any -> boolean
;;; An <exp-library> record's bindings table holds every library binding
;;; (transformers, live objects).  Serializing it is useless (bindings are
;;; replayed from source by install-cache-load!) and can blow up the
;;; graph walk, so exp-library records are written by name only (their
;;; other fields are recovered on load).  Name is the record's second
;;; field, always a list; the record has exactly three fields.
(define (exp-library-record? v)
  (and (= (vector-length v) 3)
       (pair? (vector-ref v 1))))

(define (write-roundtrip x p)
  (if (not (has-record? x))
    (let rec ((v x))
      (cond
        ((symbol? v) (write-roundtrip-symbol v p))
        ((pair? v)
         (display #\( p)
         (let loop ((y v))
           (cond
             ((pair? y)
              (rec (car y))
              (if (pair? (cdr y))
                (begin (display #\space p) (loop (cdr y)))
                (if (null? (cdr y))
                  #f
                  (begin
                    (display " . " p)
                    (rec (cdr y))))))
             ((null? y) #f)
             (else
              (display " . " p)
              (rec y))))
         (display #\) p))
        ((null? v) (display "()" p))
        ((vector? v)
         (display "#(" p)
         (let loop ((i 0))
           (if (< i (vector-length v))
             (begin
               (if (> i 0) (display #\space p))
               (rec (vector-ref v i))
               (loop (+ i 1)))))
         (display ")" p))
        ((procedure? v)
         (error "write-roundtrip: cannot serialize a procedure" v))
        (else (write v p))))
    ;; Graph-aware pass (data contains records): count references, then
    ;; output with #n=/#n# labels for shared/cyclic containers.
    (let ((counts '()))
      (define (count-ref v)
        (let ((e (assq v counts)))
          (if e
            (set-cdr! e (+ (cdr e) 1))
            (set! counts (cons (cons v 1) counts)))))
      (define (count-of v)
        (let ((e (assq v counts)))
          (if e (cdr e) 0)))
      (let walk ((v x) (seen '()))
        (cond
          ((pair? v)
           (count-ref v)
           (if (not (assq v seen))
             (let ((seen* (cons (cons v #t) seen)))
               (walk (car v) seen*)
               (walk (cdr v) seen*))))
          ((record-instance? v)
           (count-ref v)
           (if (not (assq v seen))
             (let ((seen* (cons (cons v #t) seen)))
               (let loop ((i 1))
                 (if (< i (vector-length v))
                   (begin
                     (if (not (and (= i 2) (exp-library-record? v)))
                       (walk (vector-ref v i) seen*))
                     (loop (+ i 1))))))))
          ((and (vector? v) (not (bytevector? v)) (not (record-instance? v)))
           (count-ref v)
           (if (not (assq v seen))
             (let ((seen* (cons (cons v #t) seen)))
               (let loop ((i 0))
                 (if (< i (vector-length v))
                   (begin (walk (vector-ref v i) seen*) (loop (+ i 1))))))))
          (else #f)))
      (let ((labels '())
            (next-label 0))
        (define (shared? v) (> (count-of v) 1))
        (define (label-of v)
          (let ((e (assq v labels)))
            (if e
              (cdr e)
              (let ((n next-label))
                (set! next-label (+ n 1))
                (set! labels (cons (cons v n) labels))
                n))))
        (define (has-label? v) (not (not (assq v labels))))
        (define (write-mark v)
          (when (shared? v)
            (display #\# p)
            (display (label-of v) p)
            (display #\= p)))
        (define (write-ref v)
          (when (shared? v)
            (display #\# p)
            (display (label-of v) p)
            (display #\# p)))
        (define (wrt v)
          (cond
            ((symbol? v)
             (write-roundtrip-symbol v p))
            ((pair? v)
             (if (shared? v)
               (if (has-label? v)
                 (write-ref v)
                 (begin (write-mark v) (wrt-pair v)))
               (wrt-pair v)))
            ((null? v)
             (display "()" p))
            ((record-instance? v)
             (if (shared? v)
               (if (has-label? v)
                 (write-ref v)
                 (begin (write-mark v) (wrt-record v)))
               (wrt-record v)))
            ((vector? v)
             (if (shared? v)
               (if (has-label? v)
                 (write-ref v)
                 (begin (write-mark v) (wrt-vector v)))
               (wrt-vector v)))
            ((procedure? v)
             (error "write-roundtrip: cannot serialize a procedure" v))
            (else (write v p))))
        (define (wrt-pair v)
          (display #\( p)
          (let loop ((y v))
            (cond
              ((pair? y)
               (wrt (car y))
               (if (pair? (cdr y))
                 (begin (display #\space p) (loop (cdr y)))
                 (if (null? (cdr y))
                   #f
                   (begin
                     (display " . " p)
                     (wrt (cdr y))))))
              ((null? y) #f)
              (else
               (display " . " p)
               (wrt y))))
          (display #\) p))
        (define (wrt-vector v)
          (display "#(" p)
          (let loop ((i 0))
            (if (< i (vector-length v))
              (begin
                (if (> i 0) (display #\space p))
                (wrt (vector-ref v i))
                (loop (+ i 1)))))
          (display ")" p))
        (define (wrt-record v)
          (let* ((rtd (vector-ref v 0))
                 (name (record-type-name rtd))
                 (name-str (and (symbol? name) (symbol->string name))))
            (cond
              ((string=? name-str "<syntax>")
               (display "#g(syntax " p)
               (wrt (vector-ref v 1))  ; form
               (display " " p)
               (wrt (vector-ref v 2))  ; context
               (display " " p)
               (wrt (vector-ref v 3))  ; library
               (display ")" p))
              ((string=? name-str "<exp-library>")
               (display "#g(exp-library " p)
               (wrt (vector-ref v 1))  ; name
               (display " " p)
               (wrt '())  ; bindings: replay from source on load
               (display ")" p))
              ((string=? name-str "<binding>")
               (display "#g(binding " p)
               (wrt (vector-ref v 1))  ; kind
               (display " " p)
               (wrt (vector-ref v 2))  ; value
               (display ")" p))
              ((string=? name-str "<toplevel-ref>")
               (display "#g(toplevel-ref " p)
               (wrt (vector-ref v 1))  ; gensym
               (display " " p)
               (wrt (vector-ref v 2))  ; home
               (display " " p)
               (wrt (vector-ref v 3))  ; original
               (display " " p)
               (wrt (vector-ref v 4))  ; exported?
               (display ")" p))
              (else
               (display "#<" p)
               (display name-str p)
               (display ">" p)))))
        (wrt x)))))

(import (liii string-cursor))

;; R7RS 7.1.1: a <delimiter> is whitespace, ( ) " or ;.  In particular a
;; vertical bar is NOT a delimiter, so `foo|bar|` is one (invalid) token.
(define (delimiter? ch)
  ;; hot path: the delimiter set is small and ASCII-only
  (let ((n (char->integer ch)))
    (and (< n 128)
      (memv ch '(#\( #\) #\[ #\] #\; #\" #\space #\return #\xc #\newline #\tab)))))

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

(define (char-digit? ch)
  (and (char? ch) (char<=? #\0 ch #\9)))

(define (identifier-initial? ch)
  (or (char-letter? ch)
      (memv ch '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\@ #\^ #\_ #\~))
      ;; S7 extension: non-ASCII characters are allowed in identifiers
      (>= (char->integer ch) 128)))

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

(define (pure-imaginary-number str radix)
  ;; +i -i +2i -2i +1.5i ... : real part omitted
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
                        (eqv? (string-ref prefix 0) #\-)))
               (let ((n (string->number prefix radix)))
                 (and n (make-rectangular 0 n))))
              (else #f))))))))

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

;; ---------------------------------------------------------------------------

;; reusable token buffer: take-until is never called recursively, so one
;; module-level buffer is shared (results are copies via substring)
(define token-buf (make-string 16))
(define token-cap 16)

(define (take-until port first pred)
  (let ((buf token-buf))
    (string-set! buf 0 first)
    (let lp ((len 1))
      (let ((ch (peek port)))
        (if (or (eof-object? ch) (pred ch))
          (substring buf 0 len)
          (begin
            (next port)
            (when (= len token-cap)
              (set! buf (string-append buf (make-string token-cap)))
              (set! token-buf buf)
              (set! token-cap (* 2 token-cap)))
            (string-set! buf len ch)
            (lp (+ len 1))))))))

(define (read-token port ch)
  (take-until port ch delimiter?))

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
  (let ((tok (take-until port ch delimiter?)))
    (cond
      ((string=? tok "t") #t)
      ((string=? tok "f") #f)
      ((string=? tok "true") #t)
      ((string=? tok "false") #f)
      (else (error 'read-error "invalid boolean" tok)))))

(define (read-prefixed-number port ch)
  (let* ((str (string-append "#" (take-until port ch delimiter?)))
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

(define (read-hex-escape port)
  (let loop ((n 0) (any #f))
    (let ((ch (peek port)))
      (if (and (not (eof-object? ch)) (char-hex-digit? ch))
        (begin
          (next port)
          (loop (+ (* n 16) (hex-digit-value ch)) #t))
        (if any
          (if (eqv? (peek port) #\;)
            (begin
              (next port)
              (integer->char n))
            (error 'read-error "hex escape missing semicolon"))
          (error 'read-error "invalid hex escape"))))))

(define (read-quoted-string port . args)
  (let ((rdelim (if (null? args) #\" (car args)))
        (buf (make-string 16))
        (len 0))
    (letrec ((ensure! (lambda (need)
                     ;; grow geometrically so a long string is O(n), not O(n^2)
                     (when (> (+ len need) (string-length buf))
                       (set! buf (string-append buf (make-string (max need (string-length buf))))))))
          ;; chars read from the port are raw bytes in S7; write them as-is
          (add-byte! (lambda (ch)
                       (ensure! 1)
                       (string-set! buf len ch)
                       (set! len (+ len 1))))
          ;; a hex escape denotes a codepoint, encode it as UTF-8 bytes
          (add-utf8! (lambda (n)
                       (cond
                         ((<= n #x7f)
                          (ensure! 1)
                          (string-set! buf len (integer->char n))
                          (set! len (+ len 1)))
                         ((<= n #x7ff)
                          (ensure! 2)
                          (string-set! buf len (integer->char (+ #xc0 (quotient n 64))))
                          (string-set! buf (+ len 1) (integer->char (+ #x80 (modulo n 64))))
                          (set! len (+ len 2)))
                         ((<= n #xffff)
                          (ensure! 3)
                          (string-set! buf len (integer->char (+ #xe0 (quotient n 4096))))
                          (string-set! buf (+ len 1) (integer->char (+ #x80 (modulo (quotient n 64) 64))))
                          (string-set! buf (+ len 2) (integer->char (+ #x80 (modulo n 64))))
                          (set! len (+ len 3)))
                         (else
                          (ensure! 4)
                          (string-set! buf len (integer->char (+ #xf0 (quotient n 262144))))
                          (string-set! buf (+ len 1) (integer->char (+ #x80 (modulo (quotient n 4096) 64))))
                          (string-set! buf (+ len 2) (integer->char (+ #x80 (modulo (quotient n 64) 64))))
                          (string-set! buf (+ len 3) (integer->char (+ #x80 (modulo n 64))))
                          (set! len (+ len 4)))))))
      (let loop ()
        (let ((ch (next port)))
          (cond
            ((eof-object? ch)
             (error 'read-error "unexpected end of input while reading string"))
            ((eqv? ch rdelim)
             (substring buf 0 len))
            ((eqv? ch #\\)
             (let ((ch (next port)))
               (when (eof-object? ch)
                 (error 'read-error "unexpected end of input while reading string"))
               (cond
                 ((or (eqv? ch #\newline) (eqv? ch #\return))
                  ;; consume the whole line ending, including the second
                  ;; character of a CRLF pair, then any intraline whitespace
                  (when (and (eqv? ch #\return) (eqv? (peek port) #\newline))
                    (next port))
                  (let skip ()
                    (let ((p (peek port)))
                      (when (and (not (eof-object? p))
                                 (or (eqv? p #\space) (eqv? p #\tab)))
                        (next port)
                        (skip))))
                  (loop))
                 ((eqv? ch rdelim)
                  (add-byte! rdelim)
                  (loop))
                 (else
                   (case ch
                     ((#\a) (add-byte! #\alarm))
                     ((#\b) (add-byte! #\backspace))
                      ((#\t) (add-byte! #\tab))
                      ((#\n) (add-byte! #\newline))
                      ((#\r) (add-byte! #\return))
                      ;; S7 extensions beyond R7RS: \f \v \0 \e
                      ((#\f) (add-byte! #\x0c))
                      ((#\v) (add-byte! #\x0b))
                      ((#\0) (add-byte! #\null))
                      ((#\e) (add-byte! #\escape))
                      ((#\" #\\ #\|) (add-byte! ch))
                      ((#\x) (add-utf8! (char->integer (read-hex-escape port))))
                      (else (error 'read-error "invalid character in escape sequence" ch)))
                   (loop)))))
            (else
              (add-byte! ch)
              (loop))))))))

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
       (let* ((token (take-until port ch delimiter?))
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
       (error 'read-error "mismatched close paren" ch))
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
(define *depth* 0)

(define (read-expr port ch)
  (set! *depth* (+ *depth* 1))
  (when (> *depth* *max-depth*)
    (error 'read-error "maximum nesting depth exceeded"))
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
    ((#\)) (error 'read-error "unexpected \")\""))
    ((#\]) (error 'read-error "unexpected \"]\""))
    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\- #\.)
     (read-number port ch))
    (else (read-symbol port ch))))

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
          (let ((tok (take-until port #\! delimiter?)))
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

(define* (read (port (current-input-port)))
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
          (read-error-with-pos port (if (pair? errs) (car errs) '())))))))

;; Replace S7's load: read the file through this Scheme reader and evaluate
;; each form. Searches *load-path* like S7's load did.
(define (load file)
  (define dirs (if (list? *load-path*) *load-path* (list *load-path*)))
  (let loop ((cands (cons file (map (lambda (d) (string-append d "/" file)) dirs))))
    (cond
      ((null? cands)
       (error 'io-error (string-append "cannot load: " file)))
      ((file-exists? (car cands))
       (call-with-input-file (car cands)
         (lambda (port)
           (let loop ()
             (let ((d (read port)))
               (if (eof-object? d)
                 (begin (close-input-port port) #t)
                 (begin (eval d (rootlet)) (loop))))))))
      (else (loop (cdr cands))))))

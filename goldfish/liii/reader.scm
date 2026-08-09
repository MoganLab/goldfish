(import (liii string-cursor))

;; S7's bulk read-string, captured before our own read-string is defined
(define s7-read-string read-string)

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

;; per-datum state; read is not reentrant, so module-level is safe
(define labels '())
(define pending '())

(define (next port)
  (if (< cur-pos cur-len)
    (let ((c (string-ref cur-str cur-pos)))
      (set! cur-pos (+ cur-pos 1))
      c)
    (eof-object)))

(define (peek port)
  (if (< cur-pos cur-len)
    (string-ref cur-str cur-pos)
    (eof-object)))

;; Slurped input buffer. Only one port is read at a time: the whole remaining
;; port content is read once and parsed from the string. Switching ports
;; discards the previous port's content.
(define cur-port #f)
(define cur-str #f)
(define cur-len 0)
(define cur-pos 0)

(define (slurp! port)
  (let loop ((acc '()))
    (let ((s (s7-read-string 65536 port)))
      (if (eof-object? s)
        (let ((str (apply string-append (reverse acc))))
          (set! cur-port port)
          (set! cur-str str)
          (set! cur-len (string-length str))
          (set! cur-pos 0))
        (loop (cons s acc))))))

(define (load-buffer! port)
  (if (eq? cur-port port)
    #f
    (slurp! port)))

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
  (let ((at (char-position #\@ str)))
    (and at
      (let ((r (string->number (substring str 0 at)))
            (theta (string->number (substring str (+ at 1) (string-length str)))))
        (and r theta (real? r) (real? theta) (make-polar r theta))))))

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

(define (take-until port first)
  ;; token = first + cur-str[cur-pos .. delimiter); the delimiter is not consumed
  (let ((tok (g-scan-token cur-str cur-pos first)))
    (set! cur-pos (+ cur-pos (- (string-length tok) 1)))
    tok))

(define (read-token port ch)
  (take-until port ch))

(define (read-symbol port ch)
  (let ((str (read-token port ch)))
    (if (valid-identifier? str)
      (string->symbol (if (fold-case? port) (fold-string str) str))
      (error 'read-error "invalid token" str))))

(define (read-number port ch)
  (let ((str (read-token port ch)))
    (let ((n (or (polar-number str)
                 (string->number str)
                 (pure-imaginary-number str))))
      (if n
        n
        (if (valid-identifier? str)
          (string->symbol (if (fold-case? port) (fold-string str) str))
          (error 'read-error "invalid token" str))))))

(define (read-boolean port ch)
  (let ((tok (take-until port ch)))
    (cond
      ((string=? tok "t") #t)
      ((string=? tok "f") #f)
      ((string=? tok "true") #t)
      ((string=? tok "false") #f)
      (else (error 'read-error "invalid boolean" tok)))))

(define (read-prefixed-number port ch)
  (let ((str (string-append "#" (take-until port ch))))
    (or (string->prefixed-number str)
        (error 'read-error "invalid number" str))))

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

(define (read-string port . args)
  (let ((rdelim (if (null? args) #\" (car args)))
        (buf (make-string 16))
        (len 0))
    (let ((add! (lambda (ch)
                  (define (ensure! need)
                    (when (> (+ len need) (string-length buf))
                      (set! buf (string-append buf (make-string (+ (string-length buf) need))))))
                  (let ((n (char->integer ch)))
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
                       (set! len (+ len 4))))))))
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
                  (let skip ()
                    (let ((p (peek port)))
                      (when (and (not (eof-object? p))
                                 (or (eqv? p #\space) (eqv? p #\tab)))
                        (next port)
                        (skip))))
                  (loop))
                 ((eqv? ch rdelim)
                  (add! rdelim)
                  (loop))
                 (else
                  (add!
                    (case ch
                      ((#\a) #\alarm)
                      ((#\b) #\backspace)
                      ((#\t) #\tab)
                      ((#\n) #\newline)
                      ((#\r) #\return)
                      ((#\" #\\ #\|) ch)
                      ((#\x) (read-hex-escape port))
                      (else (error 'read-error "invalid character in escape sequence" ch))))
                  (loop)))))
            (else
              (add! ch)
              (loop))))))))

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
       (let* ((token (take-until port ch))
              (key (if (fold-case? port) (fold-string token) token))
              (entry (assoc key char-names)))
         (cond
           (entry (cdr entry))
           ((= (string-length token) 1) ch)
           (else (error 'read-error "invalid character" token)))))
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

(define (read-sharp port)
  (let ((ch (next port)))
    (cond
      ((eof-object? ch)
       (error 'read-error "unexpected end of input after #"))
      ((read-hash-procedure ch)
       => (lambda (proc) (proc ch port)))
      (else
        (case ch
          ((#\\) (read-character port))
          ((#\() (read-vector port))
          ((#\t #\T #\f #\F) (read-boolean port ch))
          ((#\b #\B #\o #\O #\d #\D #\x #\X #\e #\E #\i #\I)
           (read-prefixed-number port ch))
          ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (read-label-number port ch))
          ((#\u)
           (if (not (eqv? (peek port) #\8))
             (error 'read-error "invalid #u8")
             (begin
               (next port)
               (if (not (eqv? (peek port) #\())
                 (error 'read-error "invalid #u8")
                 (begin
                   (next port)
                   (read-bytevector port))))))
          (else
            (error 'read-error "Unknown # object" (string #\# ch))))))))

(define (dispatch-close-paren port ch)
  (error 'read-error "unexpected \")\""))
(define (dispatch-close-bracket port ch)
  (error 'read-error "unexpected \"]\""))
(define (dispatch-string port ch)
  (read-string port))
(define (dispatch-bar port ch)
  (string->symbol (read-string port ch)))
(define (dispatch-quote port ch)
  (list 'quote (read-subexpression port "quoted expression")))
(define (dispatch-quasiquote port ch)
  (list 'quasiquote (read-subexpression port "quasiquoted expression")))
(define (dispatch-unquote port ch)
  (cond
    ((eq? #\@ (peek port))
     (next port)
     (list 'unquote-splicing (read-subexpression port "subexpression of ,@")))
    (else
     (list 'unquote (read-subexpression port "unquoted expression")))))
(define (dispatch-sharp port ch)
  (read-sharp port))

(define (dispatch-close-paren port ch)
  (error 'read-error "unexpected \")\""))
(define (dispatch-close-bracket port ch)
  (error 'read-error "unexpected \"]\""))
(define (dispatch-string port ch)
  (read-string port))
(define (dispatch-bar port ch)
  (string->symbol (read-string port ch)))
(define (dispatch-quote port ch)
  (list 'quote (read-subexpression port "quoted expression")))
(define (dispatch-quasiquote port ch)
  (list 'quasiquote (read-subexpression port "quasiquoted expression")))
(define (dispatch-unquote port ch)
  (cond
    ((eq? #\@ (peek port))
     (next port)
     (list 'unquote-splicing (read-subexpression port "subexpression of ,@")))
    (else
     (list 'unquote (read-subexpression port "unquoted expression")))))
(define (dispatch-sharp port ch)
  (read-sharp port))

(define read-dispatch (make-vector 256 read-symbol))
(do ((i (char->integer #\0) (+ i 1)))
    ((> i (char->integer #\9)))
  (vector-set! read-dispatch i read-number))
(vector-set! read-dispatch (char->integer #\+) read-number)
(vector-set! read-dispatch (char->integer #\-) read-number)
(vector-set! read-dispatch (char->integer #\.) read-number)
(vector-set! read-dispatch (char->integer #\() (lambda (port ch) (read-parenthesized port #\))))
(vector-set! read-dispatch (char->integer #\[) (lambda (port ch) (read-parenthesized port #\])))
(vector-set! read-dispatch (char->integer #\") dispatch-string)
(vector-set! read-dispatch (char->integer #\|) dispatch-bar)
(vector-set! read-dispatch (char->integer #\') dispatch-quote)
(vector-set! read-dispatch (char->integer #\`) dispatch-quasiquote)
(vector-set! read-dispatch (char->integer #\,) dispatch-unquote)
(vector-set! read-dispatch (char->integer #\#) dispatch-sharp)
(vector-set! read-dispatch (char->integer #\)) dispatch-close-paren)
(vector-set! read-dispatch (char->integer #\]) dispatch-close-bracket)

(define (read-expr port ch)
  ((vector-ref read-dispatch (char->integer ch)) port ch))

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
  (let loop ()
    (set! cur-pos (g-skip-whitespace cur-str cur-pos))
    (if (>= cur-pos cur-len)
      (eof-object)
      (let ((ch (string-ref cur-str cur-pos)))
        (case ch
          ((#\;)
           (set! cur-pos (+ cur-pos 1))
           (skip-line-comment port)
           (loop))
          ((#\#)
           (if (>= (+ cur-pos 1) cur-len)
             (begin
               (set! cur-pos (+ cur-pos 1))
               ch)
             (let ((c2 (string-ref cur-str (+ cur-pos 1))))
               (case c2
                 ((#\!)
                  (set! cur-pos (+ cur-pos 2))
                  (let ((tok (take-until port #\!)))
                    (cond
                      ((string=? tok "!fold-case") (set-fold-case! port #t))
                      ((string=? tok "!no-fold-case") (set-fold-case! port #f))
                      (else (error 'read-error "unknown directive" tok)))
                    (loop)))
                 ((#\|)
                  (if (read-hash-procedure #\|)
                    (begin
                      (set! cur-pos (+ cur-pos 1))
                      ch)
                    (begin
                      (set! cur-pos (+ cur-pos 2))
                      (skip-block-comment port)
                      (loop))))
                 ((#\;)
                  (set! cur-pos (+ cur-pos 2))
                  (let ((dch (next-non-whitespace port)))
                    (if (eof-object? dch)
                      (error 'read-error "datum comment has no datum")
                      (read-expr port dch)))
                  (loop))
                 (else
                   (set! cur-pos (+ cur-pos 1))
                   ch)))))
          (else
            (set! cur-pos (+ cur-pos 1))
            ch))))))

(define* (read (port (current-input-port)))
  (set! labels '())
  (set! pending '())
  (load-buffer! port)
  (let ((ch (next-non-whitespace port)))
    (if (eof-object? ch)
      ch
      (read-expr port ch))))

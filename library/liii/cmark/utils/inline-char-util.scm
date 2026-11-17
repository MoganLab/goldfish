(define-library (liii cmark utils inline-char-util)
  (import (scheme base))
  (import (scheme char))
  (import (liii ascii))
  (export all-spaces? utf8-codepoint-at previous-char-start byte-punctuation?
    unicode-punctuation-at? unicode-punctuation-before? unicode-whitespace-at?
    unicode-whitespace-before?
  ) ;export
  (begin
    (define (all-spaces? str)
      (let ((len (string-length str)))
        (let loop
          ((pos 0))
          (cond ((>= pos len) #t)
                ((char=? (string-ref str pos) #\space) (loop (+ pos 1)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (utf8-codepoint-at str pos)
      (let ((len (string-length str)))
        (and (< pos len)
          (let ((b0 (char->integer (string-ref str pos))))
            (cond ((< b0 128) (cons b0 1))
                  ((< b0 224)
                   (and (< (+ pos 1) len)
                     (cons
                       (+ (* (- b0 192) 64) (- (char->integer (string-ref str (+ pos 1))) 128))
                       2
                     ) ;cons
                   ) ;and
                  ) ;
                  ((< b0 240)
                   (and (< (+ pos 2) len)
                     (cons
                       (+ (* (- b0 224) 4096)
                         (*
                           (- (char->integer (string-ref str (+ pos 1))) 128)
                           64
                         ) ;*
                         (- (char->integer (string-ref str (+ pos 2))) 128)
                       ) ;+
                       3
                     ) ;cons
                   ) ;and
                  ) ;
                  ((< b0 248)
                   (and (< (+ pos 3) len)
                     (cons
                       (+ (* (- b0 240) 262144)
                         (*
                           (- (char->integer (string-ref str (+ pos 1))) 128)
                           4096
                         ) ;*
                         (*
                           (- (char->integer (string-ref str (+ pos 2))) 128)
                           64
                         ) ;*
                         (- (char->integer (string-ref str (+ pos 3))) 128)
                       ) ;+
                       4
                     ) ;cons
                   ) ;and
                  ) ;
                  (else #f)
            ) ;cond
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (previous-char-start str pos)
      (let loop
        ((p (- pos 1)))
        (if
          (and (>= p 0)
            (>= (char->integer (string-ref str p)) 128)
            (< (char->integer (string-ref str p)) 192)
          ) ;and
          (loop (- p 1))
          p
        ) ;if
      ) ;let
    ) ;define

    (define (byte-punctuation? c)
      (and (not (ascii-whitespace? c))
        (not (char-alphabetic? c))
        (not (char-numeric? c))
      ) ;and
    ) ;define

    (define (unicode-punctuation-at? str pos)
      (let ((decoded (utf8-codepoint-at str pos)))
        (and decoded
          (let ((cp (car decoded)))
            (if (< cp 128)
              (byte-punctuation? (integer->char cp))
              (let ((c (integer->char cp)))
                (and (not (char-whitespace? c))
                  (not (char-alphabetic? c))
                  (not (char-numeric? c))
                ) ;and
              ) ;let
            ) ;if
          ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (unicode-punctuation-before? str pos)
      (let ((start (previous-char-start str pos)))
        (and (>= start 0) (unicode-punctuation-at? str start))
      ) ;let
    ) ;define

    (define (unicode-whitespace-at? str pos)
      (let ((decoded (utf8-codepoint-at str pos)))
        (and decoded (char-whitespace? (integer->char (car decoded))))
      ) ;let
    ) ;define

    (define (unicode-whitespace-before? str pos)
      (let ((start (previous-char-start str pos)))
        (and (>= start 0) (unicode-whitespace-at? str start))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

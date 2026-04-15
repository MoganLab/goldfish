(define-library (liii goldfix-tokenize)
  (export tokenize
    tokenize-lines
    code-token?
    line-start-close?
  ) ;export
  (import (liii base)
    (liii string)
    (liii goldfix-record)
  ) ;import

  (begin
    (define (whitespace-char? c)
      (or (char=? c #\space)
        (char=? c #\tab)
        (char=? c #\newline)
        (char=? c #\return)
      ) ;or
    ) ;define

    (define (delimiter-char? c)
      (or (whitespace-char? c)
        (char=? c #\()
        (char=? c #\))
        (char=? c #\;)
        (char=? c #\")
      ) ;or
    ) ;define
    (define (string-prefix-at? str prefix pos)
      (let ((len (string-length str))
            (prefix-len (string-length prefix))
           ) ;
        (and (<= (+ pos prefix-len) len)
          (string=? (substring str pos (+ pos prefix-len))
            prefix
          ) ;string=?
        ) ;and
      ) ;let
    ) ;define
    (define (find-string-end source start)
      (let ((len (string-length source)))
        (let loop
          ((i (+ start 1)) (escaped #f))
          (cond ((>= i len) len)
                (escaped (loop (+ i 1) #f))
                ((char=? (string-ref source i) #\\)
                 (loop (+ i 1) #t)
                ) ;
                ((char=? (string-ref source i) #\")
                 (+ i 1)
                ) ;
                (else (loop (+ i 1) #f))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (find-bar-symbol-end source start)
      (let ((len (string-length source)))
        (let loop
          ((i (+ start 1)) (escaped #f))
          (cond ((>= i len) len)
                (escaped (loop (+ i 1) #f))
                ((char=? (string-ref source i) #\\)
                 (loop (+ i 1) #t)
                ) ;
                ((char=? (string-ref source i) #\|)
                 (+ i 1)
                ) ;
                (else (loop (+ i 1) #f))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (find-block-comment-end source start)
      (let ((len (string-length source)))
        (let loop
          ((i (+ start 2)) (level 1))
          (cond ((>= i len) len)
                ((and (< (+ i 1) len)
                   (char=? (string-ref source i) #\#)
                   (char=? (string-ref source (+ i 1)) #\|)
                 ) ;and
                 (loop (+ i 2) (+ level 1))
                ) ;
                ((and (< (+ i 1) len)
                   (char=? (string-ref source i) #\|)
                   (char=? (string-ref source (+ i 1)) #\#)
                 ) ;and
                 (if (= level 1)
                   (+ i 2)
                   (loop (+ i 2) (- level 1))
                 ) ;if
                ) ;
                (else (loop (+ i 1) level))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (find-raw-string-end source start)
      (let* ((len (string-length source))
             (delimiter-end (string-index source #\" (+ start 2))
             ) ;delimiter-end
            ) ;
        (if delimiter-end
          (let* ((delimiter (substring source
                              (+ start 2)
                              delimiter-end
                            ) ;substring
                 ) ;delimiter
                 (delimiter-len (string-length delimiter)
                 ) ;delimiter-len
                 (content-start (+ delimiter-end 1))
                ) ;
            (if (= delimiter-len 0)
              (let loop
                ((i content-start))
                (cond ((>= i len) len)
                      ((and (< (+ i 1) len)
                         (char=? (string-ref source i) #\")
                         (char=? (string-ref source (+ i 1)) #\")
                       ) ;and
                       (+ i 2)
                      ) ;
                      (else (loop (+ i 1)))
                ) ;cond
              ) ;let
              (let loop
                ((i content-start))
                (cond ((>= i len) len)
                      ((and (<= (+ i delimiter-len) len)
                         (string-prefix-at? source delimiter i)
                         (< (+ i delimiter-len) len)
                         (char=? (string-ref source (+ i delimiter-len))
                           #\"
                         ) ;char=?
                       ) ;and
                       (+ i delimiter-len 1)
                      ) ;
                      (else (loop (+ i 1)))
                ) ;cond
              ) ;let
            ) ;if
          ) ;let*
          len
        ) ;if
      ) ;let*
    ) ;define
    (define (find-char-end source start)
      (let ((len (string-length source)))
        (if (>= (+ start 2) len)
          len
          (let ((c (string-ref source (+ start 2))))
            (if (delimiter-char? c)
              (+ start 3)
              (let loop
                ((i (+ start 3)))
                (if (or (>= i len)
                      (delimiter-char? (string-ref source i))
                    ) ;or
                  i
                  (loop (+ i 1))
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (find-line-comment-end source start)
      (let ((len (string-length source)))
        (let loop
          ((i start))
          (cond ((>= i len) len)
                ((char=? (string-ref source i) #\newline)
                 i
                ) ;
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (find-other-end source start)
      (let ((len (string-length source)))
        (let loop
          ((i start))
          (cond ((>= i len) len)
                ((delimiter-char? (string-ref source i))
                 i
                ) ;
                ((and (< (+ i 1) len)
                   (char=? (string-ref source i) #\#)
                   (char=? (string-ref source (+ i 1)) #\|)
                 ) ;and
                 i
                ) ;
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (code-token? token)
      (and (fix-token? token)
        (not (eq? (fix-token-type token)
               'line-comment
             ) ;eq?
        ) ;not
        (not (eq? (fix-token-type token)
               'block-comment
             ) ;eq?
        ) ;not
      ) ;and
    ) ;define
    (define (first-code-token tokens)
      (cond ((null? tokens) #f)
            ((code-token? (car tokens))
             (car tokens)
            ) ;
            (else (first-code-token (cdr tokens)))
      ) ;cond
    ) ;define
    (define (tokens-for-line tokens line-number)
      (let loop
        ((rest tokens) (result '()))
        (cond ((null? rest) (reverse result))
              ((= (fix-token-line (car rest))
                 line-number
               ) ;=
               (loop (cdr rest)
                 (cons (car rest) result)
               ) ;loop
              ) ;
              (else (loop (cdr rest) result))
        ) ;cond
      ) ;let
    ) ;define
    (define (list-ref-safe lst index)
      (let loop
        ((rest lst) (i index))
        (cond ((null? rest) 0)
              ((= i 0) (car rest))
              (else (loop (cdr rest) (- i 1)))
        ) ;cond
      ) ;let
    ) ;define
    (define (build-lines source tokens line-starts)
      (let ((line-count (length line-starts)))
        (let loop
          ((line-number 1) (result '()))
          (if (> line-number line-count)
            (reverse result)
            (let* ((line-tokens (tokens-for-line tokens line-number)
                   ) ;line-tokens
                   (first (first-code-token line-tokens))
                   (start-offset (list-ref-safe line-starts
                                   (- line-number 1)
                                 ) ;list-ref-safe
                   ) ;start-offset
                  ) ;
              (loop (+ line-number 1)
                (cons (make-fix-line :number
                        line-number
                        :start-offset
                        start-offset
                        :first-code-token
                        first
                        :tokens
                        line-tokens
                      ) ;make-fix-line
                  result
                ) ;cons
              ) ;loop
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (line-start-close? token line)
      (and (fix-token? token)
        (fix-line? line)
        (eq? (fix-token-type token)
          'close-paren
        ) ;eq?
        (let ((first (fix-line-first-code-token line))
             ) ;
          (and first
            (= (fix-token-offset token)
              (fix-token-offset first)
            ) ;=
          ) ;and
        ) ;let
      ) ;and
    ) ;define
    (define (tokenize source)
      (let ((len (string-length source))
            (tokens '())
            (i 0)
            (line 1)
            (column 0)
           ) ;
        (define (next-char offset)
          (if (< offset len)
            (string-ref source offset)
            #\null
          ) ;if
        ) ;define
        (define (advance-range! start end)
          (let loop
            ((pos start))
            (if (< pos end)
              (let ((c (string-ref source pos)))
                (if (char=? c #\newline)
                  (begin
                    (set! line (+ line 1))
                    (set! column 0)
                  ) ;begin
                  (set! column (+ column 1))
                ) ;if
                (loop (+ pos 1))
              ) ;let
            ) ;if
          ) ;let
        ) ;define
        (define (add-token! type
                  start
                  end
                  token-line
                  token-column
                ) ;add-token!
          (set! tokens
            (cons (make-fix-token :type
                    type
                    :offset
                    start
                    :end
                    end
                    :line
                    token-line
                    :column
                    token-column
                    :text
                    (substring source start end)
                  ) ;make-fix-token
              tokens
            ) ;cons
          ) ;set!
          (advance-range! start end)
          (set! i end)
        ) ;define
        (let loop
          ()
          (if (>= i len)
            (reverse tokens)
            (let ((c (next-char i))
                  (next-c (next-char (+ i 1)))
                  (start i)
                  (token-line line)
                  (token-column column)
                 ) ;
              (cond ((whitespace-char? c)
                     (advance-range! i (+ i 1))
                     (set! i (+ i 1))
                     (loop)
                    ) ;
                    ((char=? c #\()
                     (add-token! 'open-paren
                       start
                       (+ start 1)
                       token-line
                       token-column
                     ) ;add-token!
                     (loop)
                    ) ;
                    ((char=? c #\))
                     (add-token! 'close-paren
                       start
                       (+ start 1)
                       token-line
                       token-column
                     ) ;add-token!
                     (loop)
                    ) ;
                    ((char=? c #\;)
                     (add-token! 'line-comment
                       start
                       (find-line-comment-end source start)
                       token-line
                       token-column
                     ) ;add-token!
                     (loop)
                    ) ;
                    ((and (char=? c #\#) (char=? next-c #\|))
                     (add-token! 'block-comment
                       start
                       (find-block-comment-end source start)
                       token-line
                       token-column
                     ) ;add-token!
                     (loop)
                    ) ;
                    ((and (char=? c #\#) (char=? next-c #\\))
                     (add-token! 'char
                       start
                       (find-char-end source start)
                       token-line
                       token-column
                     ) ;add-token!
                     (loop)
                    ) ;
                    ((and (char=? c #\#) (char=? next-c #\"))
                     (add-token! 'raw-string
                       start
                       (find-raw-string-end source start)
                       token-line
                       token-column
                     ) ;add-token!
                     (loop)
                    ) ;
                    ((char=? c #\")
                     (add-token! 'string
                       start
                       (find-string-end source start)
                       token-line
                       token-column
                     ) ;add-token!
                     (loop)
                    ) ;
                    ((char=? c #\|)
                     (add-token! 'other
                       start
                       (find-bar-symbol-end source start)
                       token-line
                       token-column
                     ) ;add-token!
                     (loop)
                    ) ;
                    (else (add-token! 'other
                            start
                            (find-other-end source start)
                            token-line
                            token-column
                          ) ;add-token!
                      (loop)
                    ) ;else
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define
    (define (tokenize-lines source)
      (let ((tokens (tokenize source))
            (len (string-length source))
           ) ;
        (let loop
          ((pos 0) (starts '(0)))
          (if (>= pos len)
            (build-lines source
              tokens
              (reverse starts)
            ) ;build-lines
            (if (char=? (string-ref source pos)
                  #\newline
                ) ;char=?
              (loop (+ pos 1) (cons (+ pos 1) starts))
              (loop (+ pos 1) starts)
            ) ;if
          ) ;if
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library


(define-library (liii cmark utils inline-html-util)
  (import (scheme base))
  (import (scheme char))
  (import (liii utils text-util))
  (import (liii cmark utils common))
  (export inline-html-start? find-inline-html-end)
  (begin
    (define (find-string str start pattern)
      (let ((pattern-len (string-length pattern)) (len (string-length str)))
        (let loop
          ((i start))
          (cond ((> (+ i pattern-len) len) #f)
                ((string=? (substring str i (+ i pattern-len)) pattern) i)
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; 宽松标签结束扫描（declaration <!...> 用：跳过引号找 >）
    (define (find-unquoted-tag-end str start)
      (let ((len (string-length str)))
        (let loop
          ((i (+ start 1)) (cur-quote #f))
          (cond ((>= i len) #f)
                (cur-quote (if (char=? (string-ref str i) cur-quote)
                             (loop (+ i 1) #f)
                             (loop (+ i 1) cur-quote)
                           ) ;if
                ) ;cur-quote
                ((or (char=? (string-ref str i) #\") (char=? (string-ref str i) #\'))
                 (loop (+ i 1) (string-ref str i))
                ) ;
                ((char=? (string-ref str i) #\>) (+ i 1))
                (else (loop (+ i 1) #f))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (html-attrname-char? ch)
      (or (char-alphabetic? ch)
        (char-numeric? ch)
        (char=? ch #\_)
        (char=? ch #\:)
        (char=? ch #\.)
        (char=? ch #\-)
        (char=? ch #\:)
      ) ;or
    ) ;define
    (define (read-html-attrname str i)
      (let ((len (string-length str)))
        (if
          (or (>= i len)
            (not (or (char-alphabetic? (string-ref str i))
                   (char=? (string-ref str i) #\_)
                   (char=? (string-ref str i) #\:)
                 ) ;or
            ) ;not
          ) ;or
          #f
          (let loop
            ((j (+ i 1)))
            (if (and (< j len) (html-attrname-char? (string-ref str j))) (loop (+ j 1)) j)
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (unquoted-value-char? ch)
      (and (not (html-whitespace? ch))
        (not (char=? ch #\"))
        (not (char=? ch #\'))
        (not (char=? ch #\=))
        (not (char=? ch #\<))
        (not (char=? ch #\>))
        (not (char=? ch #\`))
      ) ;and
    ) ;define
    (define (skip-unquoted-value str i)
      (let ((len (string-length str)))
        (let loop
          ((j i))
          (if (and (< j len) (unquoted-value-char? (string-ref str j))) (loop (+ j 1)) j)
        ) ;let
      ) ;let
    ) ;define
    (define (find-quote-end str qpos quote)
      (let ((len (string-length str)))
        (let loop
          ((i (+ qpos 1)))
          (cond ((>= i len) #f)
                ((char=? (string-ref str i) quote) (+ i 1))
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (parse-attribute str i)
      (let ((attr-name-end (read-html-attrname str i)))
        (and attr-name-end
          (let ((j (skip-html-ws str attr-name-end)))
            (cond ((>= j (string-length str)) attr-name-end)
                  ((char=? (string-ref str j) #\=)
                   (let ((k (skip-html-ws str (+ j 1))))
                     (cond ((>= k (string-length str)) #f)
                           ((char=? (string-ref str k) #\") (find-quote-end str k #\"))
                           ((char=? (string-ref str k) #\') (find-quote-end str k #\'))
                           ((unquoted-value-char? (string-ref str k)) (skip-unquoted-value str k))
                           (else #f)
                     ) ;cond
                   ) ;let
                  ) ;
                  (else attr-name-end)
            ) ;cond
          ) ;let
        ) ;and
      ) ;let
    ) ;define
    (define (find-open-tag-end str start)
      (let ((len (string-length str)) (name-end (read-html-tag-name str (+ start 1))))
        (and name-end
          (let loop
            ((i name-end))
            (let ((i2 (skip-html-ws str i)))
              (cond ((>= i2 len) #f)
                    ((char=? (string-ref str i2) #\>) (+ i2 1))
                    ((and (char=? (string-ref str i2) #\/)
                       (< (+ i2 1) len)
                       (char=? (string-ref str (+ i2 1)) #\>)
                     ) ;and
                     (+ i2 2)
                    ) ;
                    ((= i2 i) #f)
                    (else
                      (let ((attr-end (parse-attribute str i2)))
                        (and attr-end (loop attr-end))
                      ) ;let
                    ) ;else
              ) ;cond
            ) ;let
          ) ;let
        ) ;and
      ) ;let
    ) ;define
    (define (html-whitespace? ch)
      (or (char=? ch #\space)
        (char=? ch #\tab)
        (char=? ch #\newline)
        (char=? ch #\return)
      ) ;or
    ) ;define
    (define (find-closing-tag-end str start)
      (let ((len (string-length str)))
        (let loop-name
          ((i (+ start 2)))
          (cond ((>= i len) #f)
                ((html-tag-name-char? (string-ref str i)) (loop-name (+ i 1)))
                (else
                  (let loop-end
                    ((j i))
                    (cond ((>= j len) #f)
                          ((html-whitespace? (string-ref str j)) (loop-end (+ j 1)))
                          ((char=? (string-ref str j) #\>) (+ j 1))
                          (else #f)
                    ) ;cond
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (skip-html-ws str i)
      (let ((len (string-length str)))
        (let loop
          ((j i))
          (if (and (< j len) (html-whitespace? (string-ref str j))) (loop (+ j 1)) j)
        ) ;let
      ) ;let
    ) ;define
    (define (closing-tag-start? str pos)
      (and (string-prefix-at? str pos "</")
        (< (+ pos 2) (string-length str))
        (char-alphabetic? (string-ref str (+ pos 2)))
      ) ;and
    ) ;define
    (define (open-tag-start? str pos)
      (and (string-prefix-at? str pos "<")
        (< (+ pos 1) (string-length str))
        (char-alphabetic? (string-ref str (+ pos 1)))
      ) ;and
    ) ;define
    (define (inline-html-start? str pos)
      (or (open-tag-start? str pos)
        (closing-tag-start? str pos)
        (comment-start? str pos)
        (processing-instruction-start? str pos)
        (cdata-start? str pos)
        (declaration-start? str pos)
      ) ;or
    ) ;define
    (define (find-inline-html-end str start)
      (cond
       ((comment-start? str start)
        (if (string-prefix-at? str (+ start 4) ">")
          (+ start 5)
          (let ((end-pos (find-string str (+ start 3) "-->")))
            (and end-pos (+ end-pos 3))
          ) ;let
        ) ;if
       ) ;
       ((processing-instruction-start? str start)
        (let ((end-pos (find-string str (+ start 2) "?>")))
          (and end-pos (+ end-pos 2))
        ) ;let
       ) ;
       ((cdata-start? str start)
        (let ((end-pos (find-string str (+ start 9) "]]>")))
          (and end-pos (+ end-pos 3))
        ) ;let
       ) ;
       ((open-tag-start? str start) (find-open-tag-end str start))
       ((declaration-start? str start) (find-unquoted-tag-end str start))
       ((closing-tag-start? str start) (find-closing-tag-end str start))
       (else #f)
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library

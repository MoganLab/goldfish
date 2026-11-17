
(define-library (liii cmark inline autolink)
  (import (scheme base))
  (import (scheme char))
  (import (liii string))
  (import (liii ascii))
  (import (liii cmark node))
  (import (liii cmark inline entity))
  (import (liii cmark common))
  (import (liii cmark text))
  (export autolink-start? parse-autolink bare-autolink-start?
    parse-bare-autolink bare-autolink-candidate-start?
  ) ;export
  (begin
    ;; ascii-alnum? 复用 (liii ascii) 的 ascii-alphanumeric?
    (define (ascii-alnum? ch)
      (ascii-alphanumeric? ch)
    ) ;define

    (define (count-char-in-range str start end target)
      (let loop
        ((pos start) (count 0))
        (if (>= pos end)
          count
          (loop (+ pos 1) (if (char=? (string-ref str pos) target) (+ count 1) count))
        ) ;if
      ) ;let
    ) ;define

    (define bare-autolink-multibyte-trailing-punctuations
      (list "。" "，" "；" "：" "！" "？")
    ) ;define

    (define (trailing-bare-autolink-ascii-punctuation? ch)
      (or (char=? ch #\.)
        (char=? ch #\,)
        (char=? ch #\;)
        (char=? ch #\:)
        (char=? ch #\!)
        (char=? ch #\?)
        (char=? ch #\")
        (char=? ch #\')
      ) ;or
    ) ;define
    (define (trailing-bare-autolink-punctuation-length str start end)
      (if (<= end start)
        #f
        (let ((last-char (string-ref str (- end 1))))
          (if (trailing-bare-autolink-ascii-punctuation? last-char)
            1
            (let loop
              ((suffixes bare-autolink-multibyte-trailing-punctuations))
              (if (null? suffixes)
                #f
                (let* ((suffix (car suffixes)) (suffix-len (string-length suffix)))
                  (if
                    (and (<= (+ start suffix-len) end)
                      (string=? (substring str (- end suffix-len) end) suffix)
                    ) ;and
                    suffix-len
                    (loop (cdr suffixes))
                  ) ;if
                ) ;let*
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define
    (define (trim-bare-autolink-end str start end)
      (let loop
        ((trim-end end))
        (if (<= trim-end start)
          start
          (let ((last-char (string-ref str (- trim-end 1)))
                (punct-len (trailing-bare-autolink-punctuation-length str start trim-end))
               ) ;
            (cond (punct-len (loop (- trim-end punct-len)))
                  ((char=? last-char #\))
                   (if (> (count-char-in-range str start trim-end #\))
                         (count-char-in-range str start trim-end #\()
                       ) ;>
                     (loop (- trim-end 1))
                     trim-end
                   ) ;if
                  ) ;
                  ((char=? last-char #\])
                   (if (> (count-char-in-range str start trim-end #\])
                         (count-char-in-range str start trim-end #\[)
                       ) ;>
                     (loop (- trim-end 1))
                     trim-end
                   ) ;if
                  ) ;
                  ((char=? last-char #\})
                   (if (> (count-char-in-range str start trim-end #\})
                         (count-char-in-range str start trim-end #\{)
                       ) ;>
                     (loop (- trim-end 1))
                     trim-end
                   ) ;if
                  ) ;
                  (else trim-end)
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (bare-autolink-candidate-start? str pos)
      (or (string-prefix-at? str pos "http://")
        (string-prefix-at? str pos "https://")
      ) ;or
    ) ;define
    (define (bare-autolink-boundary? str pos)
      (or (= pos 0)
        (let ((prev (string-ref str (- pos 1))))
          (not
            (or (ascii-alnum? prev)
              (char=? prev #\_)
              (char=? prev #\/)
              (char=? prev #\@)
              (and (char=? prev #\() (>= pos 2) (char=? (string-ref str (- pos 2)) #\]))
            ) ;or
          ) ;not
        ) ;let
      ) ;or
    ) ;define
    (define (bare-email-char? ch)
      (or (email-local-char? ch) (char=? ch #\@) (email-domain-char? ch))
    ) ;define
    (define (bare-email-end-check str start pos)
      (let trim
        ((end pos))
        (if
          (and (> end start) (char=? (string-ref str (- end 1)) #\.))
          (trim (- end 1))
          (let ((candidate (substring str start end)))
            (and (string-index candidate #\@ 0) (email-content? candidate) end)
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (find-bare-email-end str start)
      (let ((len (string-length str)))
        (let loop
          ((pos start))
          (cond ((>= pos len) (bare-email-end-check str start pos))
                ((bare-email-char? (string-ref str pos)) (loop (+ pos 1)))
                (else (bare-email-end-check str start pos))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
    (define (find-bare-url-end str start)
      (let ((len (string-length str)))
        (if (not (and (bare-autolink-candidate-start? str start)
                   (bare-autolink-boundary? str start)
                 ) ;and
            ) ;not
          #f
          (let loop
            ((pos start))
            (if (or (>= pos len)
                  (char=? (string-ref str pos) #\space)
                  (char=? (string-ref str pos) #\tab)
                  (char=? (string-ref str pos) #\newline)
                  (char=? (string-ref str pos) #\return)
                  (char=? (string-ref str pos) #\<)
                ) ;or
              (let ((trim-end (trim-bare-autolink-end str start pos))
                    (scheme-len (if (string-prefix-at? str start "https://") 8 7))
                   ) ;
                (and (> trim-end (+ start scheme-len)) trim-end)
              ) ;let
              (loop (+ pos 1))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (find-bare-autolink-end str start)
      (if (bare-autolink-candidate-start? str start)
        (find-bare-url-end str start)
        (find-bare-email-end str start)
      ) ;if
    ) ;define
    (define (bare-autolink-start? str pos)
      (let ((end-pos (find-bare-autolink-end str pos)))
        (and end-pos #t)
      ) ;let
    ) ;define
    (define (parse-bare-autolink str start)
      (let* ((len (string-length str)) (end-pos (find-bare-autolink-end str start)))
        (if end-pos
          (let* ((raw-url (substring str start end-pos))
                 (display-url (decode-entity-references-in-string raw-url))
                 (url (if (string-index raw-url #\@ 0)
                        (string-append "mailto:" display-url)
                        (percent-encode-uri display-url)
                      ) ;if
                 ) ;url
                 (link-node (make-cmark-link url ""))
                ) ;
            (cmark-node-append-child! link-node (make-cmark-text display-url))
            (cons link-node end-pos)
          ) ;let*
          (cons (make-cmark-text (substring str start len)) len)
        ) ;if
      ) ;let*
    ) ;define
    (define (char-in-str? ch s)
      (and (string-index s ch 0) #t)
    ) ;define
    (define (scheme-char? ch)
      (or (ascii-alnum? ch) (char=? ch #\+) (char=? ch #\.) (char=? ch #\-))
    ) ;define
    (define (uri-autolink-content? content)
      (let* ((len (string-length content)) (colon-pos (string-index content #\: 0)))
        (and colon-pos
          (>= colon-pos 2)
          (<= colon-pos 32)
          (char-alphabetic? (string-ref content 0))
          (let scheme-ok
            ((i 1))
            (if (< i colon-pos)
              (and (scheme-char? (string-ref content i)) (scheme-ok (+ i 1)))
              #t
            ) ;if
          ) ;let
          (let uri-ok
            ((i (+ colon-pos 1)))
            (if (< i len)
              (let ((c (string-ref content i)))
                (and (not (char-whitespace? c))
                  (>= (char->integer c) 32)
                  (not (char=? c #\<))
                  (not (char=? c #\>))
                  (uri-ok (+ i 1))
                ) ;and
              ) ;let
              #t
            ) ;if
          ) ;let
        ) ;and
      ) ;let*
    ) ;define
    (define (email-local-char? ch)
      (or (ascii-alnum? ch) (char-in-str? ch ".!#$%&'*+/=?^_`{|}~-"))
    ) ;define
    (define (email-domain-char? ch)
      (or (ascii-alnum? ch) (char=? ch #\.) (char=? ch #\-))
    ) ;define
    (define (email-content? content)
      (let* ((len (string-length content)) (at-pos (string-index content #\@ 0)))
        (and at-pos
          (> at-pos 0)
          (< (+ at-pos 1) len)
          (let local-ok
            ((i 0))
            (if (< i at-pos)
              (and (email-local-char? (string-ref content i)) (local-ok (+ i 1)))
              #t
            ) ;if
          ) ;let
          (let domain-ok
            ((i (+ at-pos 1)) (prev-dot #t))
            (cond ((>= i len) (not prev-dot))
                  ((char=? (string-ref content i) #\.)
                   (and (not prev-dot) (domain-ok (+ i 1) #t))
                  ) ;
                  ((and (email-domain-char? (string-ref content i))
                     (not (char=? (string-ref content i) #\.))
                     (not (char=? (string-ref content i) #\-))
                   ) ;and
                   (domain-ok (+ i 1) #f)
                  ) ;
                  ((and (char=? (string-ref content i) #\-) (not prev-dot))
                   (domain-ok (+ i 1) #f)
                  ) ;
                  (else #f)
            ) ;cond
          ) ;let
        ) ;and
      ) ;let*
    ) ;define
    (define (autolink-start? str pos)
      (and (< (+ pos 1) (string-length str))
        (char=? (string-ref str pos) #\<)
        (let ((next-char (string-ref str (+ pos 1))))
          (or (char-alphabetic? next-char) (email-local-char? next-char))
        ) ;let
        (let scan
          ((i (+ pos 1)))
          (cond ((>= i (string-length str)) #f)
                ((char=? (string-ref str i) #\>) #f)
                ((or (char=? (string-ref str i) #\:) (char=? (string-ref str i) #\@)) #t)
                (else (scan (+ i 1)))
          ) ;cond
        ) ;let
      ) ;and
    ) ;define
    (define (parse-autolink str start)
      (let* ((len (string-length str)) (end-pos (string-index str #\> (+ start 1))))
        (if end-pos
          (let* ((content (substring str (+ start 1) end-pos))
                 (display-url (decode-entity-references-in-string content))
                ) ;
            (cond
             ((uri-autolink-content? content)
              (let* ((url (percent-encode-uri display-url #\[ #\]))
                     (link-node (make-cmark-link url ""))
                    ) ;
                (cmark-node-append-child! link-node (make-cmark-text display-url))
                (cons link-node (+ end-pos 1))
              ) ;let*
             ) ;
             ((email-content? content)
              (let* ((url (string-append "mailto:" display-url))
                     (link-node (make-cmark-link url ""))
                    ) ;
                (cmark-node-append-child! link-node (make-cmark-text display-url))
                (cons link-node (+ end-pos 1))
              ) ;let*
             ) ;
             (else #f)
            ) ;cond
          ) ;let*
          #f
        ) ;if
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library

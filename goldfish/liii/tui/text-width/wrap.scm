;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(define-library (liii tui text-width wrap)
  (export wrap-text)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii list)
    (liii string)
    (liii unicode)
    (liii tui text-width core)
  ) ;import
  (begin

    (define (%string-ends-with-reset? str)
      (let ((len (string-length str)))
        (and (>= len 4) (string=? (substring str (- len 4) len) "\x1b;[0m"))
      ) ;let
    ) ;define

    (define (%trim-rev-line rev-line)
      (let loop
        ((rest rev-line))
        (cond ((null? rest) '())
              ((string=? (car rest) " ") (loop (cdr rest)))
              (else rest)
        ) ;cond
      ) ;let
    ) ;define

    (define (%close-line cur-line active-ansi)
      (let* ((trimmed (%trim-rev-line cur-line))
             (raw (apply string-append (reverse trimmed)))
            ) ;
        (if (and (not (string=? active-ansi "")) (not (%string-ends-with-reset? raw)))
          (string-append raw "\x1b;[0m")
          raw
        ) ;if
      ) ;let*
    ) ;define

    (define (%scan-ascii-word bv j len)
      (let loop
        ((k j) (acc '()))
        (if (>= k len)
          (cons k (list->string (reverse acc)))
          (let ((b (bytevector-u8-ref bv k)))
            (if (or (= b 32) (>= b 128) (= b 27))
              (cons k (list->string (reverse acc)))
              (loop (+ k 1) (cons (integer->char b) acc))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%tokenize-cchar bv i len)
      (let* ((next (bytevector-advance-utf8 bv i len))
             (step (if (= next i) (+ i 1) next))
             (chunk (utf8->string (bytevector-copy bv i step)))
             (cp (utf8->codepoint-at bv i))
             (cw (%codepoint-width cp))
            ) ;
        (list step (list 'cchar cw chunk))
      ) ;let*
    ) ;define

    (define (%tokenize-line line)
      (let* ((bv (string->utf8 line)) (len (bytevector-length bv)))
        (let loop
          ((i 0) (acc '()))
          (cond ((>= i len) (reverse acc))
                ((%match-ansi-csi bv i len)
                 =>
                 (lambda (ansi-end)
                   (let ((chunk (utf8->string (bytevector-copy bv i ansi-end))))
                     (loop ansi-end (cons (list 'ansi 0 chunk) acc))
                   ) ;let
                 ) ;lambda
                ) ;
                ((= (bytevector-u8-ref bv i) 32) (loop (+ i 1) (cons (list 'space 1 " ") acc)))
                ((< (bytevector-u8-ref bv i) 128)
                 (let* ((res (%scan-ascii-word bv i len)) (next-i (car res)) (word-str (cdr res)))
                   (loop next-i (cons (list 'word (string-length word-str) word-str) acc))
                 ) ;let*
                ) ;
                (else
                  (let* ((res (%tokenize-cchar bv i len)) (step (car res)) (tok (cadr res)))
                    (loop step (cons tok acc))
                  ) ;let*
                ) ;else
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (%split-long-word chars cur-line cur-w active-ansi lines width)
      (let loop
        ((rest chars) (cline cur-line) (cw cur-w) (clines lines))
        (if (null? rest)
          (list cline cw clines)
          (let ((c-str (string (car rest))))
            (if (<= (+ cw 1) width)
              (loop (cdr rest) (cons c-str cline) (+ cw 1) clines)
              (let* ((closed (%close-line cline active-ansi))
                     (ninit (if (not (string=? active-ansi "")) (list active-ansi) '()))
                    ) ;
                (loop (cdr rest) (cons c-str ninit) 1 (cons closed clines))
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%new-line-init active-ansi tok-str)
      (cond
       ((and (not (string=? active-ansi "")) (not (string=? tok-str "")))
        (list tok-str active-ansi)
       ) ;
       ((not (string=? active-ansi "")) (list active-ansi))
       ((not (string=? tok-str "")) (list tok-str))
       (else '())
      ) ;cond
    ) ;define

    (define (%handle-word tok-str tok-w cur-line cur-w active-ansi lines width)
      (if (<= (+ cur-w tok-w) width)
        (list (cons tok-str cur-line) (+ cur-w tok-w) lines)
        (if (> tok-w width)
          (%split-long-word (string->list tok-str) cur-line cur-w active-ansi lines width)
          (let ((closed (%close-line cur-line active-ansi))
                (ninit (%new-line-init active-ansi tok-str))
               ) ;
            (list ninit tok-w (cons closed lines))
          ) ;let
        ) ;if
      ) ;if
    ) ;define

    (define (%wrap-single-line line width)
      (let ((tokens (%tokenize-line line)))
        (let loop
          ((rest tokens) (cur-line '()) (cur-w 0) (active-ansi "") (lines '()))
          (cond
           ((null? rest)
            (let ((closed (%close-line cur-line active-ansi)))
              (reverse (cons closed lines))
            ) ;let
           ) ;
           ((eq? (caar rest) 'ansi)
            (let* ((tok (car rest))
                   (tok-str (caddr tok))
                   (next-a (if (%is-sgr-reset? tok-str) "" (string-append active-ansi tok-str)))
                  ) ;
              (loop (cdr rest) (cons tok-str cur-line) cur-w next-a lines)
            ) ;let*
           ) ;
           ((eq? (caar rest) 'space)
            (cond ((and (= cur-w 0) (pair? lines))
                   (loop (cdr rest) cur-line cur-w active-ansi lines)
                  ) ;
                  ((<= (+ cur-w 1) width)
                   (loop (cdr rest) (cons " " cur-line) (+ cur-w 1) active-ansi lines)
                  ) ;
                  (else
                    (let ((closed (%close-line cur-line active-ansi))
                          (ninit (%new-line-init active-ansi ""))
                         ) ;
                      (loop (cdr rest) ninit 0 active-ansi (cons closed lines))
                    ) ;let
                  ) ;else
            ) ;cond
           ) ;
           (else
             (let* ((tok (car rest))
                    (res (%handle-word (caddr tok) (cadr tok) cur-line cur-w active-ansi lines width)
                    ) ;res
                   ) ;
               (loop (cdr rest) (car res) (cadr res) active-ansi (caddr res))
             ) ;let*
           ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (wrap-text str width)
      (unless (string? str)
        (type-error "wrap-text: str must be a string")
      ) ;unless
      (unless (real? width)
        (type-error "wrap-text: width must be a number")
      ) ;unless
      (let ((w (inexact->exact (floor width))))
        (cond ((<= w 0) (list ""))
              ((string=? str "") (list ""))
              (else
                (let ((paragraphs (string-split str "\n")))
                  (append-map (lambda (para) (if (string=? para "") (list "") (%wrap-single-line para w)))
                    paragraphs
                  ) ;append-map
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

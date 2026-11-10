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

(define-library (liii tui fuzzy)
  (export fuzzy-match fuzzy-filter)
  (import (scheme base) (scheme char) (liii string) (liii list) (liii sort))
  (begin

    (define (%word-boundary? text index)
      (or (zero? index)
        (memv (string-ref text (- index 1)) '(#\space #\- #\_ #\. #\/ #\:))
      ) ;or
    ) ;define

    (define (%find-char text ch start)
      (string-index text (lambda (current) (char=? current ch)) start)
    ) ;define

    (define (%match-score-state text i last score consecutive)
      (let* ((is-consecutive (= last (- i 1)))
             (next-consecutive (if is-consecutive (+ consecutive 1) 0))
             (with-consecutive (if is-consecutive (- score (* next-consecutive 5)) score))
             (with-gap
               (if (and (not is-consecutive) (>= last 0))
                 (+ with-consecutive (* (- i last 1) 2))
                 with-consecutive
               ) ;if
             ) ;with-gap
             (with-boundary (if (%word-boundary? text i) (- with-gap 10) with-gap))
            ) ;
        (list (+ with-boundary (* i 0.1)) next-consecutive)
      ) ;let*
    ) ;define

    ;; 返回分数；未匹配返回 #f。分数越低越好。
    (define (%match-query query text)
      (let ((qlen (string-length query)) (tlen (string-length text)))
        (cond ((zero? qlen) 0)
              ((> qlen tlen) #f)
              (else
                (let loop
                  ((qi 0) (last -1) (score 0) (consecutive 0))
                  (if (>= qi qlen)
                    (if (string=? query text) (- score 100) score)
                    (let ((i (%find-char text (string-ref query qi) (+ last 1))))
                      (if (not i)
                        #f
                        (let ((next (%match-score-state text i last score consecutive)))
                          (loop (+ qi 1) i (car next) (cadr next))
                        ) ;let
                      ) ;if
                    ) ;let
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%ascii-letters? ch)
      (char<=? #\a ch #\z)
    ) ;define

    (define (%swap-alphanumeric-query query)
      (let* ((len (string-length query))
             (split (or (string-skip query %ascii-letters?) len))
            ) ;
        (cond
         ((and (> split 0)
            (< split len)
            (every char-numeric? (string->list (substring query split len)))
          ) ;and
          (string-append (substring query split len) (substring query 0 split))
         ) ;
         (else
           (let ((digits (or (string-skip query char-numeric?) len)))
             (if
               (and (> digits 0)
                 (< digits len)
                 (every %ascii-letters? (string->list (substring query digits len)))
               ) ;and
               (string-append (substring query digits len) (substring query 0 digits))
               ""
             ) ;if
           ) ;let
         ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (fuzzy-match query text)
      (let* ((q (string-downcase query))
             (t (string-downcase text))
             (primary (%match-query q t))
            ) ;
        (if primary
          (list (cons 'matches #t) (cons 'score primary))
          (let* ((swapped (%swap-alphanumeric-query q))
                 (swapped-score (and (not (string-null? swapped)) (%match-query swapped t)))
                ) ;
            (if swapped-score
              (list (cons 'matches #t) (cons 'score (+ swapped-score 5)))
              (list (cons 'matches #f) (cons 'score 0))
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%search-tokens query)
      (string-tokenize query (lambda (ch) (or (char-whitespace? ch) (char=? ch #\/))))
    ) ;define

    (define (%match-score query text)
      (let ((result (fuzzy-match query text)))
        (and (cdr (assoc 'matches result)) (cdr (assoc 'score result)))
      ) ;let
    ) ;define

    (define (%score-item tokens text)
      (fold
        (lambda (token total)
          (and total
            (let ((score (%match-score token text)))
              (and score (+ total score))
            ) ;let
          ) ;and
        ) ;lambda
        0
        tokens
      ) ;fold
    ) ;define

    (define (%collect-scored items tokens get-text)
      (filter (lambda (scored) (car scored))
        (map
          (lambda (item) (cons (%score-item tokens (get-text item)) item))
          items
        ) ;map
      ) ;filter
    ) ;define

    (define (fuzzy-filter items query get-text)
      (let ((tokens (%search-tokens (string-trim-both query))))
        (if (null? tokens)
          items
          (map cdr
            (list-sort (lambda (a b) (< (car a) (car b)))
              (%collect-scored items tokens get-text)
            ) ;list-sort
          ) ;map
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

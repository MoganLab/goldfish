;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii golddoc-fuzzy)
  (import (scheme base)
          (liii golddoc-function)
          (liii golddoc-index)
          (liii sort)
          (liii string)
  ) ;import
  (export max-fuzzy-edit-distance
          bounded-levenshtein-distance
          suggest-candidates
          suggest-library-functions
          suggest-visible-functions
  ) ;export
  (begin

    (define max-fuzzy-edit-distance 2)

    (define (unique-strings strings)
      (let loop ((remaining strings)
                 (result '()))
        (if (null? remaining)
            (reverse result)
            (let ((value (car remaining)))
              (loop (cdr remaining)
                    (if (and (string? value)
                             (not (member value result)))
                        (cons value result)
                        result
                    ) ;if
              ) ;loop
            ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (initialize-distance-row! row right-length)
      (let loop ((index 0))
        (if (> index right-length)
            row
            (begin
              (vector-set! row index index)
              (loop (+ index 1))
            ) ;begin
        ) ;if
      ) ;let
    ) ;define

    (define (distance-cost left right left-index right-index)
      (if (char=? (string-ref left (- left-index 1))
                  (string-ref right (- right-index 1)))
          0
          1
      ) ;if
    ) ;define

    (define (fill-distance-row! left right left-index right-length prev-row curr-row)
      (vector-set! curr-row 0 left-index)
      (let column-loop ((right-index 1)
                        (row-min left-index))
        (if (> right-index right-length)
            row-min
            (let* ((cost (distance-cost left right left-index right-index))
                   (deletion (+ (vector-ref prev-row right-index) 1))
                   (insertion (+ (vector-ref curr-row (- right-index 1)) 1))
                   (substitution (+ (vector-ref prev-row (- right-index 1)) cost))
                   (distance (min deletion insertion substitution))
                   (next-min (min row-min distance)))
              (vector-set! curr-row right-index distance)
              (column-loop (+ right-index 1) next-min)
            ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (bounded-levenshtein-distance left right)
      (let* ((left-length (string-length left))
             (right-length (string-length right))
             (length-gap (abs (- left-length right-length))))
        (if (> length-gap max-fuzzy-edit-distance)
            #f
            (let ((prev (make-vector (+ right-length 1) 0))
                  (curr (make-vector (+ right-length 1) 0)))
              (initialize-distance-row! prev right-length)
              (let row-loop ((left-index 1)
                             (prev-row prev)
                             (curr-row curr))
                (if (> left-index left-length)
                    (let ((distance (vector-ref prev-row right-length)))
                      (and (<= distance max-fuzzy-edit-distance)
                           distance
                      ) ;and
                    ) ;let
                    (let ((row-min (fill-distance-row! left
                                                       right
                                                       left-index
                                                       right-length
                                                       prev-row
                                                       curr-row)))
                      (and (<= row-min max-fuzzy-edit-distance)
                           (row-loop (+ left-index 1) curr-row prev-row)
                      ) ;and
                    ) ;let
                ) ;if
              ) ;let
            ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (edit-distance-matches query candidates)
      (let loop ((remaining (unique-strings candidates))
                 (matches '()))
        (if (null? remaining)
            (map car
                 (list-sort (lambda (left right)
                              (if (= (cdr left) (cdr right))
                                  (string<? (car left) (car right))
                                  (< (cdr left) (cdr right))
                              ) ;if
                            ) ;lambda
                            matches)
            ) ;map
            (let* ((candidate (car remaining))
                   (distance (and (not (string=? candidate query))
                                  (bounded-levenshtein-distance query candidate))))
              (loop (cdr remaining)
                    (if distance
                        (cons (cons candidate distance) matches)
                        matches
                    ) ;if
              ) ;loop
            ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (prefix-matches query candidates)
      (let loop ((remaining (unique-strings candidates))
                 (matches '()))
        (if (null? remaining)
            (list-sort string<? matches)
            (let ((candidate (car remaining)))
              (loop (cdr remaining)
                    (if (and (not (string=? candidate query))
                             (string-starts? candidate query))
                        (cons candidate matches)
                        matches
                    ) ;if
              ) ;loop
            ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (suggest-candidates query candidates)
      (let ((prefixes (prefix-matches query candidates)))
        (if (null? prefixes)
            (edit-distance-matches query candidates)
            prefixes
        ) ;if
      ) ;let
    ) ;define

    (define (suggest-library-functions library-query function-name)
      (suggest-candidates function-name
                          (library-documented-functions library-query))
    ) ;define

    (define (suggest-visible-functions function-name)
      (suggest-candidates function-name
                          (visible-function-names))
    ) ;define

  ) ;begin
) ;define-library

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

(define-library (liii cmark inline emph-parser)
  (import (scheme base))
  (import (srfi srfi-1))
  (import (liii cmark text))
  (import (liii cmark inline util))
  (import (liii cmark inline emph-delimiters))
  (import (liii cmark node))
  (export parse-em-or-strong-with)
  (begin

    (define (make-emph-or-strong segs use refmap parse-inlines-fn)
      (let ((node (if (= use 2) (make-cmark-strong) (make-cmark-emph))))
        (let add
          ((s segs) (buf "") (children '()))
          (cond
           ((null? s)
            (for-each (lambda (c) (cmark-node-append-child! node c))
              (reverse
                (if (string=? buf "")
                  children
                  (fold (lambda (n acc) (append-inline-node acc n))
                    children
                    (parse-inlines-fn buf refmap)
                  ) ;fold
                ) ;if
              ) ;reverse
            ) ;for-each
           ) ;
           ((string? (caddr (car s)))
            (add (cdr s) (string-append buf (caddr (car s))) children)
           ) ;
           (else
             (let* ((parsed (parse-inlines-fn buf refmap))
                    (acc2 (fold (lambda (n a) (append-inline-node a n)) children parsed))
                   ) ;
               (add (cdr s) "" (cons (caddr (car s)) acc2))
             ) ;let*
           ) ;else
          ) ;cond
        ) ;let
        node
      ) ;let
    ) ;define

    (define (skip-segs-before segs op-pos)
      (let skip
        ((s0 segs) (before '()))
        (if
          (or (null? s0) (> (cadr (car s0)) op-pos))
          (cons s0 before)
          (skip (cdr s0) (cons (car s0) before))
        ) ;if
      ) ;let
    ) ;define

    (define (place-emph-node acc-rev node-placed? op-end cl-pos children use refmap parse-inlines-fn)
      (if node-placed?
        acc-rev
        (let ((emph-node (make-emph-or-strong (reverse children) use refmap parse-inlines-fn)))
          (cons (list op-end cl-pos emph-node) acc-rev)
        ) ;let
      ) ;if
    ) ;define

    (define (apply-emph-pair str delims-v segs used-o used-c pair refmap parse-inlines-fn)
      (let* ((oi (car pair))
             (ci (cadr pair))
             (use (caddr pair))
             (op-rec (vector-ref delims-v oi))
             (cl-rec (vector-ref delims-v ci))
             (op-pos (car op-rec))
             (op-len0 (cadr op-rec))
             (cl-pos (car cl-rec))
             (cl-len0 (cadr cl-rec))
             (op-end (+ op-pos op-len0))
             (op-used (+ (vector-ref used-o oi) use))
             (cl-used (+ (vector-ref used-c ci) use))
             (op-rem (- op-len0 op-used))
             (cl-rem (- cl-len0 cl-used))
             (skip-info (skip-segs-before segs op-pos))
             (walk-segs (car skip-info))
             (before-rev (cdr skip-info))
            ) ;
        (vector-set! used-o oi op-used)
        (vector-set! used-c ci cl-used)
        (let walk
          ((s walk-segs) (acc-rev before-rev) (children '()) (node-placed? #f))
          (cond
           ((null? s)
            (reverse
              (if node-placed?
                acc-rev
                (let ((emph-node (make-emph-or-strong (reverse children) use refmap parse-inlines-fn))
                     ) ;
                  (cons (list op-end cl-pos emph-node) acc-rev)
                ) ;let
              ) ;if
            ) ;reverse
           ) ;
           ((and (<= (car (car s)) op-pos) (< op-pos (cadr (car s))))
            (walk (cdr s)
              (if (> op-rem 0)
                (cons (list op-pos (+ op-pos op-rem) (substring str op-pos (+ op-pos op-rem)))
                  acc-rev
                ) ;cons
                acc-rev
              ) ;if
              children
              node-placed?
            ) ;walk
           ) ;
           ((and (<= (car (car s)) cl-pos) (< cl-pos (cadr (car s))))
            (let ((acc1
                    (place-emph-node acc-rev node-placed? op-end cl-pos children use refmap parse-inlines-fn)
                  ) ;acc1
                 ) ;
              (walk (cdr s)
                (if (> cl-rem 0)
                  (cons (list cl-pos (+ cl-pos cl-rem) (substring str cl-pos (+ cl-pos cl-rem)))
                    acc1
                  ) ;cons
                  acc1
                ) ;if
                children
                #t
              ) ;walk
            ) ;let
           ) ;
           ((and (>= (car (car s)) op-end) (<= (cadr (car s)) cl-pos))
            (walk (cdr s) acc-rev (cons (car s) children) node-placed?)
           ) ;
           ((> (car (car s)) cl-pos) (append (reverse acc-rev) s))
           (else (walk (cdr s) (cons (car s) acc-rev) children node-placed?))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (fold-node-pairs str delims-v pairs inner used-o used-c refmap parse-inlines-fn)
      (let fold-pairs
        ((pl pairs) (cur inner))
        (if (null? pl)
          cur
          (fold-pairs (cdr pl)
            (apply-emph-pair str delims-v cur used-o used-c (car pl) refmap parse-inlines-fn)
          ) ;fold-pairs
        ) ;if
      ) ;let
    ) ;define

    (define (fold-emph-forest str delims-v roots segs used-o used-c refmap parse-inlines-fn)
      (let loop
        ((rs roots) (segs segs) (acc-rev '()))
        (if (null? rs)
          (reverse (prepend-rev segs acc-rev))
          (let* ((r0 (car rs))
                 (op (node-op r0))
                 (cl (node-cl r0))
                 (children (node-children r0))
                 (pairs (node-pairs r0))
                 (front+tail (split-segs-at segs op))
                 (front (car front+tail))
                 (tail (cdr front+tail))
                 (inner (if (null? children)
                          tail
                          (fold-emph-forest str delims-v children tail used-o
                            used-c refmap parse-inlines-fn
                          ) ;fold-emph-forest
                        ) ;if
                 ) ;inner
                 (folded
                   (fold-node-pairs str delims-v pairs inner used-o used-c refmap parse-inlines-fn)
                 ) ;folded
                 (part+rest (split-segs-at folded cl))
                 (part (car part+rest))
                 (rest (cdr part+rest))
                 (acc2 (prepend-rev part (prepend-rev front acc-rev)))
                ) ;
            (loop (cdr rs) rest acc2)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (render-emph-segs segs refmap parse-inlines-fn)
      (let loop
        ((s segs) (acc '()) (buf ""))
        (cond
         ((null? s)
          (reverse
            (if (string=? buf "")
              acc
              (fold (lambda (n a) (append-inline-node a n)) acc (parse-inlines-fn buf refmap))
            ) ;if
          ) ;reverse
         ) ;
         ((string? (caddr (car s)))
          (loop (cdr s) acc (string-append buf (caddr (car s))))
         ) ;
         (else
           (let* ((parsed (parse-inlines-fn buf refmap))
                  (acc2 (fold (lambda (n a) (append-inline-node a n)) acc parsed))
                 ) ;
             (loop (cdr s) (cons (caddr (car s)) acc2) "")
           ) ;let*
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (keep-relevant-pairs pairs delims-v lo hi)
      (let keep
        ((pl pairs) (acc '()))
        (if (null? pl)
          (reverse acc)
          (let* ((p (car pl))
                 (op-pos (car (vector-ref delims-v (car p))))
                 (cl-pos (car (vector-ref delims-v (cadr p))))
                ) ;
            (if (and (>= op-pos lo) (<= cl-pos hi))
              (keep (cdr pl) (cons p acc))
              (keep (cdr pl) acc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (parse-em-or-strong-with str start refmap parse-inlines-fn
              parse-link-fn parse-image-fn
            ) ;parse-em-or-strong-with
      (let* ((len (string-length str))
             (delims (scan-emph-delimiters str start refmap parse-link-fn parse-image-fn))
             (n-delims (length delims))
             (delims-v (list->vector delims))
             (pairs (collect-emph-pairs delims delims-v n-delims))
             (used-o (make-vector n-delims 0))
             (used-c (make-vector n-delims 0))
             (relevant (keep-relevant-pairs pairs delims-v start len))
             (final-segs (fold-emph-forest str
                           delims-v
                           (build-emph-forest relevant delims-v)
                           (initial-emph-segs str delims-v n-delims start len)
                           used-o
                           used-c
                           refmap
                           parse-inlines-fn
                         ) ;fold-emph-forest
             ) ;final-segs
            ) ;
        (if
          (or (= n-delims 0)
            (not (= (car (vector-ref delims-v 0)) start))
            (and (= (vector-ref used-o 0) 0) (= (vector-ref used-c 0) 0))
          ) ;or
          (cons
            (list (make-cmark-text (substring str start (+ start 1))))
            (+ start 1)
          ) ;cons
          (cons (render-emph-segs final-segs refmap parse-inlines-fn) len)
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

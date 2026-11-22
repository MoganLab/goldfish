;;
;; Copyright (C) 2026 Liii Network
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

(define-library (liii cmark inline emph-delimiters)
  (import (scheme base))
  (import (liii cmark text))
  (import (liii cmark inline emphasis))
  (import (liii cmark inline link))
  (import (liii cmark node))
  (export emph-marker-at? scan-emph-delimiters collect-emph-pairs
    initial-emph-segs split-segs-at node-op node-cl node-children node-pairs
    build-emph-forest
  ) ;export
  (begin

    (define (emph-marker-at? str pos)
      (and (< pos (string-length str))
        (or (char=? (string-ref str pos) #\*) (char=? (string-ref str pos) #\_))
      ) ;and
    ) ;define

    (define (scan-emph-delimiters str start refmap parse-link-fn parse-image-fn)
      (let ((len (string-length str)))
        (let scan
          ((pos start) (acc '()))
          (cond ((>= pos len) (reverse acc))
                ((and (emph-marker-at? str pos) (not (backslash-escaped-at? str pos)))
                 (let* ((run-len (marker-run-len str pos))
                        (ch (string-ref str pos))
                        (underscore? (char=? ch #\_))
                        (intraword? (and underscore? (intraword-run? str pos run-len)))
                        (left (left-flanking? str pos run-len))
                        (right (right-flanking? str pos run-len))
                        (can-open (and left (not intraword?)))
                        (can-close (and right (not intraword?)))
                        (delim (list pos run-len ch can-open can-close))
                       ) ;
                   (if (or can-open can-close)
                     (scan (+ pos run-len) (cons delim acc))
                     (scan (+ pos run-len) acc)
                   ) ;if
                 ) ;let*
                ) ;
                ((code-span-end str pos) (scan (code-span-end str pos) acc))
                ((skip-html-or-autolink str pos) (scan (skip-html-or-autolink str pos) acc))
                ((and (< pos len) (char=? (string-ref str pos) #\!))
                 (let ((img
                         (and (< (+ pos 1) len)
                           (char=? (string-ref str (+ pos 1)) #\[)
                           (parse-image-fn str pos refmap)
                         ) ;and
                       ) ;img
                      ) ;
                   (if
                     (and img (eq? (cmark-node-type (car img)) 'image))
                     (scan (cdr img) acc)
                     (scan (+ pos 1) acc)
                   ) ;if
                 ) ;let
                ) ;
                ((and (< pos len) (char=? (string-ref str pos) #\[))
                 (let ((lnk (parse-link-fn str pos refmap)))
                   (if
                     (and lnk (eq? (cmark-node-type (car lnk)) 'link))
                     (scan (cdr lnk) acc)
                     (scan (+ pos 1) acc)
                   ) ;if
                 ) ;let
                ) ;
                (else (scan (+ pos 1) acc))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (match-cond? delims-v can-open-v can-close-v oi ci)
      (let ((c-open (vector-ref can-open-v ci))
            (o-close (vector-ref can-close-v oi))
            (o-len (cadr (vector-ref delims-v oi)))
            (c-len (cadr (vector-ref delims-v ci)))
           ) ;
        (not
          (and (or c-open o-close)
            (not (= (modulo c-len 3) 0))
            (= (modulo (+ o-len c-len) 3) 0)
          ) ;and
        ) ;not
      ) ;let
    ) ;define

    (define (kill-between! alive oi ci)
      (let kill
        ((k (+ oi 1)))
        (if (< k ci) (begin (vector-set! alive k #f) (kill (+ k 1))) '())
      ) ;let
    ) ;define

    (define (consume-emph-pair! lens alive oi ci)
      (let* ((oi-len (vector-ref lens oi))
             (ci-len (vector-ref lens ci))
             (use (if (and (>= oi-len 2) (>= ci-len 2)) 2 1))
            ) ;
        (vector-set! lens oi (- oi-len use))
        (vector-set! lens ci (- ci-len use))
        (kill-between! alive oi ci)
        (if (= (vector-ref lens ci) 0) (vector-set! alive ci #f))
        (if (= (vector-ref lens oi) 0) (vector-set! alive oi #f))
        use
      ) ;let*
    ) ;define

    (define (collect-emph-pairs delims delims-v n-delims)
      (let ((lens (list->vector (map cadr delims)))
            (can-open-v (list->vector (map cadddr delims)))
            (can-close-v
              (list->vector
                (map (lambda (d) (car (cddddr d))) delims)
              ) ;list->vector
            ) ;can-close-v
            (char-v (list->vector (map caddr delims)))
            (alive (make-vector n-delims #t))
           ) ;
        (let collect
          ((ci 0) (acc '()))
          (cond ((>= ci n-delims) (reverse acc))
                ((and (vector-ref alive ci) (vector-ref can-close-v ci))
                 (let find-op
                   ((oi (- ci 1)))
                   (cond
                    ((< oi 0)
                     (if (not (vector-ref can-open-v ci)) (vector-set! alive ci #f))
                     (collect (+ ci 1) acc)
                    ) ;
                    ((and (vector-ref alive oi)
                       (vector-ref can-open-v oi)
                       (char=? (vector-ref char-v oi) (vector-ref char-v ci))
                       (match-cond? delims-v can-open-v can-close-v oi ci)
                     ) ;and
                     (let ((use (consume-emph-pair! lens alive oi ci)))
                       (if (vector-ref alive ci)
                         (collect ci (cons (list oi ci use) acc))
                         (collect (+ ci 1) (cons (list oi ci use) acc))
                       ) ;if
                     ) ;let
                    ) ;
                    (else (find-op (- oi 1)))
                   ) ;cond
                 ) ;let
                ) ;
                (else (collect (+ ci 1) acc))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (initial-emph-segs str delims-v n-delims lo hi)
      (let loop
        ((di 0) (cur lo) (acc '()))
        (if (>= di n-delims)
          (reverse
            (if (< cur hi) (cons (list cur hi (substring str cur hi)) acc) acc)
          ) ;reverse
          (let* ((d (vector-ref delims-v di))
                 (d-pos (car d))
                 (d-len (cadr d))
                 (d-end (+ d-pos d-len))
                ) ;
            (if (< d-pos cur)
              (loop (+ di 1) cur acc)
              (loop (+ di 1)
                d-end
                (cons (list d-pos d-end (substring str d-pos d-end))
                  (if (> d-pos cur) (cons (list cur d-pos (substring str cur d-pos)) acc) acc)
                ) ;cons
              ) ;loop
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (split-segs-at segs pos)
      (let loop
        ((s segs) (before '()))
        (if
          (or (null? s) (> (cadr (car s)) pos))
          (cons (reverse before) s)
          (loop (cdr s) (cons (car s) before))
        ) ;if
      ) ;let
    ) ;define

    (define (node-op n)
      (car n)
    ) ;define
    (define (node-cl n)
      (cadr n)
    ) ;define
    (define (node-children n)
      (caddr n)
    ) ;define
    (define (node-pairs n)
      (cadddr n)
    ) ;define
    (define (add-child! n c)
      (set-car! (cddr n) (cons c (caddr n)))
    ) ;define
    (define (add-pair! n p)
      (set-car! (cdddr n) (cons p (cadddr n)))
    ) ;define
    (define (make-forest-node op cl p)
      (list op cl '() (list p))
    ) ;define
    (define (finalize-node n)
      (list (car n) (cadr n) (reverse (caddr n)) (reverse (cadddr n)))
    ) ;define

    (define (make-tag< delims-v)
      (lambda (a b)
        (let ((oa
                (car (vector-ref delims-v (car (car a))))
              ) ;oa
              (ob
                (car (vector-ref delims-v (car (car b))))
              ) ;ob
              (ca
                (car (vector-ref delims-v (cadr (car a))))
              ) ;ca
              (cb
                (car (vector-ref delims-v (cadr (car b))))
              ) ;cb
             ) ;
          (cond ((< oa ob) #t)
                ((> oa ob) #f)
                ((> ca cb) #t)
                ((< ca cb) #f)
                (else (< (cdr a) (cdr b)))
          ) ;cond
        ) ;let
      ) ;lambda
    ) ;define

    (define (tag-pairs pl)
      (let tag
        ((ps pl) (idx 0) (acc '()))
        (if (null? ps)
          (reverse acc)
          (tag (cdr ps) (+ idx 1) (cons (cons (car ps) idx) acc))
        ) ;if
      ) ;let
    ) ;define

    (define (pop-closed-nodes stack roots-rev op)
      (let pop
        ((st stack) (acc-rev roots-rev))
        (if
          (or (null? st) (>= (node-cl (car st)) op))
          (cons st acc-rev)
          (pop (cdr st) (cons (finalize-node (car st)) acc-rev))
        ) ;if
      ) ;let
    ) ;define

    (define (drain-forest-stack stack roots-rev)
      (let drain
        ((st stack) (acc-rev roots-rev))
        (if (null? st)
          (reverse acc-rev)
          (drain (cdr st) (cons (finalize-node (car st)) acc-rev))
        ) ;if
      ) ;let
    ) ;define

    (define (build-emph-forest pl delims-v)
      (let ((op-asc (stable-sort (tag-pairs pl) (make-tag< delims-v))))
        (let loop
          ((ps op-asc) (stack '()) (roots-rev '()))
          (if (null? ps)
            (drain-forest-stack stack roots-rev)
            (let* ((p (car (car ps)))
                   (op (car (vector-ref delims-v (car p))))
                   (cl (car (vector-ref delims-v (cadr p))))
                   (topo (pop-closed-nodes stack roots-rev op))
                   (stack2 (car topo))
                   (roots1 (cdr topo))
                  ) ;
              (cond
               ((null? stack2) (loop (cdr ps) (cons (make-forest-node op cl p) stack2) roots1))
               ((and (= (node-op (car stack2)) op) (= (node-cl (car stack2)) cl))
                (add-pair! (car stack2) p)
                (loop (cdr ps) stack2 roots1)
               ) ;
               (else (add-child! (car stack2) (make-forest-node op cl p))
                 (loop (cdr ps) stack2 roots1)
               ) ;else
              ) ;cond
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core skills frontmatter)
  (export parse-frontmatter strip-frontmatter)
  (import (scheme base) (scheme char) (liii string) (liii list))
  (begin

    (define (%normalize-newlines str)
      (let* ((s1 (string-replace str "\r\n" "\n")) (s2 (string-replace s1 "\r" "\n")))
        s2
      ) ;let*
    ) ;define

    (define (%strip-quotes str)
      (let ((t (string-trim-both str)))
        (if
          (and (>= (string-length t) 2)
            (or
              (and (char=? (string-ref t 0) #\")
                (char=? (string-ref t (- (string-length t) 1)) #\")
              ) ;and
              (and (char=? (string-ref t 0) #\')
                (char=? (string-ref t (- (string-length t) 1)) #\')
              ) ;and
            ) ;or
          ) ;and
          (substring t 1 (- (string-length t) 1))
          t
        ) ;if
      ) ;let
    ) ;define
    (define (%convert-yaml-val key val-str)
      (cond
       ((string=? key "disable-model-invocation")
        (cond ((string-ci=? val-str "true") #t)
              ((string-ci=? val-str "false") #f)
              (else #f)
        ) ;cond
       ) ;
       (else (%strip-quotes val-str))
      ) ;cond
    ) ;define
    (define (%indicator? v)
      (or (string=? v "|")
        (string=? v ">")
        (string=? v ">-")
        (string=? v "|-")
        (string-null? v)
      ) ;or
    ) ;define
    (define (%indent? line)
      (and (> (string-length line) 0)
        (let ((c (string-ref line 0)))
          (or (char=? c #\space) (char=? c #\tab))
        ) ;let
      ) ;and
    ) ;define
    (define (%parse-kv line)
      (let ((pos (string-position ":" line)))
        (and pos
          (cons (string-trim-both (substring line 0 pos))
            (string-trim-both (substring line (+ pos 1)))
          ) ;cons
        ) ;and
      ) ;let
    ) ;define
    (define (%add-entry alist key val-lines)
      (if (not key)
        alist
        (let* ((raw (if (null? val-lines) "" (string-join (reverse val-lines) "\n")))
               (val (%convert-yaml-val key (string-trim-both raw)))
              ) ;
          (cons (cons key val) alist)
        ) ;let*
      ) ;if
    ) ;define
    (define (%parse-yaml-lines yaml-str)
      (let loop
        ((lines (string-split yaml-str "\n")) (cur-key #f) (cur-vals '()) (alist '()))
        (if (null? lines)
          (%add-entry alist cur-key cur-vals)
          (let* ((raw (car lines))
                 (line (string-trim-right raw))
                 (trimmed (string-trim-both line))
                ) ;
            (cond ((or (string-null? trimmed) (string-starts? trimmed "#"))
                   (loop (cdr lines) cur-key cur-vals alist)
                  ) ;
                  ((%indent? line)
                   (loop (cdr lines) cur-key (if cur-key (cons trimmed cur-vals) cur-vals) alist)
                  ) ;
                  (else
                    (let ((kv (%parse-kv line)))
                      (if kv
                        (let* ((new-alist (%add-entry alist cur-key cur-vals))
                               (k (car kv))
                               (v (cdr kv))
                               (vals (if (%indicator? v) '() (list v)))
                              ) ;
                          (loop (cdr lines) k vals new-alist)
                        ) ;let*
                        (loop (cdr lines) cur-key cur-vals alist)
                      ) ;if
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
    (define (%extract-yaml-and-body norm)
      (let ((close-idx (string-position "\n---" norm 3)))
        (if (not close-idx)
          (values '() norm)
          (let* ((yaml-start (if (string-starts? (substring norm 3) "\n") 4 3))
                 (yaml-str (substring norm yaml-start close-idx))
                 (body (string-trim-both (substring norm (+ close-idx 4))))
                ) ;
            (values (%parse-yaml-lines yaml-str) body)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
    (define (parse-frontmatter content)
      (let ((norm (%normalize-newlines content)))
        (if (not (string-starts? norm "---"))
          (values '() norm)
          (%extract-yaml-and-body norm)
        ) ;if
      ) ;let
    ) ;define
    (define (strip-frontmatter content)
      (let-values (((fm body) (parse-frontmatter content)))
        body
      ) ;let-values
    ) ;define
  ) ;begin
) ;define-library

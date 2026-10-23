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

(define-library (liii goldcheck path)
  (import (scheme base)
    (liii base)
    (liii path)
    (liii string)
    (liii json)
    (liii error)
  ) ;import
  (export normalize-sep has-wildcard? string-index-of string-starts-with?
    glob-match? path-matches-exclude? exclude-entry->path file-excluded?
    relativize-path
  ) ;export
  (begin

    (define (normalize-sep s)
      (let* ((s1 (string-replace s "\\" "/"))
             (s2
               (if (and (>= (string-length s1) 2) (string=? (substring s1 0 2) "./"))
                 (substring s1 2 (string-length s1))
                 s1
               ) ;if
             ) ;s2
            ) ;
        s2
      ) ;let*
    ) ;define

    (define (relativize-path path-str)
      (let ((root (g_project-root)))
        (if (and root (string? root) (not (string=? root "")))
          (let* ((norm-root (normalize-sep root))
                 (norm-path (normalize-sep path-str))
                 (prefix (if (string-ends? norm-root "/") norm-root (string-append norm-root "/"))
                 ) ;prefix
                ) ;
            (if (string-starts-with? norm-path prefix)
              (substring norm-path (string-length prefix) (string-length norm-path))
              norm-path
            ) ;if
          ) ;let*
          (normalize-sep path-str)
        ) ;if
      ) ;let
    ) ;define

    (define (has-wildcard? s)
      (let loop
        ((i (- (string-length s) 1)))
        (if (< i 0) #f (if (char=? (string-ref s i) #\*) #t (loop (- i 1))))
      ) ;let
    ) ;define

    (define (string-index-of str sub start)
      (let ((sublen (string-length sub)) (slen (string-length str)))
        (let loop
          ((i start))
          (cond ((> (+ i sublen) slen) #f)
                ((string=? (substring str i (+ i sublen)) sub) i)
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (string-starts-with? str prefix)
      (and (>= (string-length str) (string-length prefix))
        (string=? (substring str 0 (string-length prefix)) prefix)
      ) ;and
    ) ;define

    (define (match-segments ps str pos first?)
      (if (null? ps)
        pos
        (let ((seg (car ps)))
          (cond ((string=? seg "") (match-segments (cdr ps) str pos #f))
                (first? (and (string-starts-with? str seg)
                          (match-segments (cdr ps) str (string-length seg) #f)
                        ) ;and
                ) ;first?
                (else
                  (let ((found (string-index-of str seg pos)))
                    (and found (match-segments (cdr ps) str (+ found (string-length seg)) #f))
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (glob-match? pattern str)
      (if (not (has-wildcard? pattern))
        (string=? pattern str)
        (let* ((parts (string-split pattern "*"))
               (ends-with-star (string-ends? pattern "*"))
               (res (match-segments parts str 0 #t))
              ) ;
          (and res (or ends-with-star (= res (string-length str))))
        ) ;let*
      ) ;if
    ) ;define

    (define (exact-or-subpath? entry-norm p)
      (let ((plen (string-length p)) (elen (string-length entry-norm)))
        (or (string=? entry-norm p)
          (string-starts-with? entry-norm (string-append p "/"))
          (string-contains? entry-norm (string-append "/" p "/"))
          (and (>= elen (+ plen 1))
            (char=? (string-ref entry-norm (- elen plen 1)) #\/)
            (string-ends? entry-norm p)
          ) ;and
        ) ;or
      ) ;let
    ) ;define

    (define (path-matches-exclude? entry-str pattern)
      (let ((entry-norm (relativize-path entry-str)) (p (normalize-sep pattern)))
        (cond
         ((has-wildcard? p)
          (if (string-contains? p "/")
            (glob-match? p entry-norm)
            (or (glob-match? p (path-name (path entry-norm))) (glob-match? p entry-norm))
          ) ;if
         ) ;
         ((string-ends? p "/")
          (or (string-starts-with? entry-norm p)
            (string-contains? entry-norm (string-append "/" p))
          ) ;or
         ) ;
         (else (exact-or-subpath? entry-norm p))
        ) ;cond
      ) ;let
    ) ;define

    (define (find-path-in-alist entry)
      (let ((pair (or (assoc "path" entry) (assq 'path entry))))
        (and pair (string? (cdr pair)) (cdr pair))
      ) ;let
    ) ;define

    (define (lookup-entry-path entry)
      (let ((v (json-ref entry "path")))
        (if (string? v) v (and (pair? entry) (find-path-in-alist entry)))
      ) ;let
    ) ;define

    (define (exclude-entry->path entry)
      (cond ((string? entry) entry)
            ((json-object? entry)
             (let ((p (lookup-entry-path entry)))
               (if (string? p) p (type-error "exclude entry path must be a string: ~S" entry))
             ) ;let
            ) ;
            (else (type-error "exclude entry must be a string or object: ~S" entry))
      ) ;cond
    ) ;define

    (define (file-excluded? entry-str excludes)
      (let loop
        ((pats excludes))
        (if (null? pats)
          #f
          (let ((pat (if (string? (car pats)) (car pats) (exclude-entry->path (car pats)))))
            (if (path-matches-exclude? entry-str pat) #t (loop (cdr pats)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

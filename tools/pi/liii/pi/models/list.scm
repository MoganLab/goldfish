;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi models list)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii sort)
    (liii pi models search)
  ) ;import
  (export pi-format-token-count pi-list-models pi-format-models-table
    pi-fuzzy-match-model? pi-split-search-tokens
  ) ;export
  (begin

    (define (%dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    (define (%round-1dec-str val)
      (let* ((scaled
               (inexact->exact (floor (+ (* val 10.0) 0.5)))
             ) ;scaled
             (int-part (quotient scaled 10))
             (dec-part (modulo scaled 10))
            ) ;
        (string-append (number->string int-part) "." (number->string dec-part))
      ) ;let*
    ) ;define

    (define (pi-format-token-count count)
      (cond
       ((>= count 1000000)
        (if (= (modulo count 1000000) 0)
          (string-append (number->string (quotient count 1000000)) "M")
          (string-append (%round-1dec-str (/ count 1000000.0)) "M")
        ) ;if
       ) ;
       ((>= count 1000)
        (if (= (modulo count 1000) 0)
          (string-append (number->string (quotient count 1000)) "K")
          (string-append (%round-1dec-str (/ count 1000.0)) "K")
        ) ;if
       ) ;
       (else (number->string count))
      ) ;cond
    ) ;define

    (define (%has-image? input)
      (cond
       ((vector? input)
        (let loop
          ((i 0))
          (and (< i (vector-length input))
            (or (equal? (vector-ref input i) "image") (loop (+ i 1)))
          ) ;and
        ) ;let
       ) ;
       ((list? input) (and (member "image" input) #t))
       (else #f)
      ) ;cond
    ) ;define

    (define (%pad-right str width)
      (let ((len (string-length str)))
        (if (>= len width) str (string-append str (make-string (- width len) #\space)))
      ) ;let
    ) ;define

    (define (%build-model-row p-name m)
      (let* ((mid (%dict-ref m "id" ""))
             (cw (%dict-ref m "contextWindow" 128000))
             (mt (%dict-ref m "maxTokens" 8192))
             (reasoning (%dict-ref m "reasoning" #f))
             (input (%dict-ref m "input" '()))
            ) ;
        (list (cons 'provider p-name)
          (cons 'model mid)
          (cons 'context (pi-format-token-count cw))
          (cons 'max-out (pi-format-token-count mt))
          (cons 'thinking (if reasoning "yes" "no"))
          (cons 'images (if (%has-image? input) "yes" "no"))
        ) ;list
      ) ;let*
    ) ;define

    (define (%collect-provider-rows p-entry)
      (let* ((p-name (if (string? (car p-entry)) (car p-entry) (symbol->string (car p-entry)))
             ) ;p-name
             (raw-models (%dict-ref (cdr p-entry) "models" '()))
             (m-list (cond ((vector? raw-models) (vector->list raw-models))
                           ((list? raw-models) raw-models)
                           (else '())
                     ) ;cond
             ) ;m-list
            ) ;
        (map (lambda (m) (%build-model-row p-name m)) m-list)
      ) ;let*
    ) ;define

    (define (%row-less-p a b)
      (let ((pa (cdr (assoc 'provider a))) (pb (cdr (assoc 'provider b))))
        (if (string=? pa pb)
          (string<? (cdr (assoc 'model a)) (cdr (assoc 'model b)))
          (string<? pa pb)
        ) ;if
      ) ;let
    ) ;define

    (define (%row-match-search? item search-pattern)
      (let ((target
              (string-downcase (string-append (cdr (assoc 'provider item)) " " (cdr (assoc 'model item)))
              ) ;string-downcase
            ) ;target
           ) ;
        (pi-fuzzy-match-model? search-pattern target)
      ) ;let
    ) ;define

    (define (%filter-search sorted search-pattern)
      (if (and (string? search-pattern) (not (string-null? search-pattern)))
        (filter (lambda (item) (%row-match-search? item search-pattern)) sorted)
        sorted
      ) ;if
    ) ;define

    (define (pi-list-models models-store . opt-args)
      (let* ((provider-opt (if (pair? opt-args) (car opt-args) #f))
             (search-pattern
               (if (and (pair? opt-args) (pair? (cdr opt-args))) (cadr opt-args) #f)
             ) ;search-pattern
             (p-filter (and (string? provider-opt)
                         (not (string-null? provider-opt))
                         (not (string=? provider-opt "all"))
                         provider-opt
                       ) ;and
             ) ;p-filter
            ) ;
        (if
          (and p-filter
            (not (assoc p-filter models-store))
            (not (assoc (string->symbol p-filter) models-store))
          ) ;and
          (error 'runtime-error
            (string-append "Unknown provider \""
              p-filter
              "\". Use --list-models to see available providers/models."
            ) ;string-append
          ) ;error
        ) ;if
        (let* ((p-list
                 (if p-filter
                   (list (or (assoc p-filter models-store)
                           (assoc (string->symbol p-filter) models-store)
                         ) ;or
                   ) ;list
                   models-store
                 ) ;if
               ) ;p-list
               (all-rows (apply append (map %collect-provider-rows p-list)))
               (sorted (list-sort %row-less-p all-rows))
              ) ;
          (%filter-search sorted search-pattern)
        ) ;let*
      ) ;let*
    ) ;define

    (define (%row-val key row)
      (cdr (assoc key row))
    ) ;define

    (define (%max-col-width key header rows)
      (apply max
        (string-length header)
        (map (lambda (r) (string-length (%row-val key r))) rows)
      ) ;apply
    ) ;define

    (define (%format-table-line p m c mo th im w-p w-m w-c w-mo w-th w-im)
      (string-append (%pad-right p w-p)
        "  "
        (%pad-right m w-m)
        "  "
        (%pad-right c w-c)
        "  "
        (%pad-right mo w-mo)
        "  "
        (%pad-right th w-th)
        "  "
        (%pad-right im w-im)
        "\n"
      ) ;string-append
    ) ;define

    (define (pi-format-models-table rows . opt-search)
      (let ((search-pattern (if (pair? opt-search) (car opt-search) #f)))
        (if (null? rows)
          (if (and (string? search-pattern) (not (string-null? search-pattern)))
            (string-append "No models matching \"" search-pattern "\"\n")
            ""
          ) ;if
          (let* ((w-p (%max-col-width 'provider "provider" rows))
                 (w-m (%max-col-width 'model "model" rows))
                 (w-c (%max-col-width 'context "context" rows))
                 (w-mo (%max-col-width 'max-out "max-out" rows))
                 (w-th (%max-col-width 'thinking "thinking" rows))
                 (w-im (%max-col-width 'images "images" rows))
                 (header (%format-table-line "provider" "model" "context"
                           "max-out" "thinking" "images" w-p w-m w-c w-mo w-th
                           w-im
                         ) ;%format-table-line
                 ) ;header
                 (body
                   (map (lambda (r)
                          (%format-table-line (%row-val 'provider r)
                            (%row-val 'model r)
                            (%row-val 'context r)
                            (%row-val 'max-out r)
                            (%row-val 'thinking r)
                            (%row-val 'images r)
                            w-p
                            w-m
                            w-c
                            w-mo
                            w-th
                            w-im
                          ) ;%format-table-line
                        ) ;lambda
                     rows
                   ) ;map
                 ) ;body
                ) ;
            (apply string-append header body)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

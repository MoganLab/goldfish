;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi ai models list)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii vector)
    (liii sort)
    (liii pi ai auth)
    (liii pi ai models catalog)
    (liii pi ai models search)
  ) ;import
  (export pi-format-token-count pi-list-models pi-format-models-table
    pi-fuzzy-match-model? pi-split-search-tokens pi-usable-models
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
       ((vector? input) (vector-any (lambda (x) (equal? x "image")) input))
       ((list? input) (and (member "image" input) #t))
       (else #f)
      ) ;cond
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
      (string-append (string-pad-right p w-p)
        "  "
        (string-pad-right m w-m)
        "  "
        (string-pad-right c w-c)
        "  "
        (string-pad-right mo w-mo)
        "  "
        (string-pad-right th w-th)
        "  "
        (string-pad-right im w-im)
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

    (define (%usable-model-item p-name m)
      (list (cons 'provider p-name)
        (cons 'id (%dict-ref m "id" ""))
        (cons 'name (%dict-ref m "name" ""))
        (cons 'reasoning (and (%dict-ref m "reasoning" #f) #t))
      ) ;list
    ) ;define

    ;; 可用模型列表（对齐上游 getAvailableSnapshot）：
    ;; 只收求有凭证 provider 的模型，条目为 (provider id name reasoning) alist，
    ;; 按 provider/id 排序，供 TUI /model 选择器使用
    (define (pi-usable-models models-store auth . opt-env)
      (let* ((provider-names (pi-catalog-provider-names models-store))
             (usable
               (filter (lambda (p-name) (and (apply pi-resolve-provider-key p-name auth opt-env) #t))
                 provider-names
               ) ;filter
             ) ;usable
             (rows
               (apply append
                 (map
                   (lambda (p-name)
                     (map (lambda (m) (%usable-model-item p-name m))
                       (pi-catalog-provider-models models-store p-name)
                     ) ;map
                   ) ;lambda
                   usable
                 ) ;map
               ) ;apply
             ) ;rows
            ) ;
        (list-sort
          (lambda (a b)
            (string<? (string-append (cdr (assoc 'provider a)) "\x00;" (cdr (assoc 'id a)))
              (string-append (cdr (assoc 'provider b)) "\x00;" (cdr (assoc 'id b)))
            ) ;string<?
          ) ;lambda
          rows
        ) ;list-sort
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

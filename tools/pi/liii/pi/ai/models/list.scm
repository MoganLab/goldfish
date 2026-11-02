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
    (liii tui)
    (liii pi ai auth)
    (liii pi ai models catalog)
    (liii pi ai models search)
    (liii pi ai thinking)
  ) ;import
  (export pi-format-token-count pi-list-models pi-format-models-table
    pi-fuzzy-match-model? pi-split-search-tokens pi-usable-models
  ) ;export
  (begin

    (define %dim "\x1b;[2m")
    (define %on "\x1b;[32m")
    (define %reset "\x1b;[0m")

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

    (define (%build-model-row p-name m . opt-p-enabled)
      (let* ((p-enabled (if (pair? opt-p-enabled) (car opt-p-enabled) #t))
             (m-enabled
               (let ((val (%dict-ref m "enabled" (%dict-ref m 'enabled #t))))
                 (if (boolean? val) val #t)
               ) ;let
             ) ;m-enabled
             (row-enabled (and p-enabled m-enabled))
             (mid (%dict-ref m "id" ""))
             (cw (%dict-ref m "contextWindow" 128000))
             (mt (%dict-ref m "maxTokens" 8192))
             (reasoning (pi-model-supports-reasoning? m))
             (input (%dict-ref m "input" '()))
            ) ;
        (list (cons 'provider p-name)
          (cons 'model mid)
          (cons 'context (pi-format-token-count cw))
          (cons 'max-out (pi-format-token-count mt))
          (cons 'thinking (if reasoning "yes" "no"))
          (cons 'images (if (%has-image? input) "yes" "no"))
          (cons 'enabled (if row-enabled #t #f))
        ) ;list
      ) ;let*
    ) ;define

    (define (%collect-provider-rows p-entry)
      (let* ((p-name (if (string? (car p-entry)) (car p-entry) (symbol->string (car p-entry)))
             ) ;p-name
             (p-fields (cdr p-entry))
             (p-enabled
               (let ((val (%dict-ref p-fields "enabled" (%dict-ref p-fields 'enabled #t))))
                 (if (boolean? val) val #t)
               ) ;let
             ) ;p-enabled
             (raw-models (%dict-ref p-fields "models" '()))
             (m-list (cond ((vector? raw-models) (vector->list raw-models))
                           ((list? raw-models) raw-models)
                           (else '())
                     ) ;cond
             ) ;m-list
            ) ;
        (map (lambda (m) (%build-model-row p-name m p-enabled)) m-list)
      ) ;let*
    ) ;define

    (define (%row-less-p a b)
      (let* ((ea-pair (assoc 'enabled a))
             (eb-pair (assoc 'enabled b))
             (ea (if ea-pair (cdr ea-pair) #t))
             (eb (if eb-pair (cdr eb-pair) #t))
            ) ;
        (cond ((and ea (not eb)) #t)
              ((and (not ea) eb) #f)
              (else
                (let ((pa (cdr (assoc 'provider a))) (pb (cdr (assoc 'provider b))))
                  (if (string=? pa pb)
                    (string<? (cdr (assoc 'model a)) (cdr (assoc 'model b)))
                    (string<? pa pb)
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
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

    (define (%pad-cell text width)
      (let* ((plain (strip-ansi text)) (len (display-width plain)))
        (if (<= len width)
          (string-append text (make-string (- width len) #\space))
          text
        ) ;if
      ) ;let*
    ) ;define

    (define (%format-header w-on w-p w-m w-c w-mo w-th w-im)
      (string-append (%pad-cell "on" w-on)
        "  "
        (%pad-cell "provider" w-p)
        "  "
        (%pad-cell "model" w-m)
        "  "
        (%pad-cell "context" w-c)
        "  "
        (%pad-cell "max-out" w-mo)
        "  "
        (%pad-cell "thinking" w-th)
        "  "
        (%pad-cell "images" w-im)
        "\n"
      ) ;string-append
    ) ;define

    (define (%format-data-row r color? w-on w-p w-m w-c w-mo w-th w-im)
      (let* ((enabled-pair (assoc 'enabled r))
             (enabled? (if enabled-pair (cdr enabled-pair) #t))
             (mark (if enabled? "●" "×"))
             (mark-cell (if color?
                          (if enabled? (string-append %on mark %reset) (string-append %dim mark %reset))
                          mark
                        ) ;if
             ) ;mark-cell
             (p (%pad-cell (%row-val 'provider r) w-p))
             (m (%pad-cell (%row-val 'model r) w-m))
             (c (%pad-cell (%row-val 'context r) w-c))
             (mo (%pad-cell (%row-val 'max-out r) w-mo))
             (th (%pad-cell (%row-val 'thinking r) w-th))
             (im (%pad-cell (%row-val 'images r) w-im))
             (rest-cols (string-append "  " p "  " m "  " c "  " mo "  " th "  " im))
            ) ;
        (if (and color? (not enabled?))
          (string-append (%pad-cell mark-cell w-on) %dim rest-cols %reset "\n")
          (string-append (%pad-cell mark-cell w-on) rest-cols "\n")
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-format-models-table rows . opt-args)
      (let* ((n (length opt-args))
             (search-pattern
               (cond ((= n 0) #f)
                     ((= n 1) (if (string? (car opt-args)) (car opt-args) #f))
                     (else (if (string? (car opt-args)) (car opt-args) #f))
               ) ;cond
             ) ;search-pattern
             (color?
               (cond ((= n 0) #f)
                     ((= n 1) (if (boolean? (car opt-args)) (car opt-args) #f))
                     (else (if (boolean? (cadr opt-args)) (cadr opt-args) #f))
               ) ;cond
             ) ;color?
            ) ;
        (if (null? rows)
          (if (and (string? search-pattern) (not (string-null? search-pattern)))
            (string-append "No models matching \"" search-pattern "\"\n")
            ""
          ) ;if
          (let* ((w-on 2)
                 (w-p (%max-col-width 'provider "provider" rows))
                 (w-m (%max-col-width 'model "model" rows))
                 (w-c (%max-col-width 'context "context" rows))
                 (w-mo (%max-col-width 'max-out "max-out" rows))
                 (w-th (%max-col-width 'thinking "thinking" rows))
                 (w-im (%max-col-width 'images "images" rows))
                 (header (%format-header w-on w-p w-m w-c w-mo w-th w-im))
                 (body (map (lambda (r) (%format-data-row r color? w-on w-p w-m w-c w-mo w-th w-im))
                         rows
                       ) ;map
                 ) ;body
                ) ;
            (apply string-append header body)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%usable-model-item p-name m)
      (list (cons 'provider p-name)
        (cons 'id (%dict-ref m "id" ""))
        (cons 'name (%dict-ref m "name" ""))
        ;; JSON 布尔为符号 true/false，必须归一化，否则 reasoning: false 被当真值
        (cons 'reasoning (pi-model-supports-reasoning? m))
        ;; 可用思考级别（对齐上游 getSupportedThinkingLevels）：
        ;; /model 切换后按新模型的可用级别收敛当前思考等级
        (cons 'thinking-levels (pi-model-thinking-levels m))
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

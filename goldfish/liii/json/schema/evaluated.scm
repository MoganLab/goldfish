;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii json schema evaluated)
  (import (scheme base)
    (liii json)
    (liii json schema data)
    (liii json schema reference)
  ) ;import
  (export evaluated-properties evaluated-items)
  (begin
    (define (same-key? a b)
      (cond ((and (number? a) (number? b)) (= a b))
            ((or (number? a) (number? b)) #f)
            (else (string=? (key->string a) (key->string b)))
      ) ;cond
    ) ;define
    (define (add-unique values)
      (let loop
        ((rest values) (result '()))
        (if (null? rest)
          (reverse result)
          (loop (cdr rest)
            (if
              (let find
                ((seen result))
                (and (pair? seen) (or (same-key? (car seen) (car rest)) (find (cdr seen))))
              ) ;let
              result
              (cons (car rest) result)
            ) ;if
          ) ;loop
        ) ;if
      ) ;let
    ) ;define
    (define (merge . lists)
      (add-unique (apply append lists))
    ) ;define
    (define (ref-root ref root target)
      (if (and (string? ref) (> (string-length ref) 0) (char=? (string-ref ref 0) #\#))
        root
        target
      ) ;if
    ) ;define
    ;; pattern 类关键字按注解忽略：patternProperties 匹配的属性不算已求值，
    ;; 因此 unevaluatedProperties 会把它们视为未求值（已记录偏差）。
    (define (pattern-match? patterns key)
      #f
    ) ;define
    (define (direct-properties instance schema)
      (if (not (json-object? instance))
        '()
        (let ((properties (object-ref schema "properties" '(())))
              (patterns (object-ref schema "patternProperties" '(())))
              (additional (object-ref schema "additionalProperties" absent))
             ) ;
          (let loop
            ((rest (entries instance)) (result '()))
            (if (null? rest)
              result
              (let ((key (caar rest)))
                (loop (cdr rest)
                  (if (or (object-has? properties key)
                        (pattern-match? patterns key)
                        (not (eq? additional absent))
                      ) ;or
                    (cons key result)
                    result
                  ) ;if
                ) ;loop
              ) ;let
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define
    (define (successful-schemas instance schemas root valid?)
      (let loop
        ((rest (listify schemas)) (result '()))
        (if (null? rest)
          (reverse result)
          (let ((matched (valid? instance (car rest) root)))
            (loop (cdr rest) (if matched (cons (car rest) result) result))
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (collect-property-branches validator instance branches root valid?)
      (let loop
        ((rest branches) (result '()))
        (if (null? rest)
          result
          (let* ((branch (car rest))
                 (collected (evaluated-properties validator instance branch root valid? #t))
                 (collected
                   (if (and (json-object? branch) (object-has? branch "unevaluatedProperties"))
                     (merge collected (map car (entries instance)))
                     collected
                   ) ;if
                 ) ;collected
                ) ;
            (loop (cdr rest) (merge result collected))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
    (define (branches-properties validator instance schemas root valid? mode conditional?)
      (if (not conditional?)
        '()
        (let* ((all-schemas (listify schemas))
               (successes (successful-schemas instance all-schemas root valid?))
              ) ;
          (cond
           ((eq? mode 'all)
            (if (= (length successes) (length all-schemas))
              (collect-property-branches validator instance successes root valid?)
              '()
            ) ;if
           ) ;
           ((eq? mode 'any)
            (collect-property-branches validator instance successes root valid?)
           ) ;
           ((eq? mode 'one)
            (if (= (length successes) 1)
              (collect-property-branches validator instance successes root valid?)
              '()
            ) ;if
           ) ;
           (else '())
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define
    (define (evaluated-properties validator instance schema root valid? . conditional?)
      (let ((conditional? (if (null? conditional?) #t (car conditional?))))
        (if (not (json-object? schema))
          '()
          (let* ((direct (direct-properties instance schema))
                 (all (branches-properties validator
                        instance
                        (object-ref schema "allOf" #())
                        root
                        valid?
                        'all
                        conditional?
                      ) ;branches-properties
                 ) ;all
                 (any (branches-properties validator
                        instance
                        (object-ref schema "anyOf" #())
                        root
                        valid?
                        'any
                        conditional?
                      ) ;branches-properties
                 ) ;any
                 (one (branches-properties validator
                        instance
                        (object-ref schema "oneOf" #())
                        root
                        valid?
                        'one
                        conditional?
                      ) ;branches-properties
                 ) ;one
                 (ref (object-ref schema "$ref" #f))
                 (dynamic-ref (object-ref schema "$dynamicRef" #f))
                ) ;
            (merge direct
              all
              any
              one
              (if (and conditional? (object-has? schema "if"))
                (let* ((condition (object-ref schema "if" #f))
                       (passed (valid? instance condition root))
                       (branch-name (if passed "then" "else"))
                       (branch (object-ref schema branch-name absent))
                      ) ;
                  (merge (if passed
                           (evaluated-properties validator instance condition root valid? #f)
                           '()
                         ) ;if
                    (if (or (eq? branch absent) (not (valid? instance branch root)))
                      '()
                      (evaluated-properties validator instance branch root valid? #t)
                    ) ;if
                  ) ;merge
                ) ;let*
                '()
              ) ;if
              (let loop
                ((deps (entries (object-ref schema "dependentSchemas" '(())))) (result '()))
                (if (null? deps)
                  result
                  (let* ((dependency (car deps))
                         (matched (and (object-has? instance (car dependency))
                                    (valid? instance (cdr dependency) root)
                                  ) ;and
                         ) ;matched
                         (collected (if matched
                                      (evaluated-properties validator instance (cdr dependency) root valid? #t)
                                      '()
                                    ) ;if
                         ) ;collected
                        ) ;
                    (loop (cdr deps) (merge result collected))
                  ) ;let*
                ) ;if
              ) ;let
              (if (and conditional? ref)
                (let* ((target (resolve-reference validator ref root))
                       (target-root (ref-root ref root target))
                      ) ;
                  (if (valid? instance target target-root)
                    (evaluated-properties validator instance target target-root valid? #t)
                    '()
                  ) ;if
                ) ;let*
                '()
              ) ;if
              (if (and conditional? dynamic-ref)
                (let* ((target (resolve-dynamic-reference validator root dynamic-ref))
                       (target-root (ref-root dynamic-ref root target))
                      ) ;
                  (if (valid? instance target target-root)
                    (call-with-dynamic-scope root
                      (lambda ()
                        (evaluated-properties validator instance target target-root valid? #t)
                      ) ;lambda
                    ) ;call-with-dynamic-scope
                    '()
                  ) ;if
                ) ;let*
                '()
              ) ;if
            ) ;merge
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
    (define (direct-items instance schema valid?)
      (if (not (vector? instance))
        '()
        (let* ((prefix (listify (object-ref schema "prefixItems" #())))
               (prefix-count (min (length prefix) (vector-length instance)))
               (has-items (object-has? schema "items"))
               (contains (object-ref schema "contains" absent))
              ) ;
          (append
            (let loop
              ((i 0) (result '()))
              (if (= i prefix-count) result (loop (+ i 1) (cons i result)))
            ) ;let
            (if has-items
              (let loop
                ((i (length prefix)) (result '()))
                (if (>= i (vector-length instance)) result (loop (+ i 1) (cons i result)))
              ) ;let
              '()
            ) ;if
            (if (eq? contains absent)
              '()
              (let loop
                ((i 0) (result '()))
                (if (= i (vector-length instance))
                  result
                  (let ((matched (valid? (vector-ref instance i) contains #f)))
                    (loop (+ i 1) (if matched (cons i result) result))
                  ) ;let
                ) ;if
              ) ;let
            ) ;if
          ) ;append
        ) ;let*
      ) ;if
    ) ;define
    (define (all-indices instance)
      (let indices
        ((i 0) (result '()))
        (if (= i (vector-length instance)) result (indices (+ i 1) (cons i result)))
      ) ;let
    ) ;define
    (define (branches-items validator instance schemas root valid?)
      (let loop
        ((rest (listify schemas)) (result '()))
        (if (null? rest)
          result
          (let ((schema (car rest)))
            (if (not (valid? instance schema root))
              (loop (cdr rest) result)
              (let ((collected (branch-items validator instance schema root valid?)))
                (loop (cdr rest) (merge result collected))
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (branch-items validator instance schema root valid?)
      (let* ((collected (evaluated-items validator instance schema root valid?))
             (collected (if (and (json-object? schema) (object-has? schema "unevaluatedItems"))
                          (merge collected (all-indices instance))
                          collected
                        ) ;if
             ) ;collected
            ) ;
        collected
      ) ;let*
    ) ;define
    (define (conditional-items validator instance schema root valid?)
      (if (not (object-has? schema "if"))
        '()
        (let* ((condition (object-ref schema "if" #f))
               (passed (valid? instance condition root))
               (branch (object-ref schema (if passed "then" "else") absent))
              ) ;
          (merge (if passed (evaluated-items validator instance condition root valid?) '())
            (if (or (eq? branch absent) (not (valid? instance branch root)))
              '()
              (evaluated-items validator instance branch root valid?)
            ) ;if
          ) ;merge
        ) ;let*
      ) ;if
    ) ;define
    (define (referenced-items validator instance schema root valid? ref)
      (if (not ref)
        '()
        (let* ((target (resolve-reference validator ref root))
               (target-root (ref-root ref root target))
              ) ;
          (if (valid? instance target target-root)
            (evaluated-items validator instance target target-root valid?)
            '()
          ) ;if
        ) ;let*
      ) ;if
    ) ;define
    (define (dynamic-referenced-items validator instance schema root valid? dynamic-ref)
      (if (not dynamic-ref)
        '()
        (let* ((target (resolve-dynamic-reference validator root dynamic-ref))
               (target-root (ref-root dynamic-ref root target))
              ) ;
          (if (valid? instance target target-root)
            (call-with-dynamic-scope root
              (lambda () (evaluated-items validator instance target target-root valid?))
            ) ;call-with-dynamic-scope
            '()
          ) ;if
        ) ;let*
      ) ;if
    ) ;define
    (define (evaluated-items validator instance schema root valid?)
      (if (or (not (vector? instance)) (not (json-object? schema)))
        '()
        (let ((ref (object-ref schema "$ref" #f))
              (dynamic-ref (object-ref schema "$dynamicRef" #f))
             ) ;
          (merge (direct-items instance
                   schema
                   (lambda (value subschema ignored) (valid? value subschema root))
                 ) ;direct-items
            (branches-items validator instance (object-ref schema "allOf" #()) root valid?)
            (branches-items validator instance (object-ref schema "anyOf" #()) root valid?)
            (branches-items validator instance (object-ref schema "oneOf" #()) root valid?)
            (conditional-items validator instance schema root valid?)
            (referenced-items validator instance schema root valid? ref)
            (dynamic-referenced-items validator instance schema root valid? dynamic-ref)
          ) ;merge
        ) ;let
      ) ;if
    ) ;define
  ) ;begin
) ;define-library

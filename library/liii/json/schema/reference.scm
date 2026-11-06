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

(define-library (liii json schema reference)
  (import (scheme base)
    (liii json)
    (liii json schema cache)
    (liii json schema budget)
    (liii json schema data)
    (liii json schema model)
    (liii json schema pointer)
    (liii json schema embedded)
  ) ;import
  (export resolve-reference resolve-reference-root resolve-reference-with-root
    embedded-resource anchor-resource dynamic-anchor-resource
    resolve-dynamic-reference call-with-dynamic-scope reset-dynamic-scope!
    resolve-resource-name resolve-pointer uri-fragment uri-without-fragment
    percent-decode absolute-uri? uri-directory
  ) ;export
  (begin
    (define dynamic-scope '())
    (define (current-dynamic-scope)
      dynamic-scope
    ) ;define
    (define (reset-dynamic-scope!)
      (set! dynamic-scope '())
    ) ;define
    (define (call-with-dynamic-scope root thunk)
      (let ((saved dynamic-scope))
        (dynamic-wind (lambda () (set! dynamic-scope (cons root saved)))
          thunk
          (lambda () (set! dynamic-scope saved))
        ) ;dynamic-wind
      ) ;let
    ) ;define
    (define (resolve-dynamic-anchor validator root anchor)
      (let ((local (guard (condition (#t #f)) (walk-dynamic-anchor root anchor #t))))
        (if (not local)
          (anchor-resource root anchor)
          (let loop
            ((scope (append dynamic-scope (list (validator-schema validator)))))
            (cond ((null? scope) local)
                  ((equal? (car scope) root) (loop (cdr scope)))
                  (else
                    (let ((target (guard (condition (#t #f))
                                    (dynamic-anchor-resource (car scope) anchor)
                                  ) ;guard
                          ) ;target
                         ) ;
                      (if target target (loop (cdr scope)))
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    ;; The reference cache is carried in the validator options, so every
    ;; validator resolves references on its own and validators never share
    ;; cached entries.
    (define (validator-cache validator)
      (option-ref (validator-options validator) 'reference-cache #f)
    ) ;define
    ;; Bounds the references of one run, stopping exponential graphs.
    (define (validator-budget validator)
      (option-ref (validator-options validator) 'resource-budget #f)
    ) ;define
    ;; Resolves a reference, reusing a cached result when the key matches.
    ;; The compute procedure returns (resource . cacheable?); the flag is
    ;; false when the resolution went through the user supplied retrieve
    ;; function, because a cache hit must never skip its side effect.
    (define (cached-resolution validator reference root scope compute)
      (let ((cache (validator-cache validator)))
        (if (not cache)
          (car (compute))
          (let ((entry (reference-cache-lookup cache reference root scope)))
            (if entry
              (cadddr entry)
              (let ((result (compute)))
                (let ((budget (validator-budget validator)))
                  (when budget
                    (count-budget-reference! budget)
                  ) ;when
                ) ;let
                (when (cdr result)
                  (reference-cache-put! cache reference root scope (car result))
                ) ;when
                (car result)
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (resolve-dynamic-reference validator root ref)
      (cached-resolution validator
        ref
        root
        (current-dynamic-scope)
        (lambda () (resolve-dynamic-reference* validator root ref))
      ) ;cached-resolution
    ) ;define
    (define (resolve-dynamic-reference* validator root ref)
      (cond
       ((and (string? ref)
          (>= (string-length ref) 2)
          (char=? (string-ref ref 0) #\#)
          (char=? (string-ref ref 1) #\/)
        ) ;and
        (resolve-reference* validator ref root)
       ) ;
       ((and (string? ref)
          (> (string-length ref) 1)
          (char=? (string-ref ref 0) #\#)
          (not (char=? (string-ref ref 1) #\/))
        ) ;and
        (cons (resolve-dynamic-anchor validator root (substring ref 1)) #t)
       ) ;
       ((and (string? ref) (uri-fragment ref) (not (string=? (uri-fragment ref) "")))
        (let ((target (resolve-reference* validator (uri-without-fragment ref) root))
              (fragment (percent-decode (uri-fragment ref)))
             ) ;
          (cons (if (char=? (string-ref fragment 0) #\/)
                  (resolve-pointer (car target) fragment)
                  (resolve-dynamic-anchor validator (car target) fragment)
                ) ;if
            #t
          ) ;cons
        ) ;let
       ) ;
       (else (resolve-reference* validator ref root))
      ) ;cond
    ) ;define
    (define (resolve-reference-root validator ref root)
      (cond
       ((and (string? ref) (> (string-length ref) 0) (char=? (string-ref ref 0) #\#))
        root
       ) ;
       ((and (string? ref) (uri-fragment ref))
        (resolve-reference validator (uri-without-fragment ref) root)
       ) ;
       (else (resolve-reference validator ref root))
      ) ;cond
    ) ;define
    (define (resolve-reference-with-root validator ref root)
      (if (and (string? ref) (> (string-length ref) 0) (char=? (string-ref ref 0) #\#))
        (values (resolve-reference validator ref root) root)
        (let* ((target-root (resolve-reference-root validator ref root))
               (fragment (and (string? ref) (uri-fragment ref)))
              ) ;
          (if fragment
            (let ((decoded (percent-decode fragment)))
              (values
                (if (and (> (string-length decoded) 0) (char=? (string-ref decoded 0) #\/))
                  (resolve-pointer target-root decoded)
                  (anchor-resource target-root decoded)
                ) ;if
                target-root
              ) ;values
            ) ;let
            (values target-root target-root)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define
    (define (resolve-reference validator ref root)
      (cached-resolution validator
        ref
        root
        '()
        (lambda () (resolve-reference* validator ref root))
      ) ;cached-resolution
    ) ;define
    (define (resolve-reference* validator ref root)
      (cond ((not (string? ref)) (error 'json-schema-unresolvable ref))
            ((string=? ref "#") (cons root #t))
            ((and (>= (string-length ref) 2)
               (char=? (string-ref ref 0) #\#)
               (char=? (string-ref ref 1) #\/)
             ) ;and
             (cons (resolve-pointer root (percent-decode (substring ref 1))) #t)
            ) ;
            ((and (> (string-length ref) 0) (char=? (string-ref ref 0) #\#))
             (cons (anchor-resource root (percent-decode (substring ref 1))) #t)
            ) ;
            ((uri-fragment ref)
             (let ((target (resolve-reference* validator (uri-without-fragment ref) root))
                   (fragment (percent-decode (uri-fragment ref)))
                  ) ;
               (cons
                 (if (and (> (string-length fragment) 0) (char=? (string-ref fragment 0) #\/))
                   (resolve-pointer (car target) fragment)
                   (anchor-resource (car target) fragment)
                 ) ;if
                 #t
               ) ;cons
             ) ;let
            ) ;
            (else (resolve-remote-resource* validator ref root))
      ) ;cond
    ) ;define
    ;; Returns the resource and its cacheability. A resource from the
    ;; user supplied retrieve function is never cached.
    (define (resolve-remote-resource* validator ref root)
      (let* ((resource-name (resolve-resource-name ref root))
             (embedded (or (embedded-resource root resource-name)
                         (embedded-resource (validator-schema validator) resource-name)
                       ) ;or
             ) ;embedded
             (registry (option-ref (validator-options validator) 'registry #f))
             (cell (and registry (assoc resource-name (registry-resources registry))))
            ) ;
        (cond (embedded (cons embedded #t))
              (cell (cons (with-base-uri (cdr cell) resource-name) #t))
              (else (cons (with-base-uri (retrieve-resource validator resource-name) resource-name)
                      #f
                    ) ;cons
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define
    ;; A fetched or registered resource without $id uses its retrieval
    ;; URI as the base URI, so relative references inside it resolve
    ;; against the place it came from. The entry list is copied, the
    ;; caller's object is never modified.
    (define (with-base-uri resource uri)
      (if (or (not (json-object? resource)) (object-has? resource "$id"))
        resource
        (cons (cons "$id" uri) (entries resource))
      ) ;if
    ) ;define
    (define (retrieve-resource validator resource-name)
      (let ((retrieve (option-ref (validator-options validator) 'retrieve #f)))
        (if (not retrieve)
          (error 'json-schema-unresolvable resource-name)
          (let ((resource (retrieve resource-name)))
            (if (or (json-boolean? resource) (json-object? resource))
              resource
              (error 'json-schema-unresolvable resource-name)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define
  ) ;begin
) ;define-library

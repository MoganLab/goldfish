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

(define-library (liii json schema keywords)
  (import (scheme base)
    (liii json schema keywords-basic)
    (liii json schema keywords-object)
    (liii json schema keywords-array)
    (liii json schema keywords-combo)
    (liii json schema reference)
    (liii json schema format)
    (liii json schema error)
    (liii json schema model)
  ) ;import
  (export validate-keyword meta-validation-root set-meta-validation-root!
    with-meta-validation-root
  ) ;export
  (begin
    (define active-meta-root #f)
    (define (meta-validation-root)
      active-meta-root
    ) ;define
    (define (set-meta-validation-root! value)
      (set! active-meta-root value)
    ) ;define
    ;; The meta validation root is global state, so it is restored even
    ;; when the validation raises. Without this an aborted check-schema
    ;; run would leave the root in place and change how a later run
    ;; resolves a dynamic reference.
    (define (with-meta-validation-root root thunk)
      (let ((saved active-meta-root))
        (dynamic-wind (lambda () (set! active-meta-root root))
          thunk
          (lambda () (set! active-meta-root saved))
        ) ;dynamic-wind
      ) ;let
    ) ;define
    (define (validate-keyword validator keyword value instance schema path
              schema-path root descend valid?
            ) ;validate-keyword
      (cond ((basic-keyword? keyword)
             (validate-basic keyword value instance schema path schema-path)
            ) ;
            ((object-keyword? keyword)
             (validate-object validator keyword value instance schema path
               schema-path root descend valid?
             ) ;validate-object
            ) ;
            ((array-keyword? keyword)
             (validate-array validator keyword value instance schema path
               schema-path root descend valid?
             ) ;validate-array
            ) ;
            ((combo-keyword? keyword)
             (validate-combo keyword value instance schema path schema-path root
               descend valid?
             ) ;validate-combo
            ) ;
            ((string=? keyword "$ref")
             (let-values (((target target-root) (resolve-reference-with-root validator value root)))
               (call-with-dynamic-scope root
                 (lambda () (descend instance target path schema-path target-root))
               ) ;call-with-dynamic-scope
             ) ;let-values
            ) ;
            ((string=? keyword "$dynamicRef")
             (let ((meta-root (meta-validation-root)))
               (if meta-root
                 (descend instance meta-root path schema-path meta-root)
                 (let ((target (resolve-dynamic-reference validator root value)))
                   (call-with-dynamic-scope root
                     (lambda ()
                       (descend instance
                         target
                         path
                         schema-path
                         (if (and (string? value)
                               (> (string-length value) 0)
                               (char=? (string-ref value 0) #\#)
                             ) ;and
                           root
                           target
                         ) ;if
                       ) ;descend
                     ) ;lambda
                   ) ;call-with-dynamic-scope
                 ) ;let
               ) ;if
             ) ;let
            ) ;
            ((string=? keyword "format")
             (validate-format validator value instance schema path schema-path)
            ) ;
            (else '())
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library

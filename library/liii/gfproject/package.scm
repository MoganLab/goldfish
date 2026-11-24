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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii gfproject package)
  (import (scheme base) (liii base) (liii error) (liii json) (liii string))
  (export gfproject-package gfproject-package? gfproject-package-name
    gfproject-package-organization gfproject-package-description
    gfproject-package-version gfproject-package-id json->gfproject-package
  ) ;export
  (begin

    (define*-record-type <gfproject-package>
      (gfproject-package name organization (description "") (version #f))
      gfproject-package?
      (name gfproject-package-name)
      (organization gfproject-package-organization)
      (description gfproject-package-description)
      (version gfproject-package-version)
    ) ;define*-record-type

    (define (gfproject-package-id package)
      (unless (gfproject-package? package)
        (type-error "gfproject-package-id: expected a gfproject package" package)
      ) ;unless
      (string-append (gfproject-package-name package)
        "@"
        (gfproject-package-organization package)
      ) ;string-append
    ) ;define

    (define (%valid-name-part? value)
      (and (string? value)
        (> (string-length value) 0)
        (let ((first (string-ref value 0)))
          (and (char>=? first #\a) (char<=? first #\z))
        ) ;let
        (let loop
          ((index 0))
          (if (= index (string-length value))
            #t
            (let ((ch (string-ref value index)))
              (and
                (or (and (char>=? ch #\a) (char<=? ch #\z))
                  (and (char>=? ch #\0) (char<=? ch #\9))
                  (and (> index 0) (or (char=? ch #\-) (char=? ch #\_)))
                ) ;or
                (loop (+ index 1))
              ) ;and
            ) ;let
          ) ;if
        ) ;let
      ) ;and
    ) ;define

    (define (%required-name-part package key)
      (let ((value (json-ref-string package key #f)))
        (unless (%valid-name-part? value)
          (value-error "gfproject package name and organization must match ^[a-z][a-z0-9_-]*$"
            key
            value
          ) ;value-error
        ) ;unless
        value
      ) ;let
    ) ;define

    (define (%package-description package)
      (let ((value (if (json-contains-key? package "description")
                     (json-ref package "description")
                     'null
                   ) ;if
            ) ;value
           ) ;
        (cond ((json-null? value) "")
              ((string? value) value)
              (else (value-error "gfproject package description must be a string" value))
        ) ;cond
      ) ;let
    ) ;define

    ;; version 可选；缺省返回 #f，非字符串时报错。
    (define (%package-version package)
      (let ((value (if (json-contains-key? package "version")
                     (json-ref package "version")
                     'null
                   ) ;if
            ) ;value
           ) ;
        (cond ((json-null? value) #f)
              ((string? value) value)
              (else (value-error "gfproject package version must be a string" value))
        ) ;cond
      ) ;let
    ) ;define

    (define (%check-known-fields package)
      (for-each
        (lambda (key)
          (unless (member key '("name" "organization" "description" "version"))
            (value-error "gfproject package contains an unknown field" key)
          ) ;unless
        ) ;lambda
        (json-keys package)
      ) ;for-each
    ) ;define

    (define (json->gfproject-package config)
      (unless (json-object? config)
        (type-error "json->gfproject-package: expected a JSON object" config)
      ) ;unless
      (if (not (json-contains-key? config "package"))
        #f
        (let ((package (json-ref config "package")))
          (unless (json-object? package)
            (type-error "gfproject package must be a JSON object" package)
          ) ;unless
          (%check-known-fields package)
          (gfproject-package (%required-name-part package "name")
            (%required-name-part package "organization")
            (%package-description package)
            (%package-version package)
          ) ;gfproject-package
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library

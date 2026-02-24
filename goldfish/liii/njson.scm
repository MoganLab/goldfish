;
; Copyright (C) 2026 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(define-library (liii njson)
  (import (liii base)
          (liii error))
  (export njson?
          njson-free
          njson-string->json
          njson-json->string
          njson-ref
          njson-set
          njson-push
          njson-drop
          njson-contains-key?
          njson-keys)
  (begin
    (define (njson-null-symbol? x)
      (and (symbol? x) (symbol=? x 'null)))

    (define (njson? x)
      (g_njson-handle? x))

    (define (njson-free x)
      (unless (njson? x)
        (type-error "njson-free: input must be njson-handle" x))
      (g_njson-free x))

    (define (njson-string->json json-string)
      (unless (string? json-string)
        (type-error "njson-string->json: input must be string" json-string))
      (g_njson-string->json json-string))

    (define (njson-json->string x)
      (unless (or (njson? x) (string? x) (number? x) (boolean? x) (njson-null-symbol? x))
        (type-error "njson-json->string: input must be njson-handle or strict json scalar" x))
      (g_njson-json->string x))

    (define (njson-ref json key . keys)
      (unless (njson? json)
        (type-error "njson-ref: json must be njson-handle" json))
      (apply g_njson-ref (cons json (cons key keys))))

    ;; Same calling style as (liii json):
    ;; (njson-set j key value)
    ;; (njson-set j k1 k2 ... kn value)
    (define (njson-set json key val . keys)
      (unless (njson? json)
        (type-error "njson-set: json must be njson-handle" json))
      (apply g_njson-set (cons json (cons key (cons val keys)))))

    ;; Same calling style as (liii json):
    ;; (njson-push j key value)
    ;; (njson-push j k1 k2 ... kn value)
    (define (njson-push json key val . keys)
      (unless (njson? json)
        (type-error "njson-push: json must be njson-handle" json))
      (apply g_njson-push (cons json (cons key (cons val keys)))))

    (define (njson-drop json key . keys)
      (unless (njson? json)
        (type-error "njson-drop: json must be njson-handle" json))
      (apply g_njson-drop (cons json (cons key keys))))

    (define (njson-contains-key? json key)
      (unless (njson? json)
        (type-error "njson-contains-key?: json must be njson-handle" json))
      (g_njson-contains-key? json key))

    (define (njson-keys json)
      (unless (njson? json)
        (type-error "njson-keys: json must be njson-handle" json))
      (g_njson-keys json))

    ) ; end of begin
  ) ; end of define-library

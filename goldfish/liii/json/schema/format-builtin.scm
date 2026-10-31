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

(define-library (liii json schema format-builtin)
  (import (scheme base)
    (scheme char)
    (liii json schema format-datetime)
    (liii json schema format-email)
    (liii json schema format-network)
    (liii json schema format-uri)
    (liii string)
  ) ;import
  (export json-schema-format-builtins)
  (begin
    (define json-schema-format-builtins
      (list (cons 'ipv4 ipv4-address?)
        (cons 'ipv6 ipv6-address?)
        (cons 'email email-address?)
        (cons 'idn-email idn-email-address?)
        (cons 'hostname hostname?)
        (cons 'date date?)
        (cons 'time time?)
        (cons 'date-time date-time?)
        (cons 'duration duration?)
        (cons 'uri uri?)
        (cons 'uri-reference uri-reference?)
        (cons 'json-pointer json-pointer?)
        (cons 'relative-json-pointer relative-json-pointer?)
        (cons 'regex regex?)
      ) ;list
    ) ;define
  ) ;begin
) ;define-library

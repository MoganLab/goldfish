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

(define-library (liii json schema format-network)
  (import (scheme base)
    (scheme char)
    (liii json schema format-helpers)
    (liii string)
  ) ;import
  (export ipv4-address? ipv6-address? hostname?)
  (begin
    ;; ---- IPv4 -----------------------------------------------------------

    (define (ipv4-address? value)
      (and (json-schema-all-ascii? value)
        (let ((parts (json-schema-split-on-char value #\.)))
          (and (= (length parts) 4)
            (let loop
              ((rest parts))
              (cond ((null? rest) #t)
                    ((not (json-schema-decimal-string? (car rest))) #f)
                    ((> (json-schema-decimal->number (car rest)) 255) #f)
                    (else (loop (cdr rest)))
              ) ;cond
            ) ;let
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    ;; ---- IPv6 -----------------------------------------------------------

    (define (find-double-colon value)
      ;; Returns the index of the single "::" or #f when absent.
      (let loop
        ((index 0))
        (cond
         ((> (+ index 1) (- (string-length value) 1)) #f)
         ((and (char=? (string-ref value index) #\:)
            (char=? (string-ref value (+ index 1)) #\:)
          ) ;and
          (if (find-double-colon-from value (+ index 1)) #f index)
         ) ;
         (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define
    (define (find-double-colon-from value index)
      (let loop
        ((index index))
        (cond
         ((> (+ index 1) (- (string-length value) 1)) #f)
         ((and (char=? (string-ref value index) #\:)
            (char=? (string-ref value (+ index 1)) #\:)
          ) ;and
          #t
         ) ;
         (else (loop (+ index 1)))
        ) ;cond
      ) ;let
    ) ;define
    (define (ipv6-groups-ok? parts)
      (let loop
        ((rest parts))
        (cond ((null? rest) #t)
              ((not (ipv6-group? (car rest))) #f)
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define
    (define (ipv6-group? part)
      (or (ipv4-address? part) (json-schema-hex-string? part))
    ) ;define
    ;; An embedded IPv4 address occupies two 16 bit groups.
    (define (ipv6-group-count parts)
      (if (null? parts)
        0
        (let ((last (car (reverse parts))))
          (if (ipv4-address? last) (+ (- (length parts) 1) 2) (length parts))
        ) ;let
      ) ;if
    ) ;define
    (define (ipv6-address? value)
      (and (json-schema-all-ascii? value)
        (let ((double (find-double-colon value)))
          (if double
            (let* ((left (substring value 0 double))
                   (right (substring value (+ double 2) (string-length value)))
                   (parts (append (if (string=? left "") '() (json-schema-split-on-char left #\:))
                            (if (string=? right "") '() (json-schema-split-on-char right #\:))
                          ) ;append
                   ) ;parts
                  ) ;
              (and (ipv6-groups-ok? parts) (< (ipv6-group-count parts) 8))
            ) ;let*
            (let ((parts (json-schema-split-on-char value #\:)))
              (and (ipv6-groups-ok? parts) (= (ipv6-group-count parts) 8))
            ) ;let
          ) ;if
        ) ;let
      ) ;and
    ) ;define

    ;; ---- hostname -------------------------------------------------------

    (define (hostname-label? label)
      (and (> (string-length label) 0)
        (<= (string-length label) 63)
        (json-schema-ascii-alnum? (string-ref label 0))
        (json-schema-ascii-alnum? (string-ref label (- (string-length label) 1)))
        (let loop
          ((index 0))
          (cond ((= index (string-length label)) #t)
                ((or (json-schema-ascii-alnum? (string-ref label index))
                   (char=? (string-ref label index) #\-)
                 ) ;or
                 (loop (+ index 1))
                ) ;
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define
    (define (punycode-prefix? label)
      (and (> (string-length label) 4)
        (string=? (string-downcase (substring label 0 4)) "xn--")
      ) ;and
    ) ;define
    ;; Structural punycode check: the label must use the punycode alphabet.
    (define (punycode-alphabet? value)
      (let loop
        ((index 4))
        (cond ((= index (string-length value)) #t)
              ((or (json-schema-ascii-digit? (string-ref value index))
                 (and (char>=? (string-ref value index) #\a)
                   (char<=? (string-ref value index) #\z)
                 ) ;and
                 (and (char>=? (string-ref value index) #\A)
                   (char<=? (string-ref value index) #\Z)
                 ) ;and
               ) ;or
               (loop (+ index 1))
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define
    (define (hostname? value)
      (and (json-schema-all-ascii? value)
        (> (string-length value) 0)
        (<= (string-length value) 253)
        (let loop
          ((labels (json-schema-split-on-char value #\.)))
          (cond ((null? labels) #t)
                ((not (hostname-label? (car labels))) #f)
                ((and (punycode-prefix? (car labels)) (not (punycode-alphabet? (car labels))))
                 #f
                ) ;
                (else (loop (cdr labels)))
          ) ;cond
        ) ;let
      ) ;and
    ) ;define
  ) ;begin
) ;define-library

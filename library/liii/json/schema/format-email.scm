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

(define-library (liii json schema format-email)
  (import (scheme base)
    (scheme char)
    (liii json schema format-helpers)
    (liii json schema format-network)
    (liii string)
  ) ;import
  (export email-address? idn-email-address?)
  (begin
    ;; ---- email ----------------------------------------------------------

    ;; RFC 5322 atext characters.
    (define (atext? char)
      (or (json-schema-ascii-alnum? char)
        (and (char>=? char #\!) (char<=? char #\/))
        (char=? char #\:)
        (char=? char #\<)
        (char=? char #\>)
        (char=? char #\@)
        (char=? char #\[)
        (char=? char #\])
        (char=? char #\^)
        (char=? char #\_)
        (char=? char #\`)
        (char=? char #\{)
        (char=? char #\|)
        (char=? char #\})
        (char=? char #\~)
      ) ;or
    ) ;define
    (define (dot-atom? value)
      (and (> (string-length value) 0)
        (not (char=? (string-ref value 0) #\.))
        (not
          (char=? (string-ref value (- (string-length value) 1)) #\.)
        ) ;not
        (let loop
          ((index 0) (previous-dot #t))
          (cond ((= index (string-length value)) #t)
                ((char=? (string-ref value index) #\.)
                 (if previous-dot #f (loop (+ index 1) #t))
                ) ;
                ((atext? (string-ref value index)) (loop (+ index 1) #f))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define
    (define (quoted-string? value)
      (and (>= (string-length value) 2)
        (char=? (string-ref value 0) #\")
        (char=? (string-ref value (- (string-length value) 1)) #\")
        (let loop
          ((index 1) (escaped #f))
          (cond
           ((>= index (- (string-length value) 1)) #t)
           ((if escaped #t #f) (loop (+ index 1) #f))
           ((char=? (string-ref value index) #\\) (loop (+ index 1) #t))
           ((or (char=? (string-ref value index) #\")
              (char=? (string-ref value index) #\newline)
              (char=? (string-ref value index) #\return)
            ) ;or
            #f
           ) ;
           (else (loop (+ index 1) #f))
          ) ;cond
        ) ;let
      ) ;and
    ) ;define
    (define (ip-literal? value)
      (and (>= (string-length value) 2)
        (char=? (string-ref value 0) #\[)
        (char=? (string-ref value (- (string-length value) 1)) #\])
        (let ((inner (substring value 1 (- (string-length value) 1))))
          (cond ((ipv4-address? inner) #t)
                ((and (> (string-length inner) 5)
                   (string=? (substring inner 0 5) "IPv6:")
                   (ipv6-address? (substring inner 5))
                 ) ;and
                 #t
                ) ;
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define
    (define (email-domain? value)
      (or (ip-literal? value) (hostname? value))
    ) ;define
    (define (find-at value)
      (let loop
        ((index 0) (in-quote #f) (escaped #f) (at #f))
        (cond ((= index (string-length value)) at)
              ((if escaped #t #f) (loop (+ index 1) in-quote #f at))
              ((and in-quote (char=? (string-ref value index) #\\))
               (loop (+ index 1) in-quote #t at)
              ) ;
              ((char=? (string-ref value index) #\") (loop (+ index 1) (not in-quote) #f at))
              ((and (not in-quote) (char=? (string-ref value index) #\@))
               (if at #f (loop (+ index 1) in-quote #f index))
              ) ;
              (else (loop (+ index 1) in-quote #f at))
        ) ;cond
      ) ;let
    ) ;define
    (define (email-address? value)
      (and (json-schema-all-ascii? value)
        (let ((at (find-at value)))
          (if (not at)
            #f
            (let ((local (substring value 0 at))
                  (domain (substring value (+ at 1) (string-length value)))
                 ) ;
              (and (or (dot-atom? local) (quoted-string? local)) (email-domain? domain))
            ) ;let
          ) ;if
        ) ;let
      ) ;and
    ) ;define
    (define (utf8-code-points value)
      (let loop
        ((index 0) (result '()))
        (if (= index (string-length value))
          (reverse result)
          (let* ((byte (char->integer (string-ref value index)))
                 (extra
                   (cond ((< byte 128) 0)
                         ((and (>= byte 194) (<= byte 223)) 1)
                         ((and (>= byte 224) (<= byte 239)) 2)
                         ((and (>= byte 240) (<= byte 244)) 3)
                         (else -1)
                   ) ;cond
                 ) ;extra
                ) ;
            (if
              (or (< extra 0) (> (+ index extra) (- (string-length value) 1)))
              (loop (+ index 1) (cons 65533 result))
              (let ((code (decode-utf8 value index extra)))
                (loop (+ index extra 1) (cons code result))
              ) ;let
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
    (define (decode-utf8 value index extra)
      (let ((byte (char->integer (string-ref value index))))
        (if (< extra 1)
          byte
          (let ((leading (cond ((= extra 1) 192) ((= extra 2) 224) (else 240))))
            (let loop
              ((offset 1) (code (- byte leading)))
              (if (> offset extra)
                code
                (loop (+ offset 1)
                  (+ (* code 64) (- (char->integer (string-ref value (+ index offset))) 128))
                ) ;loop
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (surrogate-code? code)
      (and (>= code 55296) (<= code 57343))
    ) ;define
    (define (valid-scalars? value)
      (let loop
        ((codes (utf8-code-points value)))
        (cond ((null? codes) #t)
              ((surrogate-code? (car codes)) #f)
              (else (loop (cdr codes)))
        ) ;cond
      ) ;let
    ) ;define
    (define (idn-email-address? value)
      (and (valid-scalars? value)
        (> (string-length value) 0)
        (let ((at (find-at value)))
          (if (not at)
            #f
            (let ((local (substring value 0 at))
                  (domain (substring value (+ at 1) (string-length value)))
                 ) ;
              (and (> (string-length local) 0)
                (> (string-length domain) 0)
                (not
                  (char=? (string-ref local (- (string-length local) 1)) #\.)
                ) ;not
                (not
                  (char=? (string-ref domain (- (string-length domain) 1)) #\.)
                ) ;not
              ) ;and
            ) ;let
          ) ;if
        ) ;let
      ) ;and
    ) ;define
  ) ;begin
) ;define-library

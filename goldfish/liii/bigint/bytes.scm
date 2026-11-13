;;; liii/bigint/bytes.scm - bigint 字节向量转换
;;;
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

(define-library (liii bigint bytes)
  (import (scheme base) (liii error) (liii bigint record) (liii bigint string))

  (export bytes->bigint bigint->bytes)

  (begin
    (define (drop-while pred lst)
      (if (null? lst) '() (if (pred (car lst)) (drop-while pred (cdr lst)) lst))
    ) ;define

    (define (bytevector->u8-list bytes)
      (let loop
        ((index 0) (result '()))
        (if (= index (bytevector-length bytes))
          (reverse result)
          (loop (+ index 1) (cons (bytevector-u8-ref bytes index) result))
        ) ;if
      ) ;let
    ) ;define

    (define (u8-list->bytevector bytes)
      (let ((result (make-bytevector (length bytes) 0)))
        (let loop
          ((index 0) (rest bytes))
          (if (null? rest)
            result
            (begin
              (bytevector-u8-set! result index (car rest))
              (loop (+ index 1) (cdr rest))
            ) ;begin
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (normalize-byte-order bytes big-endian?)
      (if big-endian? bytes (reverse bytes))
    ) ;define

    (define (byte->hex byte)
      (string (digit->char (quotient byte 16)) (digit->char (remainder byte 16)))
    ) ;define

    (define (bytes->hex-string bytes)
      (let loop
        ((rest bytes) (parts '()))
        (if (null? rest)
          (apply string-append (reverse parts))
          (loop (cdr rest) (cons (byte->hex (car rest)) parts))
        ) ;if
      ) ;let
    ) ;define

    (define (hex-string->bytes hex)
      (let* ((len (string-length hex))
             (normalized (if (odd? len) (string-append "0" hex) hex))
             (norm-len (string-length normalized))
            ) ;
        (let loop
          ((index 0) (result '()))
          (if (= index norm-len)
            (reverse result)
            (let* ((hi (char->digit (string-ref normalized index)))
                   (lo (char->digit (string-ref normalized (+ index 1))))
                  ) ;
              (loop (+ index 2) (cons (+ (* hi 16) lo) result))
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (add-one-to-bytes bytes)
      (let loop
        ((rest (reverse bytes)) (carry 1) (result '()))
        (if (null? rest)
          (if (= carry 0) result (cons carry result))
          (let* ((sum (+ (car rest) carry))
                 (byte (remainder sum 256))
                 (next-carry (quotient sum 256))
                ) ;
            (loop (cdr rest) next-carry (cons byte result))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (bytes->bigint bytes . maybe-big-endian)
      (let ((big-endian? (if (null? maybe-big-endian) #t (car maybe-big-endian))))
        (if (not (bytevector? bytes))
          (type-error "bytes->bigint in (liii bigint bytes): not a bytevector" bytes)
          (let ((ordered (normalize-byte-order (bytevector->u8-list bytes) big-endian?)))
            (if (null? ordered)
              (make-bigint 0 '())
              (if (< (car ordered) 128)
                (string->bigint (bytes->hex-string ordered) 16)
                (let* ((inverted (map (lambda (byte) (- 255 byte)) ordered))
                       (magnitude (add-one-to-bytes inverted))
                       (trimmed (drop-while (lambda (byte) (= byte 0)) magnitude))
                      ) ;
                  (if (null? trimmed)
                    (make-bigint 0 '())
                    (let ((positive (string->bigint (bytes->hex-string trimmed) 16)))
                      (make-bigint -1 (bigint-mag positive))
                    ) ;let
                  ) ;if
                ) ;let*
              ) ;if
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (unsigned-bigint->bytes n)
      (if (= (bigint-signum n) 0) '(0) (hex-string->bytes (bigint->string n 16)))
    ) ;define

    (define (twos-complement-negative-bytes abs-bytes)
      (let loop
        ((magnitude abs-bytes))
        (let* ((inverted (map (lambda (byte) (- 255 byte)) magnitude))
               (twos (add-one-to-bytes inverted))
              ) ;
          (if (>= (car twos) 128) twos (loop (cons 0 magnitude)))
        ) ;let*
      ) ;let
    ) ;define

    (define (bigint->bytes n . maybe-big-endian)
      (let* ((big-endian? (if (null? maybe-big-endian) #t (car maybe-big-endian)))
             (value (ensure-bigint "bigint->bytes" n))
             (signum (bigint-signum value))
             (ordered
               (cond ((= signum 0) '(0))
                     ((= signum 1)
                      (let ((bytes (unsigned-bigint->bytes value)))
                        (if (>= (car bytes) 128) (cons 0 bytes) bytes)
                      ) ;let
                     ) ;
                     (else
                       (twos-complement-negative-bytes (unsigned-bigint->bytes (make-bigint 1 (bigint-mag value)))
                       ) ;twos-complement-negative-bytes
                     ) ;else
               ) ;cond
             ) ;ordered
             (normalized (normalize-byte-order ordered big-endian?))
            ) ;
        (u8-list->bytevector normalized)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

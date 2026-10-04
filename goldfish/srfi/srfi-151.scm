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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (srfi srfi-151)
  (import (only (goldfish) lognot logand logior logxor ash) (scheme base))
  (export bitwise-not bitwise-and bitwise-ior bitwise-xor bitwise-eqv
          bitwise-nor bitwise-nand bitwise-orc1 bitwise-orc2
          bitwise-andc1 bitwise-andc2 arithmetic-shift bit-count
          integer-length bitwise-if bit-set? copy-bit bit-swap
          any-bit-set? every-bit-set? first-set-bit
          bit-field bit-field-any? bit-field-every? bit-field-clear bit-field-set
          bit-field-replace bit-field-replace-same bit-field-rotate bit-field-reverse
          bits->list list->bits bits->vector vector->bits bits
          bitwise-fold bitwise-for-each bitwise-unfold make-bitwise-generator)
  (begin
    (define bitwise-not lognot)
    (define bitwise-and logand)
    (define bitwise-ior logior)
    (define bitwise-xor logxor)
    (define arithmetic-shift ash)

    (define (check-integer value)
      (unless (and (integer? value) (exact? value))
        (error 'wrong-type-arg "bitwise operation expects an exact integer" value)))
    (define (check-index index)
      (check-integer index)
      (when (negative? index)
        (error 'out-of-range "bit index must be non-negative" index)))
    (define (check-procedure proc)
      (unless (procedure? proc)
        (error 'wrong-type-arg "bitwise operation expects a procedure" proc)))
    (define (field-mask start end)
      (check-index start)
      (check-index end)
      (when (> start end)
        (error 'out-of-range "bit field starts after its end" start end))
      (- (ash 1 (- end start)) 1))

    (define (bitwise-eqv . integers)
      (let loop ((rest integers) (result -1))
        (if (null? rest) result
          (loop (cdr rest) (lognot (logxor result (car rest)))))))
    (define (bitwise-nor i j) (lognot (logior i j)))
    (define (bitwise-nand i j) (lognot (logand i j)))
    (define (bitwise-orc1 i j) (logior (lognot i) j))
    (define (bitwise-orc2 i j) (logior i (lognot j)))
    (define (bitwise-andc1 i j) (logand (lognot i) j))
    (define (bitwise-andc2 i j) (logand i (lognot j)))

    (define (bit-count i)
      (check-integer i)
      (let loop ((value (if (negative? i) (lognot i) i)) (count 0))
        (if (zero? value) count
          (loop (logand value (- value 1)) (+ count 1)))))
    (define (integer-length i)
      (check-integer i)
      (let loop ((value (if (negative? i) (lognot i) i)) (count 0))
        (if (zero? value) count (loop (ash value -1) (+ count 1)))))
    (define (bitwise-if mask i j)
      (logior (logand mask i) (logand (lognot mask) j)))
    (define (bit-set? index i)
      (check-index index)
      (not (zero? (logand (ash i (- index)) 1))))
    (define (copy-bit index i bit)
      (check-index index)
      (unless (boolean? bit)
        (error 'wrong-type-arg "copy-bit expects a boolean" bit))
      (if (eq? bit (bit-set? index i)) i
        (let ((mask (ash 1 index)))
          (if bit (logior i mask) (logand i (lognot mask))))))
    (define (bit-swap index1 index2 i)
      (copy-bit index2 (copy-bit index1 i (bit-set? index2 i))
                      (bit-set? index1 i)))
    (define (any-bit-set? test-bits i) (not (zero? (logand test-bits i))))
    (define (every-bit-set? test-bits i) (= (logand test-bits i) test-bits))
    (define (first-set-bit i)
      (check-integer i)
      (if (zero? i) -1 (- (integer-length (logand i (- i))) 1)))

    (define (bit-field i start end)
      (let ((mask (field-mask start end))) (logand (ash i (- start)) mask)))
    (define (bit-field-any? i start end) (not (zero? (bit-field i start end))))
    (define (bit-field-every? i start end)
      (= (bit-field i start end) (field-mask start end)))
    (define (bit-field-clear i start end)
      (logand i (lognot (ash (field-mask start end) start))))
    (define (bit-field-set i start end)
      (logior i (ash (field-mask start end) start)))
    (define (bit-field-replace dest source start end)
      (let ((mask (ash (field-mask start end) start)))
        (bitwise-if mask (ash source start) dest)))
    (define (bit-field-replace-same dest source start end)
      (bitwise-if (ash (field-mask start end) start) source dest))
    (define (bit-field-rotate i count start end)
      (check-integer count)
      (let ((field (bit-field i start end)) (width (- end start)))
        (if (zero? width) i
          (let ((count (modulo count width)))
            (bit-field-replace i
              (logior (ash field count) (ash field (- count width))) start end)))))
    (define (bit-field-reverse i start end)
      (let loop ((field (bit-field i start end)) (remaining (- end start)) (result 0))
        (if (zero? remaining) (bit-field-replace i result start end)
          (loop (ash field -1) (- remaining 1)
                (logior (ash result 1) (logand field 1))))))

    (define (bits->list i . lengths)
      (check-index i)
      (when (> (length lengths) 1)
        (error 'wrong-number-of-args "bits->list expects at most two arguments"))
      (let ((len (if (null? lengths) (integer-length i) (car lengths))))
        (check-index len)
        (let loop ((value i) (remaining len) (result '()))
          (if (zero? remaining) (reverse result)
            (loop (ash value -1) (- remaining 1)
                  (cons (not (zero? (logand value 1))) result))))))
    (define (list->bits bools)
      (unless (list? bools)
        (error 'wrong-type-arg "list->bits expects a list" bools))
      (let loop ((rest bools) (mask 1) (result 0))
        (if (null? rest) result
          (begin
            (unless (boolean? (car rest))
              (error 'wrong-type-arg "list->bits expects booleans" (car rest)))
            (loop (cdr rest) (ash mask 1)
                  (if (car rest) (logior result mask) result))))))
    (define (bits->vector i . lengths) (list->vector (apply bits->list i lengths)))
    (define (vector->bits bools) (list->bits (vector->list bools)))
    (define (bits . bools) (list->bits bools))

    (define (bitwise-fold proc seed i)
      (check-procedure proc)
      (let loop ((value i) (remaining (integer-length i)) (result seed))
        (if (zero? remaining) result
          (loop (ash value -1) (- remaining 1)
                (proc (not (zero? (logand value 1))) result)))))
    (define (bitwise-for-each proc i)
      (check-procedure proc)
      (bitwise-fold (lambda (bit ignored) (proc bit)) #f i)
      (if #f #f))
    (define (bitwise-unfold stop? mapper successor seed)
      (check-procedure stop?)
      (check-procedure mapper)
      (check-procedure successor)
      (let loop ((state seed) (mask 1) (result 0))
        (if (stop? state) result
          (let ((bit (mapper state)))
            (loop (successor state) (ash mask 1)
                  (if bit (logior result mask) result))))))
    (define (make-bitwise-generator i)
      (check-integer i)
      (lambda ()
        (let ((bit (not (zero? (logand i 1)))))
          (set! i (ash i -1))
          bit)))))

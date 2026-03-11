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

(define-library (srfi srfi-143)
  (import (liii base)
          (liii error)
          (srfi srfi-151))
  (export
    ; Constants
    fx-width
    fx-greatest
    fx-least

    ; Predicates
    fixnum?
    fx=? 
    fx<? 
    fx>? 
    fx<=? 
    fx>=?
    fxzero? 
    fxpositive? 
    fxnegative? 
    fxodd? 
    fxeven?
    fxmax 
    fxmin

    ; Basic arithmetic
    fx+ 
    fx- 
    fxneg 
    fx* 
    fxquotient 
    fxremainder 
    fxabs 
    fxsquare 
    fxsqrt

    ; Arithmetic with carry
    fx+/carry 
    fx-/carry 
    fx*/carry

    ; Bitwise operations
    fxnot 
    fxand 
    fxior 
    fxxor
    fxarithmetic-shift 
    fxarithmetic-shift-left 
    fxarithmetic-shift-right
    fxbit-count 
    fxlength 
    fxif 
    fxbit-set? 
    fxcopy-bit
    fxfirst-set-bit 
    fxbit-field 
    fxbit-field-rotate 
    fxbit-field-reverse)
  (begin

    (define fx-greatest (*s7* 'most-positive-fixnum))
    (define fx-least (*s7* 'most-negative-fixnum))
    (define fx-width (+ (integer-length fx-greatest) 1))

    (define (fixnum? obj)
      (and (exact-integer? obj)
           (<= fx-least obj fx-greatest)))

    (define (fx-assert name obj)
      (unless (fixnum? obj)
        (error 'type-error (string-append name ": expected fixnum") obj)))

    (define (fx-assert-all name args)
      (for-each (lambda (x) (fx-assert name x)) args))

    (define (fx-ensure name value)
      (if (fixnum? value)
          value
          (error 'out-of-range (string-append name ": result not a fixnum") value)))

    (define (fx-check-index name index)
      (fx-assert name index)
      (when (or (< index 0) (>= index fx-width))
        (error 'out-of-range (string-append name ": index out of range") index)))

    (define (fx-check-range name start end)
      (fx-assert name start)
      (fx-assert name end)
      (when (or (< start 0) (< end 0) (> start end) (> end fx-width))
        (error 'out-of-range (string-append name ": invalid start/end") start end)))

    (define (fx-field-mask width)
      (cond
        ((<= width 0) 0)
        ((>= width fx-width) -1)
        ((= width (- fx-width 1)) fx-greatest)
        (else (- (arithmetic-shift 1 width) 1))))

    (define (fx-extract-field i start end)
      (let* ((width (- end start))
             (mask (fx-field-mask width)))
        (bitwise-and (fx-safe-ash i (- start)) mask)))

    (define (fx-insert-field i field start width)
      (let* ((mask (fx-field-mask width))
             (mask-shifted (arithmetic-shift mask start))
             (cleared (bitwise-and i (bitwise-not mask-shifted)))
             (field-shifted (arithmetic-shift (bitwise-and field mask) start)))
        (bitwise-ior cleared field-shifted)))

    (define (fx-add-carry a b)
      (let ((r (+ a b)))
        (cond
          ((and (>= a 0) (>= b 0) (< r 0)) (values r 1))
          ((and (< a 0) (< b 0) (>= r 0)) (values r -1))
          (else (values r 0)))))

    (define (fx-sub-carry a b)
      (let ((r (- a b)))
        (cond
          ((and (>= a 0) (< b 0) (< r 0)) (values r 1))
          ((and (< a 0) (>= b 0) (>= r 0)) (values r -1))
          (else (values r 0)))))

    (define (fx-safe-ash value count)
      (let loop ((v value) (c count))
        (cond
          ((= c 0) v)
          ((> c 62) (loop (arithmetic-shift v 62) (- c 62)))
          ((< c -62) (loop (arithmetic-shift v -62) (+ c 62)))
          (else (arithmetic-shift v c)))))

    (define (fx=? . args)
      (fx-assert-all "fx=?" args)
      (apply = args))

    (define (fx<? . args)
      (fx-assert-all "fx<?" args)
      (apply < args))

    (define (fx>? . args)
      (fx-assert-all "fx>?" args)
      (apply > args))

    (define (fx<=? . args)
      (fx-assert-all "fx<=?" args)
      (apply <= args))

    (define (fx>=? . args)
      (fx-assert-all "fx>=?" args)
      (apply >= args))

    (define (fxzero? i)
      (fx-assert "fxzero?" i)
      (zero? i))

    (define (fxpositive? i)
      (fx-assert "fxpositive?" i)
      (positive? i))

    (define (fxnegative? i)
      (fx-assert "fxnegative?" i)
      (negative? i))

    (define (fxodd? i)
      (fx-assert "fxodd?" i)
      (odd? i))

    (define (fxeven? i)
      (fx-assert "fxeven?" i)
      (even? i))

    (define (fxmax . args)
      (fx-assert-all "fxmax" args)
      (fx-ensure "fxmax" (apply max args)))

    (define (fxmin . args)
      (fx-assert-all "fxmin" args)
      (fx-ensure "fxmin" (apply min args)))

    (define (fx+ i j)
      (fx-assert-all "fx+" (list i j))
      (fx-ensure "fx+" (+ i j)))

    (define (fx- i j)
      (fx-assert-all "fx-" (list i j))
      (fx-ensure "fx-" (- i j)))

    (define (fxneg i)
      (fx-assert "fxneg" i)
      (fx-ensure "fxneg" (- i)))

    (define (fx* i j)
      (fx-assert-all "fx*" (list i j))
      (fx-ensure "fx*" (* i j)))

    (define (fxquotient i j)
      (fx-assert-all "fxquotient" (list i j))
      (fx-ensure "fxquotient" (quotient i j)))

    (define (fxremainder i j)
      (fx-assert-all "fxremainder" (list i j))
      (fx-ensure "fxremainder" (remainder i j)))

    (define (fxabs i)
      (fx-assert "fxabs" i)
      (fx-ensure "fxabs" (abs i)))

    (define (fxsquare i)
      (fx-assert "fxsquare" i)
      (fx-ensure "fxsquare" (* i i)))

    (define (fxsqrt i)
      (fx-assert "fxsqrt" i)
      (receive (s r) (exact-integer-sqrt i)
        (values (fx-ensure "fxsqrt" s)
                (fx-ensure "fxsqrt" r))))

    (define (fx+/carry i j k)
      (fx-assert-all "fx+/carry" (list i j k))
      (call-with-values
        (lambda () (fx-add-carry i j))
        (lambda (s1 c1)
          (call-with-values
            (lambda () (fx-add-carry s1 k))
            (lambda (s2 c2)
              (values (fx-ensure "fx+/carry" s2)
                      (fx-ensure "fx+/carry" (+ c1 c2))))))))

    (define (fx-/carry i j k)
      (fx-assert-all "fx-/carry" (list i j k))
      (call-with-values
        (lambda () (fx-sub-carry i j))
        (lambda (s1 c1)
          (call-with-values
            (lambda () (fx-sub-carry s1 k))
            (lambda (s2 c2)
              (values (fx-ensure "fx-/carry" s2)
                      (fx-ensure "fx-/carry" (+ c1 c2))))))))

    (define (fx*/carry i j k)
      (fx-assert-all "fx*/carry" (list i j k))
      (let ((p (* i j)))
        (call-with-values
          (lambda () (fx-add-carry p k))
          (lambda (s1 c1)
            (values (fx-ensure "fx*/carry" s1)
                    (fx-ensure "fx*/carry" c1))))))

    (define (fxnot i)
      (fx-assert "fxnot" i)
      (fx-ensure "fxnot" (bitwise-not i)))

    (define (fxand . args)
      (fx-assert-all "fxand" args)
      (let ((result (if (null? args) -1 (apply bitwise-and args))))
        (fx-ensure "fxand" result)))

    (define (fxior . args)
      (fx-assert-all "fxior" args)
      (let ((result (if (null? args) 0 (apply bitwise-ior args))))
        (fx-ensure "fxior" result)))

    (define (fxxor . args)
      (fx-assert-all "fxxor" args)
      (let ((result (if (null? args) 0 (apply bitwise-xor args))))
        (fx-ensure "fxxor" result)))

    (define (fxarithmetic-shift i count)
      (fx-assert-all "fxarithmetic-shift" (list i count))
      (when (> (abs count) (- fx-width 1))
        (error 'out-of-range "fxarithmetic-shift: count exceeds fixnum width" count))
      (fx-ensure "fxarithmetic-shift" (fx-safe-ash i count)))

    (define (fxarithmetic-shift-left i count)
      (fx-assert-all "fxarithmetic-shift-left" (list i count))
      (when (< count 0)
        (error 'out-of-range "fxarithmetic-shift-left: count must be non-negative" count))
      (when (> count (- fx-width 1))
        (error 'out-of-range "fxarithmetic-shift-left: count exceeds fixnum width" count))
      (fx-ensure "fxarithmetic-shift-left" (fx-safe-ash i count)))

    (define (fxarithmetic-shift-right i count)
      (fx-assert-all "fxarithmetic-shift-right" (list i count))
      (when (< count 0)
        (error 'out-of-range "fxarithmetic-shift-right: count must be non-negative" count))
      (when (> count (- fx-width 1))
        (error 'out-of-range "fxarithmetic-shift-right: count exceeds fixnum width" count))
      (fx-ensure "fxarithmetic-shift-right" (fx-safe-ash i (- count))))

    (define (fxbit-count i)
      (fx-assert "fxbit-count" i)
      (fx-ensure "fxbit-count" (bit-count i)))

    (define (fxlength i)
      (fx-assert "fxlength" i)
      (fx-ensure "fxlength" (integer-length i)))

    (define (fxif mask i j)
      (fx-assert-all "fxif" (list mask i j))
      (fx-ensure "fxif" (bitwise-if mask i j)))

    (define (fxbit-set? index i)
      (fx-check-index "fxbit-set?" index)
      (fx-assert "fxbit-set?" i)
      (if (= index (- fx-width 1))
          (negative? i)
          (let ((mask (arithmetic-shift 1 index)))
            (not (zero? (bitwise-and i mask))))))

    (define (fxcopy-bit index i boolean)
      (fx-check-index "fxcopy-bit" index)
      (fx-assert "fxcopy-bit" i)
      (unless (boolean? boolean)
        (error 'type-error "fxcopy-bit: boolean must be #t or #f" boolean))
      (if (= index (- fx-width 1))
          (fx-ensure "fxcopy-bit"
                     (if boolean
                         (bitwise-ior i fx-least)
                         (bitwise-and i fx-greatest)))
          (let ((mask (arithmetic-shift 1 index)))
            (fx-ensure "fxcopy-bit"
                       (if boolean
                           (bitwise-ior i mask)
                           (bitwise-and i (bitwise-not mask)))))))

    (define (fxfirst-set-bit i)
      (fx-assert "fxfirst-set-bit" i)
      (fx-ensure "fxfirst-set-bit" (first-set-bit i)))

    (define (fxbit-field i start end)
      (fx-assert "fxbit-field" i)
      (fx-check-range "fxbit-field" start end)
      (let* ((width (- end start)))
        (if (<= width 0)
            0
            (fx-ensure "fxbit-field"
                       (fx-extract-field i start end)))))

    (define (fxbit-field-rotate i count start end)
      (fx-assert-all "fxbit-field-rotate" (list i count start end))
      (fx-check-range "fxbit-field-rotate" start end)
      (let* ((width (- end start)))
        (if (<= width 0)
            i
            (let* ((field (fx-extract-field i start end))
                   (rot (modulo count width)))
              (if (= rot 0)
                  i
                  (let* ((mask (fx-field-mask width))
                         (left (bitwise-and (fx-safe-ash field rot) mask))
                         (right (fx-safe-ash field (- rot width)))
                         (rotated (bitwise-ior left right))
                         (res (fx-insert-field i rotated start width)))
                    (fx-ensure "fxbit-field-rotate" res)))))))

    (define (fxbit-field-reverse i start end)
      (fx-assert-all "fxbit-field-reverse" (list i start end))
      (fx-check-range "fxbit-field-reverse" start end)
      (let* ((width (- end start)))
        (if (<= width 1)
            i
            (let* ((field (fx-extract-field i start end))
                   (rev (let loop ((n field) (k width) (acc 0))
                          (if (= k 0)
                              acc
                              (loop (arithmetic-shift n -1)
                                    (- k 1)
                                    (bitwise-ior (arithmetic-shift acc 1)
                                                 (bitwise-and n 1))))))
                   (res (fx-insert-field i rev start width)))
              (fx-ensure "fxbit-field-reverse" res)))))

  ) ; end of begin
 ) ; end of define-library

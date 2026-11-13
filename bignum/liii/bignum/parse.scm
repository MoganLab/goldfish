;;; liii/bignum/parse.scm - bignum 字符串解析（由 liii/bignum.scm include）
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

(define (bn:parse-prefixes str default-radix)
  (let loop
    ((i 0) (n (string-length str)) (exactness #f) (radix #f))
    (if (and (native:< (native:+ i 1) n) (char=? (string-ref str i) #\#))
      (let ((c (string-ref str (native:+ i 1))))
        (cond
         ((memv c '(#\e #\E))
          (if exactness (values 'invalid #f #f) (loop (native:+ i 2) n 'exact radix))
         ) ;
         ((memv c '(#\i #\I))
          (if exactness (values 'invalid #f #f) (loop (native:+ i 2) n 'inexact radix))
         ) ;
         ((memv c '(#\b #\B))
          (if radix (values 'invalid #f #f) (loop (native:+ i 2) n exactness 2))
         ) ;
         ((memv c '(#\o #\O))
          (if radix (values 'invalid #f #f) (loop (native:+ i 2) n exactness 8))
         ) ;
         ((memv c '(#\d #\D))
          (if radix (values 'invalid #f #f) (loop (native:+ i 2) n exactness 10))
         ) ;
         ((memv c '(#\x #\X))
          (if radix (values 'invalid #f #f) (loop (native:+ i 2) n exactness 16))
         ) ;
         (else (values 'invalid #f #f))
        ) ;cond
      ) ;let
      (values exactness (or radix default-radix) (substring str i n))
    ) ;if
  ) ;let
) ;define

(define (bn:digit-for-radix? ch radix)
  (let ((c (char->integer ch)))
    (cond
     ((and (not (native:< c 48)) (not (native:> c 57)))
      (native:< (native:- c 48) radix)
     ) ;
     ((and (native:> radix 10) (not (native:< c 97)) (not (native:> c 122)))
      (native:< (native:- c 87) radix)
     ) ;
     ((and (native:> radix 10) (not (native:< c 65)) (not (native:> c 90)))
      (native:< (native:- c 55) radix)
     ) ;
     (else #f)
    ) ;cond
  ) ;let
) ;define

(define (bn:valid-int-str? s radix)
  (let ((n (string-length s)))
    (if (native:= n 0)
      #f
      (let* ((c0 (string-ref s 0)) (start (if (or (char=? c0 #\+) (char=? c0 #\-)) 1 0)))
        (if (native:= start n)
          #f
          (let loop
            ((i start))
            (cond ((native:= i n) #t)
                  ((bn:digit-for-radix? (string-ref s i) radix) (loop (native:+ i 1)))
                  (else #f)
            ) ;cond
          ) ;let
        ) ;if
      ) ;let*
    ) ;if
  ) ;let
) ;define

(define (bn:parse-exact-int s radix)
  (if (bn:valid-int-str? s radix) (bn:demote-bigint (string->bigint s radix)) #f)
) ;define

(define (bn:string-index str ch)
  (let ((n (string-length str)))
    (let loop
      ((i 0))
      (cond ((native:= i n) #f)
            ((char=? (string-ref str i) ch) i)
            (else (loop (native:+ i 1)))
      ) ;cond
    ) ;let
  ) ;let
) ;define

(define (bn:all-digits? str start end)
  (let loop
    ((i start))
    (cond ((native:= i end) #t)
          ((and (char>=? (string-ref str i) #\0) (char<=? (string-ref str i) #\9))
           (loop (native:+ i 1))
          ) ;
          (else #f)
    ) ;cond
  ) ;let
) ;define

(define (bn:scan-digits str start n)
  (let loop
    ((i start))
    (if (and (native:< i n)
          (char>=? (string-ref str i) #\0)
          (char<=? (string-ref str i) #\9)
        ) ;and
      (loop (native:+ i 1))
      i
    ) ;if
  ) ;let
) ;define

(define (bn:parse-exp-part s j n)
  (cond ((native:= j n) 0)
        ((not (memv (string-ref s j) '(#\e #\E))) #f)
        ((native:= (native:+ j 1) n) #f)
        (else
          (let* ((k (native:+ j 1))
                 (ec (string-ref s k))
                 (es (if (or (char=? ec #\+) (char=? ec #\-)) (native:+ k 1) k))
                ) ;
            (if (and (native:< es n) (bn:all-digits? s es n))
              (native:string->number (substring s k n))
              #f
            ) ;if
          ) ;let*
        ) ;else
  ) ;cond
) ;define

(define (bn:build-decimal neg? digits frac-len exp-val)
  (let* ((mant (string->bigint digits 10))
         (signed-mant (if neg? (bigint-negate mant) mant))
         (net-exp (native:- exp-val frac-len))
        ) ;
    (if (native:< net-exp 0)
      (bn:div-2 signed-mant (bigint-expt (integer->bigint 10) (native:- net-exp)))
      (bn:demote-bigint (bigint-multiply signed-mant (bigint-expt (integer->bigint 10) net-exp))
      ) ;bn:demote-bigint
    ) ;if
  ) ;let*
) ;define

(define (bn:parse-exact-decimal s)
  (let ((n (string-length s)))
    (if (native:= n 0)
      #f
      (let* ((c0 (string-ref s 0))
             (neg? (char=? c0 #\-))
             (pos0 (if (or neg? (char=? c0 #\+)) 1 0))
             (int-end (bn:scan-digits s pos0 n))
             (has-dot? (and (native:< int-end n) (char=? (string-ref s int-end) #\.)))
             (frac-start (if has-dot? (native:+ int-end 1) int-end))
             (frac-end (bn:scan-digits s frac-start n))
             (int-len (native:- int-end pos0))
             (frac-len (native:- frac-end frac-start))
             (exp-val (if (or (native:> int-len 0) (native:> frac-len 0))
                        (bn:parse-exp-part s frac-end n)
                        #f
                      ) ;if
             ) ;exp-val
            ) ;
        (if (not exp-val)
          #f
          (bn:build-decimal neg?
            (string-append (substring s pos0 int-end) (substring s frac-start frac-end))
            frac-len
            exp-val
          ) ;bn:build-decimal
        ) ;if
      ) ;let*
    ) ;if
  ) ;let
) ;define

(define (bn:parse-rational s radix)
  (let ((slash (bn:string-index s #\/)))
    (if (not slash)
      #f
      (let* ((num-str (substring s 0 slash))
             (den-str (substring s (native:+ slash 1) (string-length s)))
            ) ;
        (if (or (native:= (string-length num-str) 0)
              (native:= (string-length den-str) 0)
              (memv (string-ref den-str 0) '(#\+ #\-))
            ) ;or
          #f
          (let ((num (bn:parse-exact-int num-str radix))
                (den (bn:parse-exact-int den-str radix))
               ) ;
            (if (and num den (not (bn:zero? den))) (bn:div-2 num den) #f)
          ) ;let
        ) ;if
      ) ;let*
    ) ;if
  ) ;let
) ;define

(define (bn:find-complex-split s len)
  (let loop
    ((i (native:- len 1)))
    (cond ((not (native:> i 0)) #f)
          ((memv (string-ref s i) '(#\+ #\-))
           (let ((prev (string-ref s (native:- i 1))))
             (if (memv prev '(#\e #\E #\/)) (loop (native:- i 1)) i)
           ) ;let
          ) ;
          (else (loop (native:- i 1)))
    ) ;cond
  ) ;let
) ;define

(define (bn:parse-real-part s radix exactness)
  (cond ((bn:parse-exact-int s radix) => (lambda (v) v))
        ((bn:parse-rational s radix) => (lambda (v) v))
        ((and (eq? exactness 'exact) (native:= radix 10)) (bn:parse-exact-decimal s))
        ((native:= radix 10)
         (let ((n (native:string->number s 10)))
           (if (and n (native:real? n)) n #f)
         ) ;let
        ) ;
        (else #f)
  ) ;cond
) ;define

(define (bn:parse-imag-part s radix exactness)
  (cond ((string=? s "+") 1)
        ((string=? s "-") -1)
        (else (bn:parse-real-part s radix exactness))
  ) ;cond
) ;define

(define (bn:parse-complex s radix exactness)
  (let ((n (string-length s)))
    (if
      (or (native:= n 0) (not (memv (string-ref s (native:- n 1)) '(#\i #\I))))
      #f
      (let* ((body (substring s 0 (native:- n 1))) (m (string-length body)))
        (cond ((native:= m 0) (bn:make-complex 0 1))
              ((string=? body "+") (bn:make-complex 0 1))
              ((string=? body "-") (bn:make-complex 0 -1))
              ((bn:find-complex-split body m)
               =>
               (lambda (k)
                 (let ((r (bn:parse-real-part (substring body 0 k) radix exactness))
                       (im (bn:parse-imag-part (substring body k m) radix exactness))
                      ) ;
                   (if (and r im) (bn:make-complex r im) #f)
                 ) ;let
               ) ;lambda
              ) ;
              (else
                (let ((im (bn:parse-imag-part body radix exactness)))
                  (if im (bn:make-complex 0 im) #f)
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;if
  ) ;let
) ;define

(define (bn:apply-exactness val exactness)
  (cond ((not val) #f)
        ((eq? exactness 'exact) (bn:exact val))
        ((eq? exactness 'inexact) (bn:inexact val))
        (else val)
  ) ;cond
) ;define

(define (bn:string->number str . rest)
  (let ((default-radix (if (null? rest) 10 (car rest))))
    (let-values (((exactness radix body) (bn:parse-prefixes str default-radix)))
      (cond ((eq? exactness 'invalid) #f)
            ((bn:parse-exact-int body radix)
             =>
             (lambda (v) (bn:apply-exactness v exactness))
            ) ;
            ((bn:parse-rational body radix)
             =>
             (lambda (v) (bn:apply-exactness v exactness))
            ) ;
            ((bn:parse-complex body radix exactness)
             =>
             (lambda (v) (bn:apply-exactness v exactness))
            ) ;
            ((and (eq? exactness 'exact) (native:= radix 10))
             (or (bn:parse-exact-decimal body) (apply native:string->number str rest))
            ) ;
            ((apply native:string->number str rest) => (lambda (v) v))
            ((native:= radix 10)
             (bn:apply-exactness (bn:parse-exact-decimal body) exactness)
            ) ;
            (else #f)
      ) ;cond
    ) ;let-values
  ) ;let
) ;define

(define (bn:read-literal str radix)
  (let-values (((exactness r body) (bn:parse-prefixes str radix)))
    (cond ((eq? exactness 'invalid) #f)
          ((bn:parse-exact-int body r) => (lambda (v) (bn:apply-exactness v exactness)))
          ((bn:parse-rational body r) => (lambda (v) (bn:apply-exactness v exactness)))
          ((bn:parse-complex body r exactness)
           =>
           (lambda (v) (bn:apply-exactness v exactness))
          ) ;
          ((and (eq? exactness 'exact) (native:= r 10)) (bn:parse-exact-decimal body))
          ((and (or (eq? exactness 'inexact) (eq? exactness 'exact)) (native:= r 10))
           (or (bn:apply-exactness (native:string->number body r) exactness)
             (bn:apply-exactness (bn:parse-exact-decimal body) exactness)
           ) ;or
          ) ;
          ((and (not exactness) (native:= r 10)) (bn:parse-exact-decimal body))
          (else #f)
    ) ;cond
  ) ;let-values
) ;define

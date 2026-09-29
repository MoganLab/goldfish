(define-library (liii base64)
  (import (goldfish))
  (import (scheme base) (liii base) (liii bitwise) (liii error))
  (export string-base64-encode
    bytevector-base64-encode
    base64-encode
    string-base64-decode
    bytevector-base64-decode
    base64-decode
  ) ;export
  (begin
    (define base64-alphabet
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

    (define bytevector-base64-encode
      (typed-lambda ((bv bytevector?))
        (let* ((length (bytevector-length bv))
               (output-length (if (= length 0) 0 (* 4 (quotient (+ length 2) 3))))
               (output (make-bytevector output-length 0)))
          (define (emit! index value)
            (bytevector-u8-set! output index value))
          (define (emit-digit! index value)
            (emit! index (char->integer (string-ref base64-alphabet value))))
          (let loop ((input-index 0) (output-index 0))
            (if (>= input-index length)
              output
              (let* ((remaining (- length input-index))
                     (a (bytevector-u8-ref bv input-index))
                     (b (if (> remaining 1)
                          (bytevector-u8-ref bv (+ input-index 1))
                          0))
                     (c (if (> remaining 2)
                          (bytevector-u8-ref bv (+ input-index 2))
                          0)))
                (emit-digit! output-index (quotient a 4))
                (emit-digit! (+ output-index 1)
                  (+ (* (remainder a 4) 16) (quotient b 16)))
                (if (> remaining 1)
                  (emit-digit! (+ output-index 2)
                    (+ (* (remainder b 16) 4) (quotient c 64)))
                  (emit! (+ output-index 2) 61))
                (if (> remaining 2)
                  (emit-digit! (+ output-index 3) (remainder c 64))
                  (emit! (+ output-index 3) 61))
                (loop (+ input-index 3) (+ output-index 4))))))
      ) ;typed-lambda
    ) ;define

    (define string-base64-encode
      (typed-lambda ((str string?))
        (utf8->string (bytevector-base64-encode (string->utf8 str)))
      ) ;typed-lambda
    ) ;define

    (define (base64-encode x)
      (cond ((string? x) (string-base64-encode x))
            ((bytevector? x) (bytevector-base64-encode x))
            (else (type-error "input must be string or bytevector"))
      ) ;cond
    ) ;define

    (define (bytevector-base64-decode bv)
      (if (not (bytevector? bv))
        (type-error "bytevector-base64-decode: expected a bytevector" bv)
        (let ((length (bytevector-length bv)))
          (if (not (= (remainder length 4) 0))
            (value-error "bytevector-base64-decode: input length must be a multiple of 4")
            (let* ((groups (quotient length 4))
                   (padding (if (= length 0) 0
                              (let ((last (- length 1)))
                                (if (= (bytevector-u8-ref bv last) 61)
                                  (if (and (> length 1)
                                           (= (bytevector-u8-ref bv (- last 1)) 61))
                                    2 1)
                                  0))))
                   (output (make-bytevector (- (* groups 3) padding) 0)))
              (define (digit value)
                (cond ((and (>= value 65) (<= value 90)) (- value 65))
                      ((and (>= value 97) (<= value 122)) (+ 26 (- value 97)))
                      ((and (>= value 48) (<= value 57)) (+ 52 (- value 48)))
                      ((= value 43) 62)
                      ((= value 47) 63)
                      (else -1)))
              (let loop ((group 0) (output-index 0))
                (if (= group groups)
                  output
                  (let* ((input-index (* group 4))
                         (c1 (bytevector-u8-ref bv input-index))
                         (c2 (bytevector-u8-ref bv (+ input-index 1)))
                         (c3 (bytevector-u8-ref bv (+ input-index 2)))
                         (c4 (bytevector-u8-ref bv (+ input-index 3)))
                         (last-group (= group (- groups 1)))
                         (pad3 (= c3 61))
                         (pad4 (= c4 61))
                         (v1 (digit c1)) (v2 (digit c2))
                         (v3 (if pad3 0 (digit c3)))
                         (v4 (if pad4 0 (digit c4))))
                    (if (or (= c1 61) (= c2 61)
                            (and pad3 (not pad4))
                            (and (or pad3 pad4) (not last-group))
                            (< v1 0) (< v2 0) (< v3 0) (< v4 0))
                      (value-error "bytevector-base64-decode: invalid base64 input")
                      (begin
                        (bytevector-u8-set! output output-index
                          (+ (* v1 4) (quotient v2 16)))
                        (unless pad3
                          (bytevector-u8-set! output (+ output-index 1)
                            (+ (* (remainder v2 16) 16) (quotient v3 4))))
                        (unless pad4
                          (bytevector-u8-set! output (+ output-index 2)
                            (+ (* (remainder v3 4) 64) v4)))
                        (loop (+ group 1)
                          (+ output-index (if pad3 1 (if pad4 2 3)))))))))))))
    ) ;define

    (define string-base64-decode
      (typed-lambda ((str string?))
        (utf8->string (bytevector-base64-decode (string->utf8 str)))
      ) ;typed-lambda
    ) ;define

    (define (base64-decode x)
      (cond ((string? x) (string-base64-decode x))
            ((bytevector? x) (bytevector-base64-decode x))
            (else (type-error "input must be string or bytevector"))
      ) ;cond
    ) ;define
  ) ;begin
) ;define-library

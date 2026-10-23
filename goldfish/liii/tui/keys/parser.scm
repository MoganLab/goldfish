;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(define-library (liii tui keys parser)
  (export parse-key parse-keys)
  (import (scheme base)
    (liii base)
    (liii error)
    (liii unicode)
    (liii tui keys table)
  ) ;import
  (begin

    (define (%split-semicolon str)
      (let ((len (string-length str)))
        (let loop
          ((k 0))
          (cond ((>= k len) (list str))
                ((char=? (string-ref str k) #\;)
                 (list (substring str 0 k) (substring str (+ k 1) len))
                ) ;
                (else (loop (+ k 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%combine-mod-key base-k mod-code)
      (if (eq? base-k 'shift-tab)
        'shift-tab
        (let ((prefix (if mod-code (%modifier-prefix mod-code) "")))
          (if (and base-k (> (string-length prefix) 0))
            (string->symbol (string-append prefix (symbol->string base-k)))
            (or base-k 'escape)
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%resolve-csi-tilde param-str next-i)
      (let ((parts (%split-semicolon param-str)))
        (if (= (length parts) 1)
          (let ((k (%tilde-key (car parts))))
            (values (or k 'escape) next-i)
          ) ;let
          (let* ((code-str (car parts))
                 (mod-code (string->number (cadr parts)))
                 (base-k (%tilde-key code-str))
                ) ;
            (values (%combine-mod-key base-k mod-code) next-i)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%resolve-csi-alpha param-str term-byte next-i)
      (let ((parts (%split-semicolon param-str)))
        (cond
         ((= term-byte 117)
          (let ((code (and (pair? parts) (string->number (car parts))))
                (mod-code (and (pair? (cdr parts)) (string->number (cadr parts))))
               ) ;
            (cond ((and code (= code 9) mod-code (= mod-code 2)) (values 'shift-tab next-i))
                  ((and code (= code 13) mod-code (= mod-code 2)) (values 'shift-enter next-i))
                  ((and code (= code 13) mod-code (= mod-code 5)) (values 'ctrl-enter next-i))
                  ((and code (= code 10)) (values 'ctrl-j next-i))
                  (else (values 'escape next-i))
            ) ;cond
          ) ;let
         ) ;
         ((or (= (string-length param-str) 0) (= (length parts) 1))
          (let ((k (%csi-final-key term-byte)))
            (values (or k 'escape) next-i)
          ) ;let
         ) ;
         (else
           (let* ((mod-code (string->number (cadr parts))) (base-k (%csi-final-key term-byte)))
             (values (%combine-mod-key base-k mod-code) next-i)
           ) ;let*
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%scan-csi-term bv start len)
      (let loop
        ((j start))
        (if (>= j len)
          #f
          (let ((b (bytevector-u8-ref bv j)))
            (if (and (>= b 64) (<= b 126)) j (loop (+ j 1)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%parse-csi-body bv i len)
      (let ((term-idx (%scan-csi-term bv (+ i 2) len)))
        (if (not term-idx)
          (values 'escape (+ i 1))
          (let* ((param-bv (bytevector-copy bv (+ i 2) term-idx))
                 (param-str (utf8->string param-bv))
                 (term-byte (bytevector-u8-ref bv term-idx))
                 (next-i (+ term-idx 1))
                ) ;
            (if (= term-byte 126)
              (%resolve-csi-tilde param-str next-i)
              (%resolve-csi-alpha param-str term-byte next-i)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%parse-csi bv i len)
      (if (>= (+ i 2) len)
        (values 'escape (+ i 1))
        (let ((b2 (bytevector-u8-ref bv (+ i 2))))
          (if (= b2 91)
            (if (< (+ i 3) len)
              (let* ((b3 (bytevector-u8-ref bv (+ i 3))) (fn (%linux-fn-key b3)))
                (if fn (values fn (+ i 4)) (values 'escape (+ i 1)))
              ) ;let*
              (values 'escape (+ i 1))
            ) ;if
            (%parse-csi-body bv i len)
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%parse-ss3 bv i len)
      (if (< (+ i 2) len)
        (let* ((b2 (bytevector-u8-ref bv (+ i 2))) (k (%ss3-key b2)))
          (if k (values k (+ i 3)) (values 'escape (+ i 1)))
        ) ;let*
        (values 'escape (+ i 1))
      ) ;if
    ) ;define

    (define (%parse-alt-enter bv i len)
      (if
        (and (< (+ i 2) len) (= (bytevector-u8-ref bv (+ i 2)) 10))
        (values 'alt-enter (+ i 3))
        (values 'alt-enter (+ i 2))
      ) ;if
    ) ;define

    (define (%parse-escape bv i len)
      (if (>= (+ i 1) len)
        (values 'escape (+ i 1))
        (let ((b1 (bytevector-u8-ref bv (+ i 1))))
          (cond ((= b1 91) (%parse-csi bv i len))
                ((= b1 79) (%parse-ss3 bv i len))
                ((= b1 27) (values 'escape (+ i 1)))
                ((= b1 13) (%parse-alt-enter bv i len))
                ((= b1 10) (values 'alt-enter (+ i 2)))
                ((= b1 9) (values 'alt-tab (+ i 2)))
                ((or (= b1 8) (= b1 127)) (values 'alt-backspace (+ i 2)))
                ((and (>= b1 32) (< b1 127))
                 (values
                   (string->symbol (string-append "alt-" (string (integer->char b1))))
                   (+ i 2)
                 ) ;values
                ) ;
                (else (values 'escape (+ i 1)))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%parse-step bv i len)
      (if (>= i len)
        (values #f len)
        (let ((b0 (bytevector-u8-ref bv i)))
          (cond ((= b0 27) (%parse-escape bv i len))
                ((= b0 13)
                 (if
                   (and (< (+ i 1) len) (= (bytevector-u8-ref bv (+ i 1)) 10))
                   (values 'enter (+ i 2))
                   (values 'enter (+ i 1))
                 ) ;if
                ) ;
                ((or (< b0 32) (= b0 127))
                 (let ((k (%ctrl-char-key b0)))
                   (values (or k #f) (+ i 1))
                 ) ;let
                ) ;
                ((< b0 128) (values (string (integer->char b0)) (+ i 1)))
                (else
                  (let* ((next (bytevector-advance-utf8 bv i len))
                         (step (if (= next i) (+ i 1) next))
                         (chunk (utf8->string (bytevector-copy bv i step)))
                        ) ;
                    (values chunk step)
                  ) ;let*
                ) ;else
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (parse-key str . rest)
      (unless (string? str)
        (type-error "parse-key: str must be a string")
      ) ;unless
      (let ((with-rest? (and (not (null? rest)) (car rest))))
        (if (string=? str "")
          (if with-rest? (cons #f "") #f)
          (let* ((bv (string->utf8 str)) (len (bytevector-length bv)))
            (let-values (((key next-i) (%parse-step bv 0 len)))
              (if with-rest?
                (let ((rest-str (if (>= next-i len) "" (utf8->string (bytevector-copy bv next-i len)))
                      ) ;rest-str
                     ) ;
                  (cons key rest-str)
                ) ;let
                key
              ) ;if
            ) ;let-values
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (parse-keys str)
      (unless (string? str)
        (type-error "parse-keys: str must be a string")
      ) ;unless
      (if (string=? str "")
        '()
        (let* ((bv (string->utf8 str)) (len (bytevector-length bv)))
          (let loop
            ((i 0) (acc '()))
            (if (>= i len)
              (reverse acc)
              (let-values (((key next-i) (%parse-step bv i len)))
                (let ((step (if (= next-i i) (+ i 1) next-i)))
                  (if key (loop step (cons key acc)) (loop step acc))
                ) ;let
              ) ;let-values
            ) ;if
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library

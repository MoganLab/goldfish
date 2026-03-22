;
; Copyright (C) 2024 The Goldfish Scheme Authors
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

(import (scheme time) (liii lang) (liii base))

(define (timing msg thunk)
  (let* ((start (current-jiffy))
         (val (thunk))
         (end (current-jiffy)))
    (display* msg (number->string (- end start)) "\n")))

(define (repeat n proc)
  (when (>= n 0)
        (proc)
        (repeat (- n 1) proc)))

(define (prim-sqrt data)
  (if (< data 0)
      (value-error
        (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
      (inexact->exact (floor (sqrt data)))))

(define-case-class rich-integer2 ((data integer?))
  (define (%to-string)
    (number->string data)))

(define (rint x)
  (lambda (msg . args)
    (let ((env (funclet rint)))
      (varlet env 'data x)
      (let ((r (case msg
                ((:to-string)
                 ((env '%to-string)))
                ((:to-rich-string)
                 ((env '%to-rich-string)))
                ((:sqrt)
                 ((env '%sqrt))))
        (cutlet env 'data)
        r))))

(with-let (funclet rint)
  (define (%to-string)
    (number->string data))
  (varlet (funclet rint) '%to-string %to-string)
  
  (define (%to-rich-string)
    (rich-string (%to-string)))
  (varlet (funclet rint) '%to-rich-string %to-rich-string)
  
  (define (%sqrt)
        (if (< data 0)
            (value-error
              (format #f "sqrt of negative integer is undefined!         ** Got ~a **" data))
            (inexact->exact (floor (sqrt data)))))
  (varlet (funclet rint) '%sqrt %sqrt))

(display* "Bench of number->string:\n")
(timing "prim%to-string:\t\t\t" (lambda () (repeat 10000 (lambda () (number->string 65536)))))
(timing "rich-integer%to-string:\t\t" (lambda () (repeat 10000 (lambda () ((rich-integer 65536) :to-string)))))
(timing "rich-integer2%to-string:\t" (lambda () (repeat 10000 (lambda () ((rich-integer2 65536) :to-string)))))
(timing "rint%to-string:\t\t\t" (lambda () (repeat 10000 (lambda () ((rint 65536) :to-string)))))

(display* ((rint 65535) :to-string))
(newline)

(display* "\n\nBench of SQRT:\n")
(timing "prim%sqrt:\t\t\t" (lambda () (repeat 10000 (lambda () (prim-sqrt 65536)))))
(timing "rint%sqrt:\t\t\t" (lambda () (repeat 10000 (lambda () ((rint 65536) :sqrt)))))

(display "\nBench of integer\n")
(timing "rich-integer%sqrt:\t\t" (lambda () (repeat 10000 (lambda () ((rich-integer 65536) :sqrt)))))

(display* ((rint 65535) :sqrt))

; slow because of rich-string
; (timing "rint%to-rich-string " (lambda () (repeat 1000 (lambda () (((rint 65536) :to-rich-string) :length)))))

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

(define-library (liii goldrepl eval)
  (import (scheme base) (scheme write) (liii base) (liii string))
  (export %repl-missing-parens *repl-eval-error* %repl-eval *repl-value-count*
    %repl-register-value!
  ) ;export
  (begin

    (define (%repl-missing-parens text)
      (let loop
        ((chars (string->list text))
         (depth 0)
         (in-string #f)
         (escaped #f)
         (in-comment #f)
        ) ;
        (if (null? chars)
          depth
          (let ((ch (car chars)))
            (cond (escaped (loop (cdr chars) depth in-string #f in-comment))
                  (in-string
                    (cond ((char=? ch #\\) (loop (cdr chars) depth #t #t #f))
                          ((char=? ch #\") (loop (cdr chars) depth #f #f #f))
                          (else (loop (cdr chars) depth #t #f #f))
                    ) ;cond
                  ) ;in-string
                  (in-comment (if (char=? ch #\newline)
                                (loop (cdr chars) depth #f #f #f)
                                (loop (cdr chars) depth #f #f #t)
                              ) ;if
                  ) ;in-comment
                  ((char=? ch #\;) (loop (cdr chars) depth #f #f #t))
                  ((char=? ch #\") (loop (cdr chars) depth #t #f #f))
                  ((char=? ch #\() (loop (cdr chars) (+ depth 1) #f #f #f))
                  ((char=? ch #\)) (loop (cdr chars) (- depth 1) #f #f #f))
                  (else (loop (cdr chars) depth #f #f #f))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define *repl-eval-error* (list 'eval-error))
    (define (%repl-format-error-info info)
      (if (and (pair? info) (string? (car info)))
        (apply format #f (car info) (if (pair? (cdr info)) (cdr info) '()))
        (object->string info)
      ) ;if
    ) ;define
    (define (%repl-format-eval-error tag info)
      (string-append (object->string tag) ": " (%repl-format-error-info info))
    ) ;define
    (define (%repl-collect-values text)
      (call-with-values (lambda () (eval-string text (rootlet)))
        (lambda results
          (cond ((null? results) (if #f #f))
                ((null? (cdr results)) (car results))
                (else results)
          ) ;cond
        ) ;lambda
      ) ;call-with-values
    ) ;define
    (define (%repl-eval text)
      (let* ((out-port (open-output-string))
             (err-port (open-output-string))
             (orig-out (current-output-port))
             (orig-err (current-error-port))
             (res *repl-eval-error*)
            ) ;
        (dynamic-wind (lambda () (set-current-output-port out-port) (set-current-error-port err-port))
          (lambda ()
            (set! res
              (catch #t
                (lambda () (%repl-collect-values text))
                (lambda (tag info)
                  (display (%repl-format-eval-error tag info) err-port)
                  *repl-eval-error*
                ) ;lambda
              ) ;catch
            ) ;set!
          ) ;lambda
          (lambda () (set-current-output-port orig-out) (set-current-error-port orig-err))
        ) ;dynamic-wind
        (let* ((out (get-output-string out-port))
               (err (get-output-string err-port))
               (ok? (not (eq? res *repl-eval-error*)))
              ) ;
          (list out err ok? (if ok? res #f))
        ) ;let*
      ) ;let*
    ) ;define
    (define *repl-value-count* 0)
    (define (%repl-register-value! result)
      (set! *repl-value-count* (+ *repl-value-count* 1))
      (let ((name (string->symbol (string-append "$" (number->string *repl-value-count*))))
           ) ;
        (varlet (rootlet) name result)
        name
      ) ;let
    ) ;define
  ) ;begin
) ;define-library

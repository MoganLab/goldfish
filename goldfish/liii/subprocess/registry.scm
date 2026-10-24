;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii subprocess registry)
  (export run-set! run-get run-allow! run-ban! run-unban! %check-symbol-command
    %check-string-command %resolve-symbol-command
  ) ;export
  (import (scheme base) (liii error) (liii hash-table) (liii list) (liii path))
  (begin

    (define %run-registry (make-hash-table))
    (define %run-allow-list '())
    (define %run-ban-list '(rm))

    (define (%check-symbol-command sym)
      (when (memq sym %run-ban-list)
        (value-error (string-append "Symbol command '" (symbol->string sym) "' has been banned")
        ) ;value-error
      ) ;when
      (when (and (not (null? %run-allow-list)) (not (memq sym %run-allow-list)))
        (value-error (string-append "Symbol command '"
                       (symbol->string sym)
                       "' is not in the allow list"
                     ) ;string-append
        ) ;value-error
      ) ;when
    ) ;define

    (define (%check-string-command cmd)
      (let loop
        ((banned %run-ban-list))
        (when (pair? banned)
          (let ((sym-str (symbol->string (car banned))))
            (when
              (and (>= (string-length cmd) (string-length sym-str))
                (string=? (substring cmd 0 (string-length sym-str)) sym-str)
                (or (= (string-length cmd) (string-length sym-str))
                  (char=? (string-ref cmd (string-length sym-str)) #\space)
                ) ;or
              ) ;and
              (value-error (string-append "Command '" cmd "' has been banned"))
            ) ;when
          ) ;let
          (loop (cdr banned))
        ) ;when
      ) ;let
    ) ;define

    (define (%resolve-symbol-command sym args)
      (%check-symbol-command sym)
      (let ((val (hash-table-ref/default %run-registry sym #f)))
        (cond ((procedure? val) (cons 'lambda (cons val args)))
              ((string? val) (cons val args))
              ((path? val) (cons (path->string val) args))
              (else (cons (symbol->string sym) args))
        ) ;cond
      ) ;let
    ) ;define

    (define (run-set! symbol value)
      (hash-table-set! %run-registry symbol value)
    ) ;define

    (define (run-get symbol)
      (let ((val (hash-table-ref/default %run-registry symbol #f)))
        (cond ((not val) #f)
              ((string? val) (path val))
              (else val)
        ) ;cond
      ) ;let
    ) ;define

    (define (run-allow! symbol-or-list)
      (set! %run-allow-list
        (cond ((null? symbol-or-list) '())
              ((symbol? symbol-or-list) (list symbol-or-list))
              ((list? symbol-or-list) symbol-or-list)
              (else (type-error "(run-allow! symbol-or-list): must be symbol or list of symbols")
              ) ;else
        ) ;cond
      ) ;set!
    ) ;define

    (define (run-ban! symbol)
      (set! %run-ban-list (cons symbol %run-ban-list))
    ) ;define

    (define (run-unban! symbol)
      (set! %run-ban-list (filter (lambda (x) (not (eq? x symbol))) %run-ban-list))
    ) ;define

  ) ;begin
) ;define-library

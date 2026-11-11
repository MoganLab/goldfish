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

(define-library (liii packrat parser)
  (import (scheme base)
    (scheme write)
    (liii error)
    (liii match)
    (liii packrat combinator)
    (liii packrat result)
  ) ;import
  (export packrat-parser)
  (begin
    (define (object->external-representation o)
      (let ((s (open-output-string)))
        (write o s)
        (get-output-string s)
      ) ;let
    ) ;define

    (define (quote? x)
      (and (pair? x) (not (symbol? (car x))))
    ) ;define

    (define-macro (packrat-parser start-nt . nonterminal-defs)
      (letrec ((parse-nonterminal
                 (lambda (nt-def)
                   (let ((nt (car nt-def)))
                     `(define ,nt
                        (lambda (results)
                          (results->result results
                            (quote ,nt)
                            (lambda ,()
                              (,(parse-alternatives nt (cdr nt-def)) results)))))
                   ) ;let
                 ) ;lambda
               ) ;parse-nonterminal
               (parse-alternatives
                 (lambda (nt alts)
                   (if (null? (cdr alts))
                     (parse-alternative nt (car alts))
                     `(packrat-or ,(parse-alternative nt (car alts))
                        ,(parse-alternatives nt (cdr alts)))
                   ) ;if
                 ) ;lambda
               ) ;parse-alternatives
               (parse-alternative
                 (lambda (nt alt)
                   (let ((pattern (car alt)) (body (cadr alt)))
                     (parse-pattern nt body pattern)
                   ) ;let
                 ) ;lambda
               ) ;parse-alternative
               (parse-pattern
                 (lambda (nt body pattern)
                   (match pattern
                    ((('! fails ...) rest ...)
                     `(packrat-unless (string-append "Nonterminal "
                                        (symbol->string (quote ,nt))
                                        " expected to fail "
                                        ,(object->external-representation fails))
                        ,(parse-pattern nt #t fails)
                        ,(parse-pattern nt body rest))
                    ) ;
                    ((var '<- (? quote? val) rest ...)
                     `(packrat-check-base ,val
                        (lambda (,var) ,(parse-pattern nt body rest)))
                    ) ;
                    ((var '<- '^ rest ...)
                     `(lambda (results)
                        (let ((,var (parse-results-position results)))
                          (,(parse-pattern nt body rest) results)))
                    ) ;
                    ((var '<- val rest ...)
                     `(packrat-check ,val
                        (lambda (,var) ,(parse-pattern nt body rest)))
                    ) ;
                    (((? quote? val) rest ...)
                     `(packrat-check-base ,val
                        (lambda (dummy) ,(parse-pattern nt body rest)))
                    ) ;
                    ((val rest ...)
                     `(packrat-check ,val
                        (lambda (dummy) ,(parse-pattern nt body rest)))
                    ) ;
                    (() `(lambda (results) (make-result ,body results)))
                    (else (type-error "invalid pattern in packrat-lambda" pattern))
                   ) ;match
                 ) ;lambda
               ) ;parse-pattern
              ) ;
        `(let ,() ,@(map parse-nonterminal nonterminal-defs) ,start-nt)
      ) ;letrec
    ) ;define-macro
  ) ;begin
) ;define-library

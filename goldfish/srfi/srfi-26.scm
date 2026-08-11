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

(define-library (srfi srfi-26)
  (export cut cute)
  (import (liii list) (liii error))
  (begin

    (define-syntax cut
      (lambda (stx)
        (letrec* ((slot? (lambda (x) (equal? '<> x)))
                  (more-slot? (lambda (x) (equal? '<...> x)))
                  (paras (cdr (syntax->datum stx)))
                  (slots (filter slot? paras))
                  (more-slots (filter more-slot? paras))
                  (xs (map (lambda (x) (gensym)) slots))
                  (rest (gensym))
                  (parse (lambda (xs paras)
                           (cond ((null? paras) paras)
                                 ((not (list? paras)) paras)
                                 ((more-slot? (car paras)) `(,rest
                                                             ,@(parse xs
                                                                     (cdr paras))))
                                 ((slot? (car paras)) `(,(car xs)
                                                        ,@(parse (cdr xs)
                                                                 (cdr paras))))
                                 (else `(,(car paras) ,@(parse xs (cdr paras))))))))
          (datum->syntax
           stx
           (cond ((null? more-slots) `(lambda ,xs ,(parse xs paras)))
                 (else (when (or (> (length more-slots) 1) (not (more-slot? (last paras))))
                         (error 'syntax-error "<...> must be the last parameter of cut"))
                   (let ((parsed (parse xs paras)))
                     `(lambda (,@xs . ,rest) (apply ,@parsed)))))))))

    (define-syntax cute
      (lambda (stx)
        (letrec* ((slot? (lambda (x) (equal? '<> x)))
                  (more-slot? (lambda (x) (equal? '<...> x)))
                  (paras (cdr (syntax->datum stx)))
                  (exprs (filter (lambda (x) (not (or (slot? x) (more-slot? x)))) paras))
                  (xs (map (lambda (x) (gensym)) exprs))
                  (lets (map list xs exprs))
                  (parse (lambda (xs paras)
                           (cond ((null? paras) paras)
                                 ((not (list? paras)) paras)
                                 ((not (or (slot? (car paras)) (more-slot? (car paras))))
                                  `(,(car xs) ,@(parse (cdr xs) (cdr paras))))
                                 (else `(,(car paras) ,@(parse xs (cdr paras))))))))
          (datum->syntax stx `(let ,lets (cut ,@(parse xs paras)))))))

  ) ;begin
) ;define-library

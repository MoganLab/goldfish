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
  (import (goldfish))
  (export cut cute)
  (import (liii list) (liii error))
  (begin

    (define-syntax cut
      (lambda (stx)
        (let* ((paras (cdr (syntax->datum stx)))
               (slots (filter (lambda (x) (equal? '<> x)) paras))
               (more-slots (filter (lambda (x) (equal? '<...> x)) paras))
               (xs (map (lambda (x) (gensym)) slots))
               (rest (gensym))
               (parsed (let parse ((xs xs) (paras paras))
                         (cond ((null? paras) paras)
                               ((not (list? paras)) paras)
                               ((equal? '<...> (car paras))
                                (cons rest (parse xs (cdr paras))))
                               ((equal? '<> (car paras))
                                (cons (car xs) (parse (cdr xs) (cdr paras))))
                               (else (cons (car paras)
                                           (parse xs (cdr paras))))))))
          (datum->syntax
           stx
           (cond ((null? more-slots) `(lambda ,xs ,parsed))
                 (else (when (or (> (length more-slots) 1)
                                 (not (equal? '<...> (last paras))))
                         (error 'syntax-error "<...> must be the last parameter of cut"))
                   `(lambda (,@xs . ,rest) (apply ,@parsed))))))))

    (define-syntax cute
      (lambda (stx)
        (let* ((paras (cdr (syntax->datum stx)))
               (exprs (filter (lambda (x)
                                (not (or (equal? '<> x)
                                         (equal? '<...> x))))
                              paras))
               (xs (map (lambda (x) (gensym)) exprs))
               (lets (map list xs exprs))
               (parsed (let parse ((xs xs) (paras paras))
                         (cond ((null? paras) paras)
                               ((not (list? paras)) paras)
                               ((not (or (equal? '<> (car paras))
                                         (equal? '<...> (car paras))))
                                (cons (car xs) (parse (cdr xs) (cdr paras))))
                               (else (cons (car paras) (parse xs (cdr paras))))))))
          (datum->syntax stx `(let ,lets (cut ,@parsed))))))

  ) ;begin
) ;define-library

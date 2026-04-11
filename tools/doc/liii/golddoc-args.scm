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

(define-library (liii golddoc-args)
  (import (scheme base)
          (liii string)
  ) ;import
  (export parse-doc-args
          library-query?
          parse-library-query
  ) ;export
  (begin

    (define (library-query? value)
      (and (string? value)
           (string-contains? value "/")
      ) ;and
    ) ;define

    (define (classify-doc-args filtered)
      (cond
        ((null? filtered)
         '(invalid)
        ) ;
        ((or (member "--help" filtered)
             (member "-h" filtered))
         '(help)
        ) ;
        ((and (= (length filtered) 1)
              (string=? (car filtered) "--build-json"))
         '(build-json)
        ) ;
        ((and (= (length filtered) 1)
              (parse-library-query (car filtered)))
         (list 'library (car filtered))
        ) ;
        ((= (length filtered) 1)
         (list 'function (car filtered))
        ) ;
        ((and (= (length filtered) 2)
              (parse-library-query (car filtered)))
         (list 'library-function (car filtered) (cadr filtered))
        ) ;
        (else
         (cons 'invalid filtered)
        ) ;else
      ) ;cond
    ) ;define

    (define (parse-doc-args args)
      (let loop ((remaining (cdr args))
                 (skip-next #f)
                 (filtered '()))
        (if (null? remaining)
            (classify-doc-args (reverse filtered))
            (let ((arg (car remaining)))
              (cond
                (skip-next
                 (loop (cdr remaining) #f filtered)
                ) ;skip-next
                ((string=? arg "doc")
                 (loop (cdr remaining) #f filtered)
                ) ;
                ((or (string=? arg "-m")
                     (string=? arg "--mode")
                     (string=? arg "-I")
                     (string=? arg "-A"))
                 (loop (cdr remaining) #t filtered)
                ) ;
                ((or (string-starts? arg "-m=")
                     (string-starts? arg "--mode="))
                 (loop (cdr remaining) #f filtered)
                ) ;
                (else
                 (loop (cdr remaining) #f (cons arg filtered))
                ) ;else
              ) ;cond
            ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (parse-library-query query)
      (if (not (library-query? query))
          #f
          (let ((parts (string-split query "/")))
            (if (and (= (length parts) 2)
                     (not (string-null? (car parts)))
                     (not (string-null? (cadr parts))))
                (cons (car parts) (cadr parts))
                #f
            ) ;if
          ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library

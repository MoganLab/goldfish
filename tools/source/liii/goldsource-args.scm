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

(define-library (liii goldsource-args)
  (import (scheme base) (liii string) (liii argparse))
  (export parse-source-args library-query? parse-library-query)
  (begin

    (define (library-query? value)
      (and (string? value) (string-contains? value "/"))
    ) ;define

    (define (parse-library-query query)
      (if (not (library-query? query))
        #f
        (let ((parts (string-split query "/")))
          (if (and (= (length parts) 2)
                (not (string-null? (car parts)))
                (not (string-null? (cadr parts)))
              ) ;and
            (cons (car parts) (cadr parts))
            #f
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (classify-source-args filtered)
      (cond ((null? filtered) '(invalid))
            ((or (member "--help" filtered) (member "-h" filtered)) '(help))
            ((and (= (length filtered) 1) (parse-library-query (car filtered)))
             (list 'library (car filtered))
            ) ;
            (else (cons 'invalid filtered))
      ) ;cond
    ) ;define

    (define (parse-source-args args)
      (let ((parser (make-argument-parser '((command . "source")
                                            (skip-value-options "-m"
                                              "--mode"
                                              "-I"
                                              "-A")
                                            (skip-prefix-options "-m="
                                              "--mode=")
                                            (unknown-options . positional))
                    ) ;make-argument-parser
            ) ;parser
           ) ;
        (parser :parse-argv args)
        (classify-source-args (parser :positionals))
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

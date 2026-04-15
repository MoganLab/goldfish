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

(define-library (liii goldsource-library)
  (import (scheme base)
          (liii goldsource-args)
          (liii path)
  ) ;import
  (export find-visible-library-root
          source-library-path
  ) ;export
  (begin

    (define (find-visible-library-root query)
      (let ((parts (parse-library-query query)))
        (if (not parts)
            #f
            (let ((group (car parts))
                  (library (cdr parts)))
              (let loop ((roots *load-path*))
                (if (null? roots)
                    #f
                    (let ((load-root (car roots)))
                      (if (and (string? load-root)
                               (path-file? (path-join load-root
                                                      group
                                                      (string-append library ".scm")))
                          ) ;and
                          load-root
                          (loop (cdr roots))
                      ) ;if
                    ) ;let
                ) ;if
              ) ;let
            ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (source-library-path query)
      (let* ((parts (parse-library-query query))
             (group (and parts (car parts)))
             (library (and parts (cdr parts)))
             (load-root (and parts
                             (find-visible-library-root query))))
        (and load-root
             (path->string (path-join load-root group (string-append library ".scm")))
        ) ;and
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

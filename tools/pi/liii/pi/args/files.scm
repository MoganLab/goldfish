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

(define-library (liii pi args files)
  (import (scheme base) (liii string) (liii path))
  (export format-file-block process-file-arguments)
  (begin

    (define (format-file-block abs-path content)
      (string-append "<file name=\"" abs-path "\">\n" content "\n</file>\n")
    ) ;define

    (define (process-single-file raw-path)
      (let* ((p (path-resolve (path-expanduser (path raw-path))))
             (abs-str (path->string p))
            ) ;
        (if (not (path-exists? p))
          (error 'file-error
            (string-append "cannot read @" raw-path ": No such file or directory")
          ) ;error
          (let ((size (path-getsize p)))
            (if (= size 0) "" (format-file-block abs-str (path-read-text p)))
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (process-file-arguments file-args)
      (apply string-append (map process-single-file file-args))
    ) ;define

  ) ;begin
) ;define-library

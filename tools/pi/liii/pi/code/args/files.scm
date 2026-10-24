;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code args files)
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

;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi review files)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii path)
    (liii pi code tools read)
    (liii pi code files)
    (liii pi review language)
  ) ;import
  (export PI_REVIEW_MAX_FILES PI_REVIEW_MAX_FILE_CHARS pi-parse-changed-files
    pi-changed-files-command pi-collect-changed-files pi-review-file-records
  ) ;export
  (begin

    (define PI_REVIEW_MAX_FILES 10)
    (define PI_REVIEW_MAX_FILE_CHARS 4000)

    (define (%truncate-content content)
      (if (<= (string-length content) PI_REVIEW_MAX_FILE_CHARS)
        content
        (string-append (string-take content PI_REVIEW_MAX_FILE_CHARS) "\n[truncated]")
      ) ;if
    ) ;define

    (define (%read-file-content file-path)
      (catch #t
        (lambda ()
          (if (path-exists? (path file-path)) (path-read-text (path file-path)) "")
        ) ;lambda
        (lambda _ "")
      ) ;catch
    ) ;define

    (define (pi-review-file-records paths)
      (map
        (lambda (file-path)
          (list (cons 'path file-path)
            (cons 'language (pi-detect-language file-path))
            (cons 'content (%truncate-content (%read-file-content file-path)))
          ) ;list
        ) ;lambda
        paths
      ) ;map
    ) ;define

  ) ;begin
) ;define-library

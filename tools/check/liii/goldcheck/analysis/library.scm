;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
(define-library (liii goldcheck analysis library)
  (import (scheme base)
    (liii goldcheck type-hint parse)
    (liii goldcheck type-hint env)
  ) ;import
  (export resolve-library)
  (begin
    (define (resolve-library lib-name . maybe-roots)
      (let* ((roots (if (null? maybe-roots) (all-hint-roots) (car maybe-roots)))
             (file (library-file roots lib-name #t))
            ) ;
        (cond ((not file) (list 'indeterminate "library source file could not be resolved"))
              ((not (cached-library-info file))
               (list 'indeterminate "library source file could not be parsed")
              ) ;
              (else
                (let ((info (cached-library-info file)))
                  (list 'resolved file (lib-exports info))
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library

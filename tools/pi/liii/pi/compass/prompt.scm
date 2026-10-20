;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi compass prompt)
  (import (scheme base)
    (liii string)
    (liii pi args)
    (liii pi compass types)
    (liii pi compass generate)
    (liii pi compass render)
    (liii pi compass store)
  ) ;import
  (export PI_COMPASS_MAX_INJECTION_CHARS pi-compass-build-injection
    pi-compass-injection-text pi-compass-turn-system-prompt
  ) ;export
  (begin

    (define PI_COMPASS_MAX_INJECTION_CHARS 6000)

    (define (pi-compass-build-injection codemap max-chars)
      (let ((markdown (pi-compass-render-codemap codemap)))
        (string-append "\n\n" (pi-compass-truncate-codemap markdown max-chars))
      ) ;let
    ) ;define

    (define (%injection-codemap cwd)
      (let* ((file (pi-compass-default-codemap-path cwd))
             (entry (pi-compass-load-codemap file))
            ) ;
        (if (pi-compass-cache-hit? entry cwd)
          (let ((codemap (pi-compass-jref entry "data" #f)))
            (if (pi-compass-codemap? codemap) codemap #f)
          ) ;let
          #f
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-compass-injection-text parsed cwd)
      (if (pi-args-ref parsed 'no-codemap-inject)
        #f
        (let ((codemap (%injection-codemap cwd)))
          (if codemap
            (pi-compass-build-injection codemap PI_COMPASS_MAX_INJECTION_CHARS)
            #f
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (pi-compass-turn-system-prompt base parsed cwd)
      (let ((block (pi-compass-injection-text parsed cwd)))
        (if block (string-append base block) base)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

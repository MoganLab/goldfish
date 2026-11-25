;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;

(define-library (liii goldcheck free-variables)
  (import (scheme base)
    (liii goldcheck config)
    (liii goldcheck path)
    (liii goldcheck free-variables internal check)
  ) ;import
  (export check-free-variables %check-file-rule-free-variables)
  (begin
    (define (check-free-variables file . roots)
      (apply %check-free-variables file roots)
    ) ;define

    (define (%check-file-rule-free-variables file cfg acc)
      (if
        (and cfg
          (check-rule-enabled? cfg "free-variables")
          (not (file-excluded? file (check-rule-excludes cfg "free-variables")))
        ) ;and
        (append acc (check-free-variables file))
        acc
      ) ;if
    ) ;define
  ) ;begin
) ;define-library

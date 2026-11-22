;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
(define-library (liii goldcheck analysis)
  (import (liii goldcheck analysis library)
    (liii goldcheck analysis import)
    (liii goldcheck analysis reexport)
    (liii goldcheck analysis arity)
  ) ;import
  (export resolve-library reexport-owner normalize-import-mapping procedure-arity)
  (begin
  ) ;begin
) ;define-library

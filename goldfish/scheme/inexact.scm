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

(define-library (scheme inexact)
  (import (scheme base) (goldfish))
  (export acos asin atan cos exp finite? infinite? log nan? pi sin sqrt s7-sqrt tan)
  (begin

    ;; Retain the compatibility alias for existing liii callers.
    (define s7-sqrt sqrt)

    (define (finite? x)
      (and (number? x) (not (infinite? x)) (not (nan? x)))
    ) ;define

  ) ;begin
) ;define-library

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

(define-library (liii pi compass packages read)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii list)
    (liii path)
    (liii json)
    (liii pi compass types)
  ) ;import
  (export pi-compass-read-text pi-compass-read-json pi-compass-object-keys)
  (begin

    (define (pi-compass-read-text file)
      (if (not (path-file? (path file)))
        #f
        (catch #t (lambda () (path-read-text (path file))) (lambda _ #f))
      ) ;if
    ) ;define

    (define (pi-compass-read-json file)
      (let ((text (pi-compass-read-text file)))
        (if (not (string? text))
          #f
          (catch #t (lambda () (string->json text)) (lambda _ #f))
        ) ;if
      ) ;let
    ) ;define

    (define (pi-compass-object-keys obj)
      (if (pi-compass-object? obj) (map car obj) '())
    ) ;define

  ) ;begin
) ;define-library

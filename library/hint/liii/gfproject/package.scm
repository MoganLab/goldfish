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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; (hint liii gfproject package) - (liii gfproject package) 的类型接口

(define-library (hint liii gfproject package)
  (import (scheme base))
  (export hint-liii/gfproject/package)
  (begin

    (define hint-gfproject-package
      '((string? string? -> gfproject-package?)
        (string? string? string? -> gfproject-package?))
    ) ;define
    (define hint-gfproject-package? '((any? -> boolean?)))
    (define hint-gfproject-package-name '((gfproject-package? -> string?)))
    (define hint-gfproject-package-organization '((gfproject-package? -> string?)))
    (define hint-gfproject-package-description '((gfproject-package? -> string?)))
    (define hint-gfproject-package-id '((gfproject-package? -> string?)))
    (define hint-json->gfproject-package '((any? -> (or #f gfproject-package?))))

    (define (hint-liii/gfproject/package name)
      (case name
       ((gfproject-package) hint-gfproject-package)
       ((gfproject-package?) hint-gfproject-package?)
       ((gfproject-package-name) hint-gfproject-package-name)
       ((gfproject-package-organization) hint-gfproject-package-organization)
       ((gfproject-package-description) hint-gfproject-package-description)
       ((gfproject-package-id) hint-gfproject-package-id)
       ((json->gfproject-package) hint-json->gfproject-package)
       (else #f)
      ) ;case
    ) ;define
  ) ;begin
) ;define-library

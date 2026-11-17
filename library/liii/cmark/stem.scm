;;
;; Copyright (C) 2026 Liii Network
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

(define-library (liii cmark stem)
  (import (scheme base))
  (import (liii cmark stem util))
  (import (liii cmark stem blocks))
  (export markdown->md-stem)
  (begin
    (define (markdown->md-stem markdown)
      (cons 'md-stem (parse-markdown-blocks (string-split-lines markdown)))
    ) ;define
  ) ;begin
) ;define-library

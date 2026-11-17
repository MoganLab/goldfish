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

(define-library (liii cmark stem util)
  (import (scheme base))
  (import (liii cmark text))
  (import (liii cmark block line))
  (export blank-or-whitespace-line?)
  (export string-trim-left)
  (export string-trim-right)
  (export string-trim)
  (export string-split-lines)
  (export find-newline)
  (export strip-trailing-carriage-return)
  (export string-prefix?)
  (export string-join)
  (begin
  ) ;begin
) ;define-library

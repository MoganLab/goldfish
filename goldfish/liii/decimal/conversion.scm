;;; liii/decimal/conversion.scm - decimal 构造与格式化
;;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(define-library (liii decimal conversion)
  (import (scheme base) (liii decimal string) (liii decimal type))

  (export string->decimal decimal->string decimal->plain-string
    decimal->engineering-string decimal->scientific-string decimal->long
    decimal->long-exact decimal->int decimal->int-exact decimal->short
    decimal->short-exact decimal->byte decimal->byte-exact decimal->double
    decimal->float decimal->rational
  ) ;export

  (begin
  ) ;begin
) ;define-library

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

(define-library (liii pi web-search)
  (import (scheme base)
    (liii pi web-search schema)
    (liii pi web-search format)
    (liii pi web-search execute)
  ) ;import
  (export pi-web-search-schema pi-validate-web-search-args
    pi-format-web-search-result pi-execute-web-search
  ) ;export
  (begin
  ) ;begin
) ;define-library

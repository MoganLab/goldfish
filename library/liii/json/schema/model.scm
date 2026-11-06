;;
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
;;

(define-library (liii json schema model)
  (import (scheme base))
  (export make-validator json-schema-validator? validator-schema
    validator-options make-registry json-schema-registry? registry-resources
  ) ;export
  (begin
    (define-record-type <json-schema-validator>
      (make-validator schema options)
      json-schema-validator?
      (schema validator-schema)
      (options validator-options)
    ) ;define-record-type
    (define-record-type <json-schema-registry>
      (make-registry resources)
      json-schema-registry?
      (resources registry-resources)
    ) ;define-record-type
  ) ;begin
) ;define-library

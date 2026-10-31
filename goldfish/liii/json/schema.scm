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

;;; JSON Schema 2020-12 validation for Goldfish Scheme.
(define-library (liii json schema)
  (import (liii json schema core)
    (liii json schema error)
    (liii json schema registry)
  ) ;import
  (export json-schema-validate json-schema-valid? json-schema-errors
    json-schema-check-schema make-json-schema-validator json-schema-validator?
    json-schema-validator-validate json-schema-validator-valid?
    json-schema-validator-errors json-schema-error? json-schema-error-kind
    json-schema-error-message json-schema-error-keyword
    json-schema-error-keyword-value json-schema-error-instance
    json-schema-error-schema json-schema-error-path
    json-schema-error-schema-path json-schema-error-context
    json-schema-error-cause json-schema-error-absolute-path
    json-schema-error-absolute-schema-path json-schema-error-json-path
    make-json-schema-registry json-schema-registry? json-schema-registry-add
    json-schema-registry-add-many
  ) ;export
) ;define-library

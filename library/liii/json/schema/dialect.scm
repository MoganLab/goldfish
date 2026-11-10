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

(define-library (liii json schema dialect)
  (import (scheme base) (liii json) (liii json schema data))
  (export draft-for known-keyword? unsupported-modern-keyword!
    subschema-location? schema-map-keyword?
  ) ;export
  (begin
    (define (draft-for schema)
      (let ((declared (if (json-object? schema) (object-ref schema "$schema" absent) absent)
            ) ;declared
           ) ;
        (cond ((eq? declared absent) 'draft2020-12)
              ((not (string? declared))
               (error 'json-schema-schema-error "$schema must be a string" declared)
              ) ;
              ((member declared
                 '("https://json-schema.org/draft/2020-12/schema"
                   "https://json-schema.org/draft/2020-12/schema#")
               ) ;member
               'draft2020-12
              ) ;
              (else (error 'json-schema-unknown-dialect declared))
        ) ;cond
      ) ;let
    ) ;define
    (define (known-keyword? keyword)
      (member keyword
        '("type" "const" "enum" "properties" "required" "additionalProperties"
          "propertyNames" "minProperties" "maxProperties" "dependentRequired"
          "dependentSchemas" "unevaluatedProperties" "prefixItems" "items"
          "minItems" "maxItems" "uniqueItems" "contains" "unevaluatedItems"
          "minimum" "maximum" "exclusiveMinimum" "exclusiveMaximum" "multipleOf"
          "minLength" "maxLength" "pattern" "patternProperties" "allOf" "anyOf"
          "oneOf" "not" "if" "$ref" "$dynamicRef" "format")
      ) ;member
    ) ;define
    ;; A subschema location is a keyword whose value holds schemas. The
    ;; embedded $id, $anchor and $dynamicAnchor discovery only follows
    ;; these locations: $id inside enum, const, examples, default or an
    ;; unknown keyword never establishes a new base URI.
    (define (subschema-location? keyword)
      (member keyword
        '("properties" "patternProperties" "additionalProperties"
          "dependentSchemas" "unevaluatedProperties" "prefixItems" "items"
          "contains" "unevaluatedItems" "allOf" "anyOf" "oneOf" "not" "if"
          "then" "else" "$defs")
      ) ;member
    ) ;define
    ;; A map keyword's object maps names to schemas; the map's own keys
    ;; are property names, not schema keywords.
    (define (schema-map-keyword? keyword)
      (member keyword '("properties" "patternProperties" "dependentSchemas"
                        "$defs"))
    ) ;define
    (define (unsupported-modern-keyword! schema)
      #f
    ) ;define
  ) ;begin
) ;define-library

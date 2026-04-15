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

(define-library (liii goldfmt-rule)
  (export max-inline-length
          must-inline?
          never-inline?
          never-inline-when-first-child-env?
          allow-first-line-child-env?
          first-line-limit
          rest-indent)
  (import (liii base)
          (liii json)
          (liii os))

  (begin
    (define max-inline-length 40)
    (define node-rules-paths
      '("node-rules.json"
        "../node-rules.json"
        "../../node-rules.json"
        "../../../node-rules.json"))
    (define *node-rules* #f)

    (define (find-node-rules-path)
      (let loop ((paths node-rules-paths))
        (cond
          ((null? paths) "node-rules.json")
          ((access (car paths) 'R_OK) (car paths))
          (else (loop (cdr paths))))))

    (define (read-all-string port)
      (let loop ((result ""))
        (let ((chunk (read-string 4096 port)))
          (if (eof-object? chunk)
              result
              (loop (string-append result chunk))))))

    (define (read-file path)
      (call-with-input-file path read-all-string))

    (define (node-rules)
      (if *node-rules*
          *node-rules*
          (let ((rules (string->json (read-file (find-node-rules-path)))))
            (set! *node-rules* rules)
            rules)))

    (define (rule-for tag-name)
      (let* ((rules (node-rules))
             (rule (json-ref rules tag-name)))
        (if (null? rule)
            (json-ref rules "default")
            rule)))

    (define (rule-ref tag-name field default-value)
      (let* ((rules (node-rules))
             (rule (rule-for tag-name))
             (value (json-ref rule field)))
        (if (null? value)
            (let ((default-rule (json-ref rules "default")))
              (let ((default-field (json-ref default-rule field)))
                (if (null? default-field)
                    default-value
                    default-field)))
            value)))

    (define (json-bool value)
      (cond
        ((eq? value #t) #t)
        ((eq? value #f) #f)
        ((eq? value 'true) #t)
        ((eq? value 'false) #f)
        (else #f)))

    (define (rule-bool tag-name field default-value)
      (json-bool (rule-ref tag-name field default-value)))

    (define (must-inline? tag-name)
      (rule-bool tag-name "mustInline" #f))

    (define (never-inline? tag-name)
      (rule-bool tag-name "neverInline" #f))

    (define (never-inline-when-first-child-env? tag-name)
      (rule-bool tag-name "neverInlineWhenFirstChildEnv" #f))

    (define (allow-first-line-child-env? tag-name)
      (rule-bool tag-name "allowFirstLineChildEnv" #t))

    (define (first-line-limit tag-name)
      (rule-ref tag-name "firstLineLimit" 1))

    (define (rest-indent tag-name)
      (let ((value (rule-ref tag-name "restIndent" "byFirstRestChild")))
        (cond
          ((string=? value "alignToFirstSelectedEnv")
           'align-to-first-selected-env)
          ((string=? value "parentPlus2")
           'parent-plus2)
          (else 'by-first-rest-child))))
    ))

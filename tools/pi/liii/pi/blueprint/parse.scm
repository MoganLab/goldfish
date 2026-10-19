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

(define-library (liii pi blueprint parse)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii json)
    (liii pi blueprint types)
  ) ;import
  (export pi-extract-blueprint-json pi-parse-blueprint-plan)
  (begin

    (define (%plan-error msg)
      (error 'blueprint-plan-error msg)
    ) ;define

    (define (%strip-fence text)
      (let ((trimmed (string-trim-both text)))
        (if (string-starts? trimmed "```")
          (let* ((nl (string-index trimmed #\newline))
                 (body
                   (if nl
                     (string-trim-both (substring trimmed (+ nl 1) (string-length trimmed)))
                     ""
                   ) ;if
                 ) ;body
                ) ;
            (if (string-ends? body "```")
              (string-trim-both (substring body 0 (- (string-length body) 3)))
              body
            ) ;if
          ) ;let*
          trimmed
        ) ;if
      ) ;let
    ) ;define

    (define (pi-extract-blueprint-json text)
      (let* ((body (%strip-fence text))
             (start (string-index body #\{))
             (end (string-index-right body #\}))
            ) ;
        (if (and start end (< start end)) (substring body start (+ end 1)) #f)
      ) ;let*
    ) ;define

    (define (%string-list value)
      (filter string? (pi-blueprint-vector->list value))
    ) ;define

    (define (%parse-task raw)
      (pi-blueprint-task
        (let ((v (pi-blueprint-jref raw "id" #f)))
          (if (string? v) v "")
        ) ;let
        (let ((v (pi-blueprint-jref raw "title" #f)))
          (if (string? v) v "")
        ) ;let
        (let ((v (pi-blueprint-jref raw "description" #f)))
          (if (string? v) v "")
        ) ;let
        (%string-list (pi-blueprint-jref raw "acceptance_criteria" #()))
        (%string-list (pi-blueprint-jref raw "file_targets" #()))
        (%string-list (pi-blueprint-jref raw "dependencies" #()))
      ) ;pi-blueprint-task
    ) ;define

    (define (%parse-gate raw)
      (let ((type (pi-blueprint-jref raw "type" #f))
            (description (pi-blueprint-jref raw "description" #f))
            (command (pi-blueprint-jref raw "command" 'null))
           ) ;
        (pi-blueprint-gate (if (string? type) type "")
          (if (string? description) description "")
          (if (string? command) command #f)
        ) ;pi-blueprint-gate
      ) ;let
    ) ;define

    (define (%parse-phase raw)
      (pi-blueprint-phase
        (let ((v (pi-blueprint-jref raw "id" #f)))
          (if (string? v) v "")
        ) ;let
        (let ((v (pi-blueprint-jref raw "title" #f)))
          (if (string? v) v "")
        ) ;let
        (let ((v (pi-blueprint-jref raw "description" #f)))
          (if (string? v) v "")
        ) ;let
        (map %parse-task
          (pi-blueprint-vector->list (pi-blueprint-jref raw "tasks" #()))
        ) ;map
        (map %parse-gate
          (pi-blueprint-vector->list (pi-blueprint-jref raw "verification_gates" #()))
        ) ;map
      ) ;pi-blueprint-phase
    ) ;define

    ;; 从模型输出中解析结构化计划，返回 {objective, phases}。
    ;; 无法解析时抛 blueprint-plan-error（含原因摘要）。
    (define (pi-parse-blueprint-plan text objective)
      (let ((json-text (pi-extract-blueprint-json text)))
        (if (not json-text)
          (%plan-error "No JSON plan found in model output. Expected an object with phases."
          ) ;%plan-error
          (let ((parsed (catch #t (lambda () (string->json json-text)) (lambda _ #f))))
            (if (not (pair? parsed))
              (%plan-error "Invalid JSON plan: could not parse model output as a JSON object."
              ) ;%plan-error
              (let* ((phase-pair (assoc "phases" parsed))
                     (raw-phases (if phase-pair (cdr phase-pair) #f))
                    ) ;
                (if (not (or (vector? raw-phases) (list? raw-phases)))
                  (%plan-error "Invalid JSON plan: missing \"phases\" array.")
                  (list (cons "objective" objective)
                    (cons "phases"
                      (list->vector (map %parse-phase (pi-blueprint-vector->list raw-phases)))
                    ) ;cons
                  ) ;list
                ) ;if
              ) ;let*
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

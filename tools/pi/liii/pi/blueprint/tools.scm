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

(define-library (liii pi blueprint tools)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi tools record)
    (liii pi blueprint types)
    (liii pi blueprint graph)
    (liii pi blueprint state)
    (liii pi blueprint transition)
    (liii pi blueprint render)
    (liii pi blueprint runtime)
  ) ;import
  (export pi-blueprint-update-schema pi-blueprint-status-schema
    pi-blueprint-next-schema pi-execute-blueprint-update
    pi-execute-blueprint-status pi-execute-blueprint-next
    pi-blueprint-update-definition pi-blueprint-status-definition
    pi-blueprint-next-definition
  ) ;export
  (begin

    (define %guidelines
      '("Use blueprint_status to check current plan progress."
        "Use blueprint_update to mark tasks as completed, in_progress, or skipped."
        "Use blueprint_next to get the next actionable task.")
    ) ;define

    (define (%arg args key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (pair (assoc str-key args))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    (define (%object-schema properties)
      (list (cons "type" "object")
        (cons "properties" (if (null? properties) '(()) properties))
      ) ;list
    ) ;define

    (define (%string-property description)
      (list (cons "type" "string") (cons "description" description))
    ) ;define

    (define (pi-blueprint-status-schema)
      (%object-schema '())
    ) ;define

    (define (pi-blueprint-next-schema)
      (%object-schema '())
    ) ;define

    (define (pi-blueprint-update-schema)
      (%object-schema
        (list (cons "task_id" (%string-property "Task ID to update (e.g. '1.1')"))
          (cons "status"
            (list (cons "type" "string")
              (cons "enum" #("in_progress" "completed" "skipped"))
              (cons "description" "New task status")
            ) ;list
          ) ;cons
          (cons "notes" (%string-property "Optional notes about the change"))
        ) ;list
      ) ;%object-schema
    ) ;define

    (define (%next-info bp)
      (let ((next (pi-blueprint-get-next-task bp)))
        (if next
          (string-append "\nNext task: " (pi-task-id next) " - " (pi-task-title next))
          "\nNo more tasks."
        ) ;if
      ) ;let
    ) ;define

    (define (pi-execute-blueprint-status args . opts)
      (let ((bp (pi-blueprint-current)))
        (if (not bp)
          (pi-tool-result "No active blueprint." #f)
          (pi-tool-result (string-append (pi-format-blueprint-progress bp)
                            "\n\n"
                            (pi-render-blueprint-plan bp)
                          ) ;string-append
            #f
          ) ;pi-tool-result
        ) ;if
      ) ;let
    ) ;define

    (define (pi-execute-blueprint-next args . opts)
      (let ((bp (pi-blueprint-current)))
        (if (not bp)
          (pi-tool-result "No active blueprint." #f)
          (let ((next (pi-blueprint-get-next-task bp)))
            (cond
             ((not next)
              (pi-tool-result (if (string=? (pi-blueprint-status bp) "completed")
                                "Blueprint is complete. All tasks and verifications are done."
                                "No actionable tasks. Some may be blocked or awaiting verification."
                              ) ;if
                #f
              ) ;pi-tool-result
             ) ;
             (else (pi-tool-result (pi-render-next-task next) #f))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%event-for status)
      (cond ((string=? status "completed") "task_completed")
            ((string=? status "skipped") "task_skipped")
            (else "task_started")
      ) ;cond
    ) ;define

    (define (%apply-status bp task-id status)
      (cond ((string=? status "completed") (pi-blueprint-complete-task bp task-id))
            ((string=? status "skipped") (pi-blueprint-skip-task bp task-id))
            ((string=? status "in_progress") (pi-blueprint-start-task bp task-id #f))
            (else bp)
      ) ;cond
    ) ;define

    (define (pi-execute-blueprint-update args . opts)
      (let* ((bp (pi-blueprint-current))
             (task-id (%arg args "task_id"))
             (status (%arg args "status"))
             (notes (%arg args "notes"))
             (tasks (and bp (pi-blueprint-all-tasks bp)))
             (task (and tasks task-id (pi-blueprint-find-task tasks task-id)))
            ) ;
        (cond ((not bp) (pi-tool-result "No active blueprint." #t))
              ((not (string? task-id))
               (pi-tool-result "Missing required argument: task_id" #t)
              ) ;
              ((not (member status '("in_progress" "completed" "skipped")))
               (pi-tool-result "Invalid status. Allowed: in_progress, completed, skipped" #t)
              ) ;
              ((not task) (pi-tool-result (string-append "Task " task-id " not found.") #t))
              (else
                (let* ((bp1 (%apply-status bp task-id status))
                       (bp2
                         (if (and (string? notes) (not (string-null? notes)))
                           (pi-blueprint-set-notes bp1 task-id notes)
                           bp1
                         ) ;if
                       ) ;bp2
                       (details
                         (if (and (string? notes) (not (string-null? notes)))
                           notes
                           (string-append "Task " task-id " -> " status)
                         ) ;if
                       ) ;details
                      ) ;
                  (pi-blueprint-updated-with-history! bp2 (%event-for status) task-id details)
                  (pi-tool-result (string-append "Task " task-id " updated to " status "." (%next-info bp2))
                    #f
                  ) ;pi-tool-result
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-blueprint-update-definition)
      (pi-tool-definition "blueprint_update"
        "Update a blueprint task status (mark as completed, in_progress, or skipped)"
        (pi-blueprint-update-schema)
        pi-execute-blueprint-update
        "Mark a blueprint task as completed or update its status"
        %guidelines
      ) ;pi-tool-definition
    ) ;define

    (define (pi-blueprint-status-definition)
      (pi-tool-definition "blueprint_status"
        "Get the current blueprint progress, active phase, and task status"
        (pi-blueprint-status-schema)
        pi-execute-blueprint-status
        "Check current blueprint plan progress"
        %guidelines
      ) ;pi-tool-definition
    ) ;define

    (define (pi-blueprint-next-definition)
      (pi-tool-definition "blueprint_next"
        "Get the next actionable task from the active blueprint"
        (pi-blueprint-next-schema)
        pi-execute-blueprint-next
        "Get the next task to work on"
        %guidelines
      ) ;pi-tool-definition
    ) ;define

  ) ;begin
) ;define-library

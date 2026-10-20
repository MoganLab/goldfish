;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi blueprint validate)
  (import (scheme base)
    (liii string)
    (liii list)
    (srfi srfi-1)
    (liii pi blueprint types)
    (liii pi blueprint graph)
  ) ;import
  (export pi-validate-blueprint-plan pi-blueprint-valid-gate-type?)
  (begin

    (define (%plan-error msg)
      (error 'blueprint-plan-error msg)
    ) ;define

    (define (pi-blueprint-valid-gate-type? type)
      (or (string=? type "custom_command") (string=? type "user_approval"))
    ) ;define

    (define (%phase-task-ids phase)
      (map pi-task-id (pi-phase-tasks phase))
    ) ;define

    (define (%id-format-ok? id)
      (let ((parts (string-split id ".")))
        (and (= (length parts) 2)
          (not (string-null? (car parts)))
          (not (string-null? (cadr parts)))
          (number? (string->number (car parts)))
          (number? (string->number (cadr parts)))
        ) ;and
      ) ;let
    ) ;define

    (define (%cycles-text cycles)
      (string-append "["
        (string-join
          (map
            (lambda (cycle)
              (string-append "["
                (string-join (map (lambda (id) (string-append "\"" id "\"")) cycle) ",")
                "]"
              ) ;string-append
            ) ;lambda
            cycles
          ) ;map
          ","
        ) ;string-join
        "]"
      ) ;string-append
    ) ;define

    (define (%validate-phase-count phases)
      (let ((n (length phases)))
        (when (or (< n 2) (> n 6))
          (%plan-error (string-append "Invalid plan: expected 2-6 phases, got " (number->string n) ".")
          ) ;%plan-error
        ) ;when
      ) ;let
    ) ;define

    (define (%validate-task-count phase)
      (let ((n (length (pi-phase-tasks phase))))
        (when (or (< n 2) (> n 8))
          (%plan-error (string-append "Invalid plan: phase \""
                         (pi-phase-id phase)
                         "\" must contain 2-8 tasks, got "
                         (number->string n)
                         "."
                       ) ;string-append
          ) ;%plan-error
        ) ;when
      ) ;let
    ) ;define

    (define (%validate-phases phases)
      (let ((phase-ids (map pi-phase-id phases)))
        (when
          (not (= (length phase-ids) (length (delete-duplicates phase-ids))))
          (%plan-error "Invalid plan: duplicate phase IDs.")
        ) ;when
      ) ;let
      (for-each
        (lambda (phase)
          (when (or (string-null? (pi-phase-id phase)) (string-null? (pi-phase-title phase)))
            (%plan-error "Invalid plan: every phase needs a non-empty id and title.")
          ) ;when
          (%validate-task-count phase)
        ) ;lambda
        phases
      ) ;for-each
    ) ;define

    (define (%validate-tasks tasks)
      (let ((ids (map pi-task-id tasks)))
        (when
          (not (= (length ids) (length (delete-duplicates ids))))
          (%plan-error "Invalid plan: duplicate task IDs.")
        ) ;when
        (for-each
          (lambda (task)
            (when (not (%id-format-ok? (pi-task-id task)))
              (%plan-error (string-append "Invalid plan: task ID \""
                             (pi-task-id task)
                             "\" must look like \"phase.task\" (e.g. \"1.1\")."
                           ) ;string-append
              ) ;%plan-error
            ) ;when
            (when (string-null? (pi-task-title task))
              (%plan-error (string-append "Invalid plan: task " (pi-task-id task) " has an empty title.")
              ) ;%plan-error
            ) ;when
            (for-each
              (lambda (dep)
                (when (not (member dep ids))
                  (%plan-error (string-append "Invalid plan: task "
                                 (pi-task-id task)
                                 " depends on unknown task "
                                 dep
                                 "."
                               ) ;string-append
                  ) ;%plan-error
                ) ;when
              ) ;lambda
              (pi-task-dependencies task)
            ) ;for-each
          ) ;lambda
          tasks
        ) ;for-each
        (let ((cycles (pi-blueprint-detect-cycles tasks)))
          (when (pair? cycles)
            (%plan-error (string-append "Dependency cycles detected: " (%cycles-text cycles))
            ) ;%plan-error
          ) ;when
        ) ;let
      ) ;let
    ) ;define

    (define (%validate-gate phase gate)
      (let ((type (pi-gate-type gate)))
        (cond ((string=? type "tests_pass")
               (%plan-error "Invalid plan: gate type \"tests_pass\" is not supported in script mode. Use \"custom_command\" instead."
               ) ;%plan-error
              ) ;
              ((string=? type "typecheck_clean")
               (%plan-error "Invalid plan: gate type \"typecheck_clean\" is not supported in script mode. Use \"custom_command\" instead."
               ) ;%plan-error
              ) ;
              ((not (pi-blueprint-valid-gate-type? type))
               (%plan-error (string-append "Invalid plan: unsupported gate type \""
                              type
                              "\". Allowed: custom_command, user_approval."
                            ) ;string-append
               ) ;%plan-error
              ) ;
        ) ;cond
        (when
          (and (string=? type "custom_command")
            (not
              (and (string? (pi-gate-command gate))
                (not (string-null? (string-trim-both (pi-gate-command gate))))
              ) ;and
            ) ;not
          ) ;and
          (%plan-error (string-append "Invalid plan: phase \""
                         (pi-phase-id phase)
                         "\" has a custom_command gate without a command."
                       ) ;string-append
          ) ;%plan-error
        ) ;when
      ) ;let
    ) ;define

    (define (%validate-gates phase)
      (for-each (lambda (gate) (%validate-gate phase gate)) (pi-phase-gates phase))
    ) ;define

    (define (pi-validate-blueprint-plan plan)
      (let* ((phases (pi-blueprint-vector->list (pi-blueprint-jref plan "phases" #())))
             (tasks (pi-blueprint-flat-tasks phases))
            ) ;
        (%validate-phase-count phases)
        (%validate-phases phases)
        (%validate-tasks tasks)
        (for-each %validate-gates phases)
        #t
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

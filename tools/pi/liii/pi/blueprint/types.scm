;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi blueprint types)
  (import (scheme base) (liii string) (liii list) (liii json) (liii pi time))
  (export pi-blueprint-null? pi-blueprint-json-normalize pi-blueprint-jref
    pi-blueprint-assoc-set pi-blueprint-vector->list pi-blueprint-list->vector
    pi-blueprint-task pi-blueprint-phase pi-blueprint-gate
    pi-blueprint-blueprint pi-blueprint-new-id pi-task-id pi-task-title
    pi-task-description pi-task-status pi-task-acceptance pi-task-file-targets
    pi-task-dependencies pi-task-notes pi-task-done? pi-phase-id pi-phase-title
    pi-phase-description pi-phase-status pi-phase-tasks pi-phase-gates
    pi-gate-type pi-gate-description pi-gate-command pi-gate-passed?
    pi-gate-last-checked pi-gate-error-message pi-blueprint-id
    pi-blueprint-objective pi-blueprint-status pi-blueprint-phases
    pi-blueprint-active-phase-id pi-blueprint-active-task-id
    pi-blueprint-all-tasks pi-blueprint-completed-task-ids
  ) ;export
  (begin

    (define (pi-blueprint-null? x)
      (eq? x 'null)
    ) ;define

    ;; JSON 解析器把 true/false 读成符号；加载状态后统一还原为布尔值。
    (define (pi-blueprint-json-normalize x)
      (cond ((eq? x 'true) #t)
            ((eq? x 'false) #f)
            ((vector? x) (list->vector (map pi-blueprint-json-normalize (vector->list x))))
            ((pair? x)
             (map
               (lambda (pair) (cons (car pair) (pi-blueprint-json-normalize (cdr pair))))
               x
             ) ;map
            ) ;
            (else x)
      ) ;cond
    ) ;define

    ;; 状态对象统一采用 JSON 原生形状：关联表（字符串键）+ 向量（数组），
    ;; 因此可以直接用 (liii json) 读写，无需二次转换。
    (define (%key->string key)
      (if (symbol? key) (symbol->string key) key)
    ) ;define

    (define (pi-blueprint-jref obj key default)
      (if (not (pair? obj))
        default
        (let ((pair (assoc (%key->string key) obj)))
          (if pair (cdr pair) default)
        ) ;let
      ) ;if
    ) ;define

    (define (pi-blueprint-assoc-set obj key val)
      (let* ((k (%key->string key)) (pair (assoc k obj)))
        (if pair
          (map (lambda (p) (if (eq? p pair) (cons k val) p)) obj)
          (append obj (list (cons k val)))
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-blueprint-vector->list x)
      (cond ((vector? x) (vector->list x))
            ((list? x) x)
            ((pi-blueprint-null? x) '())
            (else '())
      ) ;cond
    ) ;define

    (define (pi-blueprint-list->vector x)
      (if (vector? x) x (list->vector x))
    ) ;define

    (define (pi-blueprint-task id title description acceptance file-targets deps)
      (list (cons "id" id)
        (cons "title" title)
        (cons "description" description)
        (cons "status" "pending")
        (cons "acceptance_criteria" (pi-blueprint-list->vector acceptance))
        (cons "file_targets" (pi-blueprint-list->vector file-targets))
        (cons "dependencies" (pi-blueprint-list->vector deps))
        (cons "started_at" 'null)
        (cons "completed_at" 'null)
        (cons "session_id" 'null)
        (cons "notes" 'null)
      ) ;list
    ) ;define

    (define (pi-blueprint-gate type description command)
      (list (cons "type" type)
        (cons "command" (if command command 'null))
        (cons "description" description)
        (cons "passed" #f)
        (cons "last_checked_at" 'null)
        (cons "error_message" 'null)
      ) ;list
    ) ;define

    (define (pi-blueprint-phase id title description tasks gates)
      (list (cons "id" id)
        (cons "title" title)
        (cons "description" description)
        (cons "status" "pending")
        (cons "tasks" (pi-blueprint-list->vector tasks))
        (cons "verification_gates" (pi-blueprint-list->vector gates))
        (cons "started_at" 'null)
        (cons "completed_at" 'null)
      ) ;list
    ) ;define

    (define (pi-blueprint-new-id)
      (string-append "bp-" (number->string (pi-current-time-millis)))
    ) ;define

    (define (pi-blueprint-blueprint id objective project-id phases)
      (let ((now (pi-iso-8601-utc-now)))
        (list (cons "id" id)
          (cons "objective" objective)
          (cons "project_id" project-id)
          (cons "status" "active")
          (cons "created_at" now)
          (cons "updated_at" now)
          (cons "phases" (pi-blueprint-list->vector phases))
          (cons "active_phase_id" (if (pair? phases) (pi-phase-id (car phases)) 'null))
          (cons "active_task_id" 'null)
        ) ;list
      ) ;let
    ) ;define

    ;; task 访问器
    (define (pi-task-id t)
      (pi-blueprint-jref t "id" "")
    ) ;define
    (define (pi-task-title t)
      (pi-blueprint-jref t "title" "")
    ) ;define
    (define (pi-task-description t)
      (pi-blueprint-jref t "description" "")
    ) ;define
    (define (pi-task-status t)
      (pi-blueprint-jref t "status" "pending")
    ) ;define
    (define (pi-task-acceptance t)
      (pi-blueprint-vector->list (pi-blueprint-jref t "acceptance_criteria" #()))
    ) ;define
    (define (pi-task-file-targets t)
      (pi-blueprint-vector->list (pi-blueprint-jref t "file_targets" #()))
    ) ;define
    (define (pi-task-dependencies t)
      (pi-blueprint-vector->list (pi-blueprint-jref t "dependencies" #()))
    ) ;define
    (define (pi-task-notes t)
      (let ((v (pi-blueprint-jref t "notes" 'null)))
        (if (pi-blueprint-null? v) #f v)
      ) ;let
    ) ;define

    (define (pi-task-done? t)
      (let ((s (pi-task-status t)))
        (or (string=? s "completed") (string=? s "skipped"))
      ) ;let
    ) ;define

    ;; phase 访问器
    (define (pi-phase-id p)
      (pi-blueprint-jref p "id" "")
    ) ;define
    (define (pi-phase-title p)
      (pi-blueprint-jref p "title" "")
    ) ;define
    (define (pi-phase-description p)
      (pi-blueprint-jref p "description" "")
    ) ;define
    (define (pi-phase-status p)
      (pi-blueprint-jref p "status" "pending")
    ) ;define
    (define (pi-phase-tasks p)
      (pi-blueprint-vector->list (pi-blueprint-jref p "tasks" #()))
    ) ;define
    (define (pi-phase-gates p)
      (pi-blueprint-vector->list (pi-blueprint-jref p "verification_gates" #()))
    ) ;define

    ;; gate 访问器
    (define (pi-gate-type g)
      (pi-blueprint-jref g "type" "")
    ) ;define
    (define (pi-gate-description g)
      (pi-blueprint-jref g "description" "")
    ) ;define
    (define (pi-gate-command g)
      (let ((v (pi-blueprint-jref g "command" 'null)))
        (if (pi-blueprint-null? v) #f v)
      ) ;let
    ) ;define
    (define (pi-gate-passed? g)
      (pi-blueprint-jref g "passed" #f)
    ) ;define
    (define (pi-gate-last-checked g)
      (pi-blueprint-jref g "last_checked_at" 'null)
    ) ;define
    (define (pi-gate-error-message g)
      (let ((v (pi-blueprint-jref g "error_message" 'null)))
        (if (pi-blueprint-null? v) #f v)
      ) ;let
    ) ;define

    ;; blueprint 访问器
    (define (pi-blueprint-id bp)
      (pi-blueprint-jref bp "id" "")
    ) ;define
    (define (pi-blueprint-objective bp)
      (pi-blueprint-jref bp "objective" "")
    ) ;define
    (define (pi-blueprint-status bp)
      (pi-blueprint-jref bp "status" "active")
    ) ;define
    (define (pi-blueprint-phases bp)
      (pi-blueprint-vector->list (pi-blueprint-jref bp "phases" #()))
    ) ;define
    (define (pi-blueprint-active-phase-id bp)
      (let ((v (pi-blueprint-jref bp "active_phase_id" 'null)))
        (if (pi-blueprint-null? v) #f v)
      ) ;let
    ) ;define
    (define (pi-blueprint-active-task-id bp)
      (let ((v (pi-blueprint-jref bp "active_task_id" 'null)))
        (if (pi-blueprint-null? v) #f v)
      ) ;let
    ) ;define

    (define (pi-blueprint-all-tasks bp)
      (apply append (map pi-phase-tasks (pi-blueprint-phases bp)))
    ) ;define

    (define (pi-blueprint-completed-task-ids tasks)
      (map pi-task-id (filter pi-task-done? tasks))
    ) ;define

  ) ;begin
) ;define-library

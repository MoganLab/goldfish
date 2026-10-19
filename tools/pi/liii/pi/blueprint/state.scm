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

(define-library (liii pi blueprint state)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi time)
    (liii pi blueprint types)
    (liii pi blueprint graph)
  ) ;import
  (export pi-blueprint-set-field pi-blueprint-map-phase-tasks
    pi-blueprint-update-phase pi-blueprint-find-phase pi-blueprint-next-task-id
    pi-blueprint-recompute-blocked pi-blueprint-get-next-task
    pi-blueprint-start-task pi-blueprint-patch-task pi-blueprint-set-notes
  ) ;export
  (begin

    (define (pi-blueprint-set-field obj key val)
      (pi-blueprint-assoc-set obj key val)
    ) ;define

    (define (pi-blueprint-map-phase-tasks phase f)
      (pi-blueprint-assoc-set phase
        "tasks"
        (list->vector (map f (pi-phase-tasks phase)))
      ) ;pi-blueprint-assoc-set
    ) ;define

    (define (pi-blueprint-update-phase bp phase-id f)
      (let ((phases
              (map
                (lambda (p) (if (string=? (pi-phase-id p) phase-id) (f p) p))
                (pi-blueprint-phases bp)
              ) ;map
            ) ;phases
           ) ;
        (pi-blueprint-assoc-set (pi-blueprint-assoc-set bp "phases" (list->vector phases))
          "updated_at"
          (pi-iso-8601-utc-now)
        ) ;pi-blueprint-assoc-set
      ) ;let
    ) ;define

    (define (pi-blueprint-find-phase bp phase-id)
      (let ((hit
              (filter (lambda (p) (string=? (pi-phase-id p) phase-id))
                (pi-blueprint-phases bp)
              ) ;filter
            ) ;hit
           ) ;
        (if (pair? hit) (car hit) #f)
      ) ;let
    ) ;define

    (define (pi-blueprint-next-task-id bp)
      (let ((next (pi-blueprint-get-next-task bp)))
        (if next (pi-task-id next) 'null)
      ) ;let
    ) ;define

    ;; 依依赖完成情况重算 blocked / pending，已结束任务不动。
    (define (pi-blueprint-recompute-blocked bp)
      (let* ((tasks (pi-blueprint-all-tasks bp))
             (blocked (pi-blueprint-blocked-id-list tasks))
             (phases
               (map
                 (lambda (phase)
                   (pi-blueprint-map-phase-tasks phase
                     (lambda (task)
                       (cond ((pi-task-done? task) task)
                             ((member (pi-task-id task) blocked)
                              (if (string=? (pi-task-status task) "blocked")
                                task
                                (pi-blueprint-set-field task "status" "blocked")
                              ) ;if
                             ) ;
                             ((string=? (pi-task-status task) "blocked")
                              (pi-blueprint-set-field task "status" "pending")
                             ) ;
                             (else task)
                       ) ;cond
                     ) ;lambda
                   ) ;pi-blueprint-map-phase-tasks
                 ) ;lambda
                 (pi-blueprint-phases bp)
               ) ;map
             ) ;phases
            ) ;
        (pi-blueprint-assoc-set (pi-blueprint-assoc-set bp "phases" (list->vector phases))
          "updated_at"
          (pi-iso-8601-utc-now)
        ) ;pi-blueprint-assoc-set
      ) ;let*
    ) ;define

    (define (%next-in-phase phase tasks)
      (let ((in-progress
              (filter (lambda (t) (string=? (pi-task-status t) "in_progress"))
                (pi-phase-tasks phase)
              ) ;filter
            ) ;in-progress
            (pending
              (filter
                (lambda (t)
                  (and (string=? (pi-task-status t) "pending")
                    (pi-blueprint-ready? tasks (pi-task-id t))
                  ) ;and
                ) ;lambda
                (pi-phase-tasks phase)
              ) ;filter
            ) ;pending
           ) ;
        (cond ((pair? in-progress) (car in-progress))
              ((pair? pending) (car pending))
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-blueprint-get-next-task bp)
      (let* ((tasks (pi-blueprint-all-tasks bp))
             (active-id (pi-blueprint-active-phase-id bp))
             (active-phase (and active-id (pi-blueprint-find-phase bp active-id)))
             (found (and active-phase (%next-in-phase active-phase tasks)))
            ) ;
        (if found
          found
          (let loop
            ((phases (pi-blueprint-phases bp)))
            (if (null? phases)
              #f
              (if (string=? (pi-phase-status (car phases)) "verified")
                (loop (cdr phases))
                (let ((hit (%next-in-phase (car phases) tasks)))
                  (if hit hit (loop (cdr phases)))
                ) ;let
              ) ;if
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (%start-phase phase now)
      (let ((status (pi-phase-status phase)))
        (cond ((string=? status "pending")
               (pi-blueprint-set-field (pi-blueprint-set-field phase "status" "active")
                 "started_at"
                 now
               ) ;pi-blueprint-set-field
              ) ;
              (else phase)
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-blueprint-start-task bp task-id session-id)
      (let* ((tasks (pi-blueprint-all-tasks bp))
             (task (pi-blueprint-find-task tasks task-id))
             (phase
               (and task
                 (let loop
                   ((rest (pi-blueprint-phases bp)))
                   (cond ((null? rest) #f)
                         ((pi-blueprint-find-task (pi-phase-tasks (car rest)) task-id) (car rest))
                         (else (loop (cdr rest)))
                   ) ;cond
                 ) ;let
               ) ;and
             ) ;phase
            ) ;
        (if (not task)
          bp
          (let* ((now (pi-iso-8601-utc-now))
                 (started (pi-blueprint-jref task "started_at" 'null))
                 (updated
                   (pi-blueprint-set-field (pi-blueprint-set-field (pi-blueprint-set-field task "status" "in_progress")
                                             "started_at"
                                             (if (pi-blueprint-null? started) now started)
                                           ) ;pi-blueprint-set-field
                     "session_id"
                     (if (and (string? session-id) (not (string-null? session-id))) session-id 'null)
                   ) ;pi-blueprint-set-field
                 ) ;updated
                 (phase-id (pi-phase-id phase))
                 (bp1
                   (pi-blueprint-update-phase bp
                     phase-id
                     (lambda (p)
                       (pi-blueprint-map-phase-tasks (%start-phase p now)
                         (lambda (t) (if (string=? (pi-task-id t) task-id) updated t))
                       ) ;pi-blueprint-map-phase-tasks
                     ) ;lambda
                   ) ;pi-blueprint-update-phase
                 ) ;bp1
                ) ;
            (pi-blueprint-assoc-set (pi-blueprint-assoc-set (pi-blueprint-assoc-set (pi-blueprint-assoc-set bp1 "active_phase_id" phase-id)
                                                              "active_task_id"
                                                              task-id
                                                            ) ;pi-blueprint-assoc-set
                                      "status"
                                      "active"
                                    ) ;pi-blueprint-assoc-set
              "updated_at"
              now
            ) ;pi-blueprint-assoc-set
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%set-task-field task task-id key val)
      (if (string=? (pi-task-id task) task-id)
        (pi-blueprint-assoc-set task key val)
        task
      ) ;if
    ) ;define

    (define (%patch-task-in-phase phase task-id key val)
      (if (pi-blueprint-find-task (pi-phase-tasks phase) task-id)
        (pi-blueprint-map-phase-tasks phase
          (lambda (task) (%set-task-field task task-id key val))
        ) ;pi-blueprint-map-phase-tasks
        phase
      ) ;if
    ) ;define

    (define (pi-blueprint-patch-task bp task-id key val)
      (let* ((tasks (pi-blueprint-all-tasks bp))
             (task (pi-blueprint-find-task tasks task-id))
            ) ;
        (if (not task)
          bp
          (let ((phases (map (lambda (phase) (%patch-task-in-phase phase task-id key val))
                          (pi-blueprint-phases bp)
                        ) ;map
                ) ;phases
               ) ;
            (pi-blueprint-assoc-set (pi-blueprint-assoc-set bp "phases" (list->vector phases))
              "updated_at"
              (pi-iso-8601-utc-now)
            ) ;pi-blueprint-assoc-set
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-blueprint-set-notes bp task-id notes)
      (pi-blueprint-patch-task bp task-id "notes" (if notes notes 'null))
    ) ;define

  ) ;begin
) ;define-library

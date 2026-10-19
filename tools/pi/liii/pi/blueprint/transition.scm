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

(define-library (liii pi blueprint transition)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi time)
    (liii pi blueprint types)
    (liii pi blueprint graph)
    (liii pi blueprint state)
  ) ;import
  (export pi-blueprint-complete-task pi-blueprint-skip-task
    pi-blueprint-set-gate pi-blueprint-advance-phase pi-blueprint-create
    pi-blueprint-abandon
  ) ;export
  (begin

    (define (%phase-all-done? bp phase-id)
      (let ((phase (pi-blueprint-find-phase bp phase-id)))
        (and phase (every pi-task-done? (pi-phase-tasks phase)))
      ) ;let
    ) ;define

    (define (%gates-all-passed? phase)
      (let ((gates (pi-phase-gates phase)))
        (or (null? gates) (every pi-gate-passed? gates))
      ) ;let
    ) ;define

    (define (%mark-completed-if-done bp)
      (let* ((tasks (pi-blueprint-all-tasks bp))
             (done (every pi-task-done? tasks))
             (verified (every %gates-all-passed? (pi-blueprint-phases bp)))
            ) ;
        (if (and done verified) (pi-blueprint-assoc-set bp "status" "completed") bp)
      ) ;let*
    ) ;define

    (define (%resolve-completion bp task-id)
      (let* ((bp1 (pi-blueprint-recompute-blocked bp))
             (active (pi-blueprint-active-task-id bp1))
             (bp2 (if (equal? active task-id)
                    (pi-blueprint-assoc-set bp1 "active_task_id" (pi-blueprint-next-task-id bp1))
                    bp1
                  ) ;if
             ) ;bp2
             (active-phase (pi-blueprint-active-phase-id bp2))
            ) ;
        (if (and active-phase (%phase-all-done? bp2 active-phase))
          (pi-blueprint-update-phase bp2
            active-phase
            (lambda (p)
              (pi-blueprint-set-field (pi-blueprint-set-field p "status" "completed")
                "completed_at"
                (pi-iso-8601-utc-now)
              ) ;pi-blueprint-set-field
            ) ;lambda
          ) ;pi-blueprint-update-phase
          bp2
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-blueprint-complete-task bp task-id)
      (let* ((tasks (pi-blueprint-all-tasks bp))
             (task (pi-blueprint-find-task tasks task-id))
            ) ;
        (if (or (not task) (pi-task-done? task))
          bp
          (let* ((bp1 (pi-blueprint-patch-task (pi-blueprint-patch-task bp task-id "status" "completed")
                        task-id
                        "completed_at"
                        (pi-iso-8601-utc-now)
                      ) ;pi-blueprint-patch-task
                 ) ;bp1
                 (bp2 (%resolve-completion bp1 task-id))
                ) ;
            (%mark-completed-if-done bp2)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-blueprint-skip-task bp task-id)
      (let* ((tasks (pi-blueprint-all-tasks bp))
             (task (pi-blueprint-find-task tasks task-id))
            ) ;
        (if (not task)
          bp
          (let* ((bp1 (pi-blueprint-patch-task (pi-blueprint-patch-task bp task-id "status" "skipped")
                        task-id
                        "completed_at"
                        (pi-iso-8601-utc-now)
                      ) ;pi-blueprint-patch-task
                 ) ;bp1
                 (bp2 (%resolve-completion bp1 task-id))
                ) ;
            (%mark-completed-if-done bp2)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (%checked-gate gate passed error-msg)
      (let ((checked (pi-blueprint-set-field (pi-blueprint-set-field gate "passed" passed)
                       "last_checked_at"
                       (pi-iso-8601-utc-now)
                     ) ;pi-blueprint-set-field
            ) ;checked
           ) ;
        (if error-msg
          (pi-blueprint-set-field checked "error_message" error-msg)
          checked
        ) ;if
      ) ;let
    ) ;define

    (define (%update-gate-at gate i target passed error-msg)
      (if (= i target) (%checked-gate gate passed error-msg) gate)
    ) ;define

    (define (%rewrite-gates target target-index passed error-msg)
      (let loop
        ((rest (pi-phase-gates target)) (i 0) (acc '()))
        (if (null? rest)
          (pi-blueprint-set-field target
            "verification_gates"
            (list->vector (reverse acc))
          ) ;pi-blueprint-set-field
          (let ((gate (%update-gate-at (car rest) i target-index passed error-msg)))
            (loop (cdr rest) (+ i 1) (cons gate acc))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-blueprint-set-gate bp phase-id gate-index passed error-msg)
      (let ((phase (pi-blueprint-find-phase bp phase-id)))
        (if
          (or (not phase) (>= gate-index (length (pi-phase-gates phase))))
          bp
          (pi-blueprint-update-phase bp
            phase-id
            (lambda (target) (%rewrite-gates target gate-index passed error-msg))
          ) ;pi-blueprint-update-phase
        ) ;if
      ) ;let
    ) ;define

    (define (%list-index pred lst)
      (let loop
        ((rest lst) (i 0))
        (cond ((null? rest) #f)
              ((pred (car rest)) i)
              (else (loop (cdr rest) (+ i 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-blueprint-advance-phase bp)
      (let* ((phase-id (pi-blueprint-active-phase-id bp))
             (phase (and phase-id (pi-blueprint-find-phase bp phase-id)))
            ) ;
        (if (not phase)
          bp
          (if (and (%phase-all-done? bp phase-id) (%gates-all-passed? phase))
            (let* ((bp1
                     (pi-blueprint-update-phase bp
                       phase-id
                       (lambda (p)
                         (pi-blueprint-set-field (pi-blueprint-set-field p "status" "verified")
                           "completed_at"
                           (pi-iso-8601-utc-now)
                         ) ;pi-blueprint-set-field
                       ) ;lambda
                     ) ;pi-blueprint-update-phase
                   ) ;bp1
                   (ids (map pi-phase-id (pi-blueprint-phases bp1)))
                   (idx (list-index (lambda (id) (string=? id phase-id)) ids))
                   (next-id (and idx (< (+ idx 1) (length ids)) (list-ref ids (+ idx 1))))
                  ) ;
              (if next-id
                (let* ((bp2 (pi-blueprint-assoc-set (pi-blueprint-assoc-set bp1 "active_phase_id" next-id)
                              "active_task_id"
                              'null
                            ) ;pi-blueprint-assoc-set
                       ) ;bp2
                       (next (pi-blueprint-get-next-task bp2))
                      ) ;
                  (if next (pi-blueprint-assoc-set bp2 "active_task_id" (pi-task-id next)) bp2)
                ) ;let*
                (pi-blueprint-assoc-set (pi-blueprint-assoc-set (pi-blueprint-assoc-set bp1 "active_phase_id" 'null)
                                          "active_task_id"
                                          'null
                                        ) ;pi-blueprint-assoc-set
                  "status"
                  "completed"
                ) ;pi-blueprint-assoc-set
              ) ;if
            ) ;let*
            bp
          ) ;if
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-blueprint-create id objective project-id phases)
      (let* ((bp (pi-blueprint-blueprint id objective project-id phases))
             (bp1 (pi-blueprint-recompute-blocked bp))
             (next (pi-blueprint-get-next-task bp1))
            ) ;
        (if next (pi-blueprint-assoc-set bp1 "active_task_id" (pi-task-id next)) bp1)
      ) ;let*
    ) ;define

    (define (pi-blueprint-abandon bp)
      (let ((bp1 (pi-blueprint-assoc-set bp "status" "abandoned")))
        (pi-blueprint-assoc-set (pi-blueprint-assoc-set (pi-blueprint-assoc-set bp1 "active_phase_id" 'null)
                                  "active_task_id"
                                  'null
                                ) ;pi-blueprint-assoc-set
          "updated_at"
          (pi-iso-8601-utc-now)
        ) ;pi-blueprint-assoc-set
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi blueprint render)
  (import (scheme base) (liii string) (liii list) (liii pi blueprint types))
  (export pi-render-blueprint-plan pi-format-blueprint-progress
    pi-render-next-task pi-blueprint-task-counts
  ) ;export
  (begin

    (define (%date-only iso)
      (if (and (string? iso) (>= (string-length iso) 10))
        (substring iso 0 10)
        (if (string? iso) iso "")
      ) ;if
    ) ;define

    (define (%done-count tasks)
      (length (filter pi-task-done? tasks))
    ) ;define

    (define (pi-blueprint-task-counts bp)
      (let* ((tasks (pi-blueprint-all-tasks bp))
             (total (length tasks))
             (done (%done-count tasks))
            ) ;
        (list (cons 'done done) (cons 'total total))
      ) ;let*
    ) ;define

    (define (pi-format-blueprint-progress bp)
      (let* ((counts (pi-blueprint-task-counts bp))
             (done (cdr (assoc 'done counts)))
             (total (cdr (assoc 'total counts)))
             (pct
               (if (= total 0) 0 (floor (/ (* 100 done) total)))
             ) ;pct
            ) ;
        (string-append "Progress: "
          (number->string done)
          "/"
          (number->string total)
          " tasks ("
          (number->string pct)
          "%)"
        ) ;string-append
      ) ;let*
    ) ;define

    (define (%render-task task)
      (let* ((mark (if (pi-task-done? task) "x" " "))
             (status (pi-task-status task))
             (annotation (cond ((string=? status "in_progress") " *(in progress)*")
                               ((string=? status "blocked") " *(blocked)*")
                               ((string=? status "skipped") " *(skipped)*")
                               (else "")
                         ) ;cond
             ) ;annotation
            ) ;
        (string-append "- ["
          mark
          "] "
          (pi-task-id task)
          " "
          (pi-task-title task)
          annotation
        ) ;string-append
      ) ;let*
    ) ;define

    (define (%render-phase phase active-id)
      (let* ((active-tag
               (if (and active-id (string=? (pi-phase-id phase) active-id)) " (active)" "")
             ) ;active-tag
             (tasks (pi-phase-tasks phase))
             (gates (pi-phase-gates phase))
             (lines
               (append
                 (list (string-append "## Phase "
                         (pi-phase-id phase)
                         ": "
                         (pi-phase-title phase)
                         active-tag
                       ) ;string-append
                   ""
                   (string-append "**Status:** "
                     (pi-phase-status phase)
                     " | "
                     (number->string (%done-count tasks))
                     "/"
                     (number->string (length tasks))
                     " tasks completed"
                   ) ;string-append
                 ) ;list
                 (if (string-null? (pi-phase-description phase))
                   '()
                   (list "" (pi-phase-description phase))
                 ) ;if
                 (if (null? tasks) '() (append '("" "### Tasks" "") (map %render-task tasks)))
                 (if (null? gates)
                   '()
                   (append '("" "### Verification Gates" "")
                     (map
                       (lambda (g)
                         (string-append "- ["
                           (if (pi-gate-passed? g) "x" " ")
                           "] "
                           (pi-gate-description g)
                         ) ;string-append
                       ) ;lambda
                       gates
                     ) ;map
                   ) ;append
                 ) ;if
               ) ;append
             ) ;lines
            ) ;
        (string-join lines "\n")
      ) ;let*
    ) ;define

    (define (pi-render-blueprint-plan bp)
      (let* ((header
               (list (string-append "# Blueprint: " (pi-blueprint-objective bp))
                 ""
                 (string-append "**Status:** "
                   (pi-blueprint-status bp)
                   " | Created: "
                   (%date-only (pi-blueprint-jref bp "created_at" ""))
                   " | Updated: "
                   (%date-only (pi-blueprint-jref bp "updated_at" ""))
                 ) ;string-append
               ) ;list
             ) ;header
             (body
               (apply append
                 (map
                   (lambda (p)
                     (list "" "---" "" (%render-phase p (pi-blueprint-active-phase-id bp)))
                   ) ;lambda
                   (pi-blueprint-phases bp)
                 ) ;map
               ) ;apply
             ) ;body
            ) ;
        (string-append (string-join (append header body) "\n") "\n")
      ) ;let*
    ) ;define

    (define (pi-render-next-task task)
      (let ((lines
              (append (list (string-append "## Next Task: " (pi-task-id task) " - " (pi-task-title task))
                        ""
                        (pi-task-description task)
                      ) ;list
                (if (null? (pi-task-acceptance task))
                  '()
                  (append '("" "**Acceptance criteria:**")
                    (map (lambda (c) (string-append "- " c)) (pi-task-acceptance task))
                  ) ;append
                ) ;if
                (if (null? (pi-task-file-targets task))
                  '()
                  (append '("" "**File targets:**")
                    (map (lambda (f) (string-append "- " f)) (pi-task-file-targets task))
                  ) ;append
                ) ;if
                (if (null? (pi-task-dependencies task))
                  '()
                  (list ""
                    (string-append "**Dependencies:** "
                      (string-join (pi-task-dependencies task) ", ")
                      " (all completed)"
                    ) ;string-append
                  ) ;list
                ) ;if
              ) ;append
            ) ;lines
           ) ;
        (string-join lines "\n")
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

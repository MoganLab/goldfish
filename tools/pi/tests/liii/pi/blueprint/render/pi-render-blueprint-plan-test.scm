(import (liii check)
        (liii string)
        (liii pi blueprint types)
        (liii pi blueprint transition)
        (liii pi blueprint render)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "desc" '() '() deps)
) ;define

(define bp
  (pi-blueprint-create "bp-1" "Add login" "proj"
    (list (pi-blueprint-phase "1" "Foundations" "base work"
            (list (task "1.1" '()) (task "1.2" '("1.1")))
            (list (pi-blueprint-gate "custom_command" "Run gates" "true"))
          ) ;pi-blueprint-phase
      (pi-blueprint-phase "2" "Flow" ""
        (list (task "2.1" '("1.2")) (task "2.2" '("2.1")))
        '()
      ) ;pi-blueprint-phase
    ) ;list
  ) ;pi-blueprint-create
) ;define

;; pi-render-blueprint-plan
(let* ((bp1 (pi-blueprint-complete-task bp "1.1"))
       (md (pi-render-blueprint-plan bp1))
      ) ;
  (check-true (string-starts? md "# Blueprint: Add login"))
  (check-true (string-contains? md "**Status:** active"))
  (check-true (string-contains? md "## Phase 1: Foundations (active)"))
  (check-true (string-contains? md "**Status:** pending | 1/2 tasks completed"))
  (check-true (string-contains? md "- [x] 1.1 Task 1.1"))
  (check-true (string-contains? md "- [ ] 1.2 Task 1.2"))
  (check-true (string-contains? md "### Verification Gates"))
  (check-true (string-contains? md "- [ ] Run gates"))
  (check-true (string-ends? md "\n"))
) ;let*

;; 无活动阶段（已完成）时仍渲染全部阶段。
(let ((md (pi-render-blueprint-plan (pi-blueprint-abandon bp))))
  (check-true (string-contains? md "**Status:** abandoned"))
  (check-false (string-contains? md "(active)"))
) ;let

(check-report)

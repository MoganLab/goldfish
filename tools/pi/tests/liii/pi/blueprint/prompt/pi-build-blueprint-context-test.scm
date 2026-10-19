(import (liii check)
        (liii string)
        (liii pi blueprint types)
        (liii pi blueprint state)
        (liii pi blueprint transition)
        (liii pi blueprint prompt)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "does stuff"
    '("works") '("a.scm") deps
  ) ;pi-blueprint-task
) ;define

(define bp
  (pi-blueprint-create "bp-1" "Add login" "proj"
    (list (pi-blueprint-phase "1" "Foundations" ""
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

;; pi-build-blueprint-context
(let* ((bp1 (pi-blueprint-start-task bp "1.1" #f))
       (ctx (pi-build-blueprint-context bp1))
      ) ;
  (check-true (string-contains? ctx "## Active Blueprint: \"Add login\""))
  (check-true (string-contains? ctx "### Current Phase: Phase 1 - Foundations"))
  (check-true (string-contains? ctx "Status: active | 0/2 tasks completed"))
  (check-true (string-contains? ctx "### Current Task: 1.1 - Task 1.1"))
  (check-true (string-contains? ctx "does stuff"))
  (check-true (string-contains? ctx "**Acceptance criteria:**"))
  (check-true (string-contains? ctx "- works"))
  (check-true (string-contains? ctx "**File targets:**"))
  (check-true (string-contains? ctx "- a.scm"))
  (check-true (string-contains? ctx "### Blocked Tasks"))
  (check-true (string-contains? ctx "1.2 \"Task 1.2\" - blocked by 1.1"))
  (check-true (string-contains? ctx "### Phase 1 Verification Gates"))
  (check-true (string-contains? ctx "- [ ] Run gates"))
) ;let*

;; 无活动阶段时给出说明。
(let ((ctx (pi-build-blueprint-context (pi-blueprint-abandon bp))))
  (check-true (string-contains? ctx "No active phase"))
  (check-true (string-contains? ctx "Status: abandoned"))
) ;let

;; pi-blueprint-turn-system-prompt 每回合读取最新状态。
(let* ((holder (vector (pi-blueprint-start-task bp "1.1" #f)))
       (provider (pi-blueprint-turn-system-prompt "BASE"
                   (lambda () (vector-ref holder 0))
                 ) ;pi-blueprint-turn-system-prompt
       ) ;provider
      ) ;
  (check-true (string-starts? (provider '()) "BASE\n\n## Active Blueprint"))
  (vector-set! holder 0 #f)
  (check (provider '()) => "BASE")
) ;let*

(check-report)

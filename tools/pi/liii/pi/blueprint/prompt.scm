;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi blueprint prompt)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi blueprint types)
    (liii pi blueprint graph)
  ) ;import
  (export pi-build-blueprint-generate-prompt pi-build-blueprint-context
    pi-build-blueprint-turn-prompt pi-blueprint-turn-system-prompt
  ) ;export
  (begin

    (define (pi-build-blueprint-generate-prompt objective)
      (string-append "You are a senior software architect planning a complex implementation.\n\n"
        "## Objective\n" objective "\n\n" "## Instructions\n\n"
        "Analyze the codebase and the objective above (you may use read, bash, grep, find and ls), "
        "then respond with ONE JSON object and nothing else -- no prose, no markdown fences.\n\n"
        "### Plan structure requirements:\n"
        "1. Break the work into 2-6 phases, ordered by dependency (foundations first)\n"
        "2. Each phase should have 2-8 concrete, agent-sized tasks\n"
        "3. Each task must have:\n" "   - A clear, imperative title\n"
        "   - A brief description of what to implement\n"
        "   - acceptance_criteria: testable conditions (2-3 items)\n"
        "   - file_targets: files to create or modify, using real paths when possible\n"
        "   - dependencies: task IDs this depends on (e.g. \"1.1\", \"2.3\")\n"
        "4. Each phase must have verification_gates:\n"
        "   - Only \"custom_command\" (with a shell command) and \"user_approval\" are supported\n"
        "   - Do NOT use \"tests_pass\" or \"typecheck_clean\"; they are unavailable in script mode\n"
        "   - Prefer custom_command gates such as \"bin/gf check && bin/gf fmt --check\" or a test command\n"
        "5. Tasks within a phase can depend on tasks in the same or earlier phases\n"
        "6. Task IDs follow the format \"phase.task\" (e.g. \"1.1\", \"1.2\", \"2.1\")\n\n"
        "### Quality criteria:\n"
        "- Each task should be completable in a single agent session\n"
        "- Dependencies should be minimal and acyclic\n"
        "- Acceptance criteria should be specific and testable\n"
        "- File targets should reference real paths in the codebase when possible\n\n"
        "### JSON shape\n"
        "{\"objective\":\"...\",\"phases\":[{\"id\":\"1\",\"title\":\"...\",\"description\":\"...\","
        "\"tasks\":[{\"id\":\"1.1\",\"title\":\"...\",\"description\":\"...\","
        "\"acceptance_criteria\":[\"...\"],\"file_targets\":[\"...\"],\"dependencies\":[]}],"
        "\"verification_gates\":[{\"type\":\"custom_command\",\"description\":\"...\","
        "\"command\":\"...\"}]}]}\n"
      ) ;string-append
    ) ;define

    (define (%task-context task)
      (let ((lines
              (append (list (string-append "### Current Task: "
                              (pi-task-id task)
                              " - "
                              (pi-task-title task)
                            ) ;string-append
                      ) ;list
                (if (string-null? (pi-task-description task))
                  '()
                  (list (pi-task-description task))
                ) ;if
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
              ) ;append
            ) ;lines
           ) ;
        (string-join lines "\n")
      ) ;let
    ) ;define

    (define (%gate-context phase)
      (let ((gates (pi-phase-gates phase)))
        (if (null? gates)
          '()
          (append (list "" (string-append "### Phase " (pi-phase-id phase) " Verification Gates"))
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
      ) ;let
    ) ;define

    (define (%blocked-context bp)
      (let* ((tasks (pi-blueprint-all-tasks bp))
             (phase-id (pi-blueprint-active-phase-id bp))
             (phase
               (and phase-id
                 (find (lambda (p) (string=? (pi-phase-id p) phase-id)) (pi-blueprint-phases bp))
               ) ;and
             ) ;phase
             (active-tasks
               (if phase
                 (filter (lambda (t) (string=? (pi-task-status t) "blocked"))
                   (pi-phase-tasks phase)
                 ) ;filter
                 '()
               ) ;if
             ) ;active-tasks
            ) ;
        (if (null? active-tasks)
          '()
          (append '("" "### Blocked Tasks")
            (map
              (lambda (t)
                (string-append "- "
                  (pi-task-id t)
                  " \""
                  (pi-task-title t)
                  "\" - blocked by "
                  (string-join (pi-blueprint-blocking-tasks tasks (pi-task-id t)) ", ")
                ) ;string-append
              ) ;lambda
              active-tasks
            ) ;map
          ) ;append
        ) ;if
      ) ;let*
    ) ;define

    ;; 每回合注入的蓝图上下文，结构对齐 native phase-context.ts。
    (define (pi-build-blueprint-context bp)
      (let* ((phase-id (pi-blueprint-active-phase-id bp))
             (phase
               (and phase-id
                 (find (lambda (p) (string=? (pi-phase-id p) phase-id)) (pi-blueprint-phases bp))
               ) ;and
             ) ;phase
             (task-id (pi-blueprint-active-task-id bp))
             (task
               (and phase
                 task-id
                 (find (lambda (t) (string=? (pi-task-id t) task-id)) (pi-phase-tasks phase))
               ) ;and
             ) ;task
             (lines (list (string-append "## Active Blueprint: \"" (pi-blueprint-objective bp) "\"")
                      ""
                    ) ;list
             ) ;lines
            ) ;
        (if (not phase)
          (string-join
            (append lines
              (list "No active phase. All phases may be complete or verified."
                (string-append "Status: " (pi-blueprint-status bp))
              ) ;list
            ) ;append
            "\n"
          ) ;string-join
          (let* ((tasks (pi-phase-tasks phase))
                 (done (length (filter pi-task-done? tasks)))
                 (phase-lines
                   (list (string-append "### Current Phase: Phase "
                           (pi-phase-id phase)
                           " - "
                           (pi-phase-title phase)
                         ) ;string-append
                     (string-append "Status: "
                       (pi-phase-status phase)
                       " | "
                       (number->string done)
                       "/"
                       (number->string (length tasks))
                       " tasks completed"
                     ) ;string-append
                   ) ;list
                 ) ;phase-lines
                ) ;
            (string-join
              (append lines
                phase-lines
                (if task (append '("") (list (%task-context task))) '())
                (%blocked-context bp)
                (%gate-context phase)
              ) ;append
              "\n"
            ) ;string-join
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-build-blueprint-turn-prompt task)
      (string-append "Implement the following blueprint task in this workspace. "
        "When you are done, call blueprint_update to mark it completed "
        "(or skipped with a note).\n\n"
        (pi-render-task-for-prompt task)
      ) ;string-append
    ) ;define

    (define (pi-render-task-for-prompt task)
      (let ((lines
              (append (list (string-append "## Task: " (pi-task-id task) " - " (pi-task-title task))
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
              ) ;append
            ) ;lines
           ) ;
        (string-join lines "\n")
      ) ;let
    ) ;define

    ;; 返回系统提示词提供者：每回合按当前蓝图状态追加注入块。
    (define (pi-blueprint-turn-system-prompt base get-state)
      (lambda (tool-calls)
        (let ((bp (get-state)))
          (if bp (string-append base "\n\n" (pi-build-blueprint-context bp)) base)
        ) ;let
      ) ;lambda
    ) ;define

  ) ;begin
) ;define-library

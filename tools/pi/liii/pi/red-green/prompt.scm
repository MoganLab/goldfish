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

(define-library (liii pi red-green prompt)
  (import (scheme base) (liii string) (liii list) (liii pi red-green state))
  (export pi-tdd-red-phase-prompt pi-tdd-green-phase-prompt
    pi-tdd-refactor-phase-prompt pi-build-red-green-prompt
    pi-tdd-turn-system-prompt
  ) ;export
  (begin

    (define (pi-tdd-red-phase-prompt task)
      (string-append "## TDD: RED Phase -- Write Failing Tests\n\n" "**Task:** "
        task "\n\n"
        "Write failing tests for this task. Do NOT write implementation yet.\n\n"
        "### Rules\n" "- Write tests that describe the expected behavior\n"
        "- Each test should assert ONE thing\n"
        "- Use real code, not mocks, unless external I/O forces it\n"
        "- After writing tests, RUN them\n"
        "- Confirm they FAIL because the feature is missing, not because of typos or import errors\n"
        "- If tests fail with runtime/compile errors, fix the test code first\n"
        "- Implementation files are write-protected in this phase; only test files may change\n\n"
        "### Rationalization Prevention\n" "| Excuse | Rebuttal |\n"
        "|--------|----------|\n"
        "| \"Too simple to test\" | Simple code breaks. A test takes 30 seconds. Write it. |\n"
        "| \"I'll write tests after\" | No. Tests first. That's the point. |\n"
        "| \"Just this once\" | There is no \"just this once.\" TDD is a discipline, not a suggestion. |\n"
        "| \"The types guarantee correctness\" | Types don't catch logic errors. Write the test. |\n\n"
        "### Verification Gate\n"
        "After writing tests, you MUST run them and confirm:\n"
        "1. Tests execute without runtime/compile errors\n"
        "2. Tests FAIL because the feature is not yet implemented\n"
        "3. The failure messages describe the missing behavior"
      ) ;string-append
    ) ;define

    (define (pi-tdd-green-phase-prompt task failed-count)
      (string-append "## TDD: GREEN Phase -- Make Tests Pass\n\n"
        "**Task:** "
        task
        "\n"
        "**Failing tests:** "
        (number->string failed-count)
        "\n\n"
        "Tests are failing as expected. Write the SIMPLEST code to make them pass.\n\n"
        "### Rules\n"
        "- Write the minimal implementation to satisfy the failing tests\n"
        "- Do not add features beyond what tests require\n"
        "- Do not refactor other code\n"
        "- Do not \"improve\" or \"clean up\" during this phase\n"
        "- After writing implementation, RUN tests and confirm they pass\n\n"
        "### Anti-patterns\n"
        "- Don't add untested features \"while you're in there\"\n"
        "- Don't mock behavior that should be real\n"
        "- Don't add test-only methods to production classes"
      ) ;string-append
    ) ;define

    (define (pi-tdd-refactor-phase-prompt task)
      (string-append "## TDD: REFACTOR Phase -- Improve Without Breaking\n\n"
        "**Task:** " task "\n\n"
        "All tests pass. Refactor if needed: remove duplication, improve names, extract helpers.\n\n"
        "### Rules\n" "- Keep tests green. Do not add new behavior.\n"
        "- RUN tests after every refactor to confirm they still pass\n"
        "- If a refactoring breaks tests, revert the last change immediately\n"
        "- This phase is optional. If the code is clean, advance to the next task."
      ) ;string-append
    ) ;define

    (define (%stale-test-warning)
      "**Stale test results.** The last test run was in a previous turn. Run tests again before claiming they pass. Fresh evidence required."
    ) ;define

    (define (%ordering-warning)
      "**TDD Warning.** Implementation files were edited before any test files in this cycle. Consider writing tests first."
    ) ;define

    (define (%failed-count state)
      (let ((run (pi-tdd-state-last-test-run state)))
        (if run (cdr (assoc 'failed run)) 0)
      ) ;let
    ) ;define

    (define (%phase-prompt state)
      (let ((phase (pi-tdd-state-phase state)) (task (pi-tdd-state-task state)))
        (case phase
         ((red) (pi-tdd-red-phase-prompt task))
         ((green) (pi-tdd-green-phase-prompt task (%failed-count state)))
         ((refactor) (pi-tdd-refactor-phase-prompt task))
         (else #f)
        ) ;case
      ) ;let
    ) ;define

    (define (%ordering-violation? state)
      (and (eq? (pi-tdd-state-phase state) 'red)
        (pair? (pi-tdd-state-impl-files state))
        (null? (pi-tdd-state-test-files state))
      ) ;and
    ) ;define

    (define (pi-build-red-green-prompt state)
      (if (not state)
        #f
        (let* ((phase-prompt (%phase-prompt state))
               (parts
                 (filter
                   (lambda (x) (and (string? x) (not (string-null? x))))
                   (append (if phase-prompt (list phase-prompt) '())
                     (if (pi-tdd-stale-test-run? state) (list (%stale-test-warning)) '())
                     (if (%ordering-violation? state) (list (%ordering-warning)) '())
                     (let ((warning (pi-tdd-state-warning state)))
                       (if (and (string? warning) (not (string-null? warning)))
                         (list (string-append "**TDD warning:** " warning))
                         '()
                       ) ;if
                     ) ;let
                   ) ;append
                 ) ;filter
               ) ;parts
              ) ;
          (if (null? parts) #f (string-append "\n\n" (string-join parts "\n\n")))
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-tdd-turn-system-prompt base)
      (lambda (tool-calls)
        (let ((block (pi-build-red-green-prompt (pi-tdd-current-state))))
          (if block (string-append base block) base)
        ) ;let
      ) ;lambda
    ) ;define

  ) ;begin
) ;define-library

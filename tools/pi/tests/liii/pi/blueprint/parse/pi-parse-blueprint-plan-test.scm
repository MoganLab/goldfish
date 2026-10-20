(import (liii check)
        (liii string)
        (liii pi blueprint types)
        (liii pi blueprint parse)
) ;import

(check-set-mode! 'report-failed)

(define text
  "```json
{\"objective\":\"Add login\",\"phases\":[
 {\"id\":\"1\",\"title\":\"Foundations\",\"description\":\"base\",
  \"tasks\":[
   {\"id\":\"1.1\",\"title\":\"Add model\",\"description\":\"m\",
    \"acceptance_criteria\":[\"model exists\"],\"file_targets\":[\"a.scm\"],\"dependencies\":[]},
   {\"id\":\"1.2\",\"title\":\"Add store\",\"description\":\"s\",
    \"acceptance_criteria\":[\"store works\"],\"file_targets\":[\"b.scm\"],\"dependencies\":[\"1.1\"]}],
  \"verification_gates\":[{\"type\":\"custom_command\",\"description\":\"check\",\"command\":\"true\"}]},
 {\"id\":\"2\",\"title\":\"Flow\",\"description\":\"flow\",
  \"tasks\":[
   {\"id\":\"2.1\",\"title\":\"Wire flow\",\"description\":\"w\",
    \"acceptance_criteria\":[\"flow works\"],\"file_targets\":[\"c.scm\"],\"dependencies\":[\"1.2\"]},
   {\"id\":\"2.2\",\"title\":\"Add tests\",\"description\":\"t\",
    \"acceptance_criteria\":[\"tests pass\"],\"file_targets\":[\"d.scm\"],\"dependencies\":[\"2.1\"]}],
  \"verification_gates\":[{\"type\":\"user_approval\",\"description\":\"approve\"}]}]}
```"
) ;define

;; pi-parse-blueprint-plan
(let* ((plan (pi-parse-blueprint-plan text "Add login"))
       (phases (pi-blueprint-vector->list (pi-blueprint-jref plan "phases" #())))
       (first (car phases))
       (tasks (pi-phase-tasks first))
       (gates (pi-phase-gates first))
      ) ;
  (check (pi-blueprint-jref plan "objective" #f) => "Add login")
  (check (length phases) => 2)
  (check (pi-phase-id first) => "1")
  (check (pi-phase-title first) => "Foundations")
  (check (length tasks) => 2)
  (check (pi-task-id (car tasks)) => "1.1")
  (check (pi-task-acceptance (car tasks)) => '("model exists"))
  (check (pi-task-file-targets (car tasks)) => '("a.scm"))
  (check (pi-task-dependencies (cadr tasks)) => '("1.1"))
  (check (pi-task-status (car tasks)) => "pending")
  (check (length gates) => 1)
  (check (pi-gate-type (car gates)) => "custom_command")
  (check (pi-gate-command (car gates)) => "true")
  (check (pi-gate-passed? (car gates)) => #f)
) ;let*

;; 无 JSON → blueprint-plan-error。
(check-catch 'blueprint-plan-error (pi-parse-blueprint-plan "no json" "x"))
;; 缺 phases → blueprint-plan-error。
(check-catch 'blueprint-plan-error (pi-parse-blueprint-plan "{\"objective\":\"x\"}" "x"))
;; 非法 JSON → blueprint-plan-error。
(check-catch 'blueprint-plan-error (pi-parse-blueprint-plan "{bad" "x"))

(check-report)

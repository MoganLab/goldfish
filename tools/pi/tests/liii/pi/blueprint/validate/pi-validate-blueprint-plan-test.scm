(import (liii check)
        (liii string)
        (liii pi blueprint types)
        (liii pi blueprint validate)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "desc" '("ok") '("a.scm") deps)
) ;define

(define (phase id tasks type)
  (pi-blueprint-phase id (string-append "Phase " id) ""
    tasks
    (list (pi-blueprint-gate type "gate" (if (string=? type "custom_command") "true" #f)))
  ) ;pi-blueprint-phase
) ;define

(define (plan phases)
  (list (cons "objective" "obj")
    (cons "phases" (list->vector phases))
  ) ;list
) ;define

(define (ok-plan)
  (plan (list (phase "1" (list (task "1.1" '()) (task "1.2" '("1.1"))) "custom_command")
          (phase "2" (list (task "2.1" '("1.2")) (task "2.2" '("2.1"))) "user_approval")
        ) ;list
  ) ;plan
) ;define

;; pi-validate-blueprint-plan
(check (pi-validate-blueprint-plan (ok-plan)) => #t)

;; 阶段数越界。
(check-catch 'blueprint-plan-error
  (pi-validate-blueprint-plan (plan (list (phase "1"
                                              (list (task "1.1" '()) (task "1.2" '()))
                                              "custom_command"
                                            ) ;phase
                                    ) ;list
                              ) ;plan
  ) ;pi-validate-blueprint-plan
) ;check-catch

;; 每阶段任务数越界。
(check-catch 'blueprint-plan-error
  (pi-validate-blueprint-plan
    (plan (list (phase "1" (list (task "1.1" '())) "custom_command")
            (phase "2" (list (task "2.1" '()) (task "2.2" '())) "custom_command")
          ) ;list
    ) ;plan
  ) ;pi-validate-blueprint-plan
) ;check-catch

;; 重复任务 ID。
(check-catch 'blueprint-plan-error
  (pi-validate-blueprint-plan
    (plan (list (phase "1" (list (task "1.1" '()) (task "1.1" '())) "custom_command")
            (phase "2" (list (task "2.1" '()) (task "2.2" '())) "custom_command")
          ) ;list
    ) ;plan
  ) ;pi-validate-blueprint-plan
) ;check-catch

;; 依赖不存在的任务。
(check-catch 'blueprint-plan-error
  (pi-validate-blueprint-plan
    (plan (list (phase "1" (list (task "1.1" '()) (task "1.2" '("9.9"))) "custom_command")
            (phase "2" (list (task "2.1" '()) (task "2.2" '())) "custom_command")
          ) ;list
    ) ;plan
  ) ;pi-validate-blueprint-plan
) ;check-catch

;; 依赖成环。
(check-catch 'blueprint-plan-error
  (pi-validate-blueprint-plan
    (plan (list (phase "1" (list (task "1.1" '("1.2")) (task "1.2" '("1.1")))
              "custom_command"
            ) ;phase
            (phase "2" (list (task "2.1" '()) (task "2.2" '())) "custom_command")
          ) ;list
    ) ;plan
  ) ;pi-validate-blueprint-plan
) ;check-catch

;; 非法任务 ID 形状。
(check-catch 'blueprint-plan-error
  (pi-validate-blueprint-plan
    (plan (list (phase "1" (list (task "one" '()) (task "1.2" '())) "custom_command")
            (phase "2" (list (task "2.1" '()) (task "2.2" '())) "custom_command")
          ) ;list
    ) ;plan
  ) ;pi-validate-blueprint-plan
) ;check-catch

;; tests_pass / typecheck_clean 被明确拒绝。
(check-catch 'blueprint-plan-error
  (pi-validate-blueprint-plan
    (plan (list (phase "1" (list (task "1.1" '()) (task "1.2" '())) "tests_pass")
            (phase "2" (list (task "2.1" '()) (task "2.2" '())) "custom_command")
          ) ;list
    ) ;plan
  ) ;pi-validate-blueprint-plan
) ;check-catch
(check-catch 'blueprint-plan-error
  (pi-validate-blueprint-plan
    (plan (list (phase "1" (list (task "1.1" '()) (task "1.2" '())) "typecheck_clean")
            (phase "2" (list (task "2.1" '()) (task "2.2" '())) "custom_command")
          ) ;list
    ) ;plan
  ) ;pi-validate-blueprint-plan
) ;check-catch

;; 未知门禁类型。
(check-catch 'blueprint-plan-error
  (pi-validate-blueprint-plan
    (plan (list (phase "1" (list (task "1.1" '()) (task "1.2" '())) "wrong")
            (phase "2" (list (task "2.1" '()) (task "2.2" '())) "custom_command")
          ) ;list
    ) ;plan
  ) ;pi-validate-blueprint-plan
) ;check-catch

;; custom_command 缺少命令。
(check-catch 'blueprint-plan-error
  (pi-validate-blueprint-plan
    (plan (list (pi-blueprint-phase "1" "Phase 1" ""
                (list (task "1.1" '()) (task "1.2" '()))
                (list (pi-blueprint-gate "custom_command" "gate" #f))
              ) ;pi-blueprint-phase
            (phase "2" (list (task "2.1" '()) (task "2.2" '())) "custom_command")
          ) ;list
    ) ;plan
  ) ;pi-validate-blueprint-plan
) ;check-catch

(check-report)

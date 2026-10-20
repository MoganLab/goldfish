(import (liii check)
        (scheme file)
        (liii string)
        (liii path)
        (liii pi tools)
        (liii pi tools record)
        (liii pi blueprint types)
        (liii pi blueprint graph)
        (liii pi blueprint transition)
        (liii pi blueprint render)
        (liii pi blueprint runtime)
        (liii pi blueprint store)
        (liii pi blueprint tools)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "desc" '() '("a.scm") deps)
) ;define

(define bp
  (pi-blueprint-create "bp-1" "obj" "proj"
    (list (pi-blueprint-phase "1" "Phase 1" ""
            (list (task "1.1" '()) (task "1.2" '("1.1")))
            '()
          ) ;pi-blueprint-phase
      (pi-blueprint-phase "2" "Phase 2" ""
        (list (task "2.1" '("1.2")) (task "2.2" '("2.1")))
        '()
      ) ;pi-blueprint-phase
    ) ;list
  ) ;pi-blueprint-create
) ;define

(define dir (path-join (path-temp-dir) "gf-bp-tools-test"))
(define state (path->string (path-join dir "blueprint.json")))

(define (cleanup!)
  (for-each
    (lambda (f) (when (path-exists? (path f)) (delete-file f)))
    (list state (pi-blueprint-plan-path state) (pi-blueprint-history-path state))
  ) ;for-each
  (when (path-exists? dir) (path-rmdir dir))
) ;define

(define (result-text res)
  (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    ;; 三个内置工具注册到工具表。
    (check-true (pi-tool-definition? (get-tool-definition "blueprint_update")))
    (check-true (pi-tool-definition? (get-tool-definition "blueprint_status")))
    (check-true (pi-tool-definition? (get-tool-definition "blueprint_next")))

    ;; 无活动蓝图时的提示。
    (pi-blueprint-clear!)
    (check (result-text (pi-execute-blueprint-status '())) => "No active blueprint.")
    (check (result-text (pi-execute-blueprint-next '())) => "No active blueprint.")
    (check-true (pi-tool-result-is-error (pi-execute-blueprint-update '())))

    ;; 激活蓝图（状态文件落盘）。
    (pi-blueprint-set-current-path! state)
    (pi-blueprint-set-current! bp)
    (pi-blueprint-save-current!)

    ;; pi-execute-blueprint-status
    (let ((text (result-text (pi-execute-blueprint-status '()))))
      (check-true (string-contains? text "Progress: 0/4 tasks (0%)"))
      (check-true (string-contains? text "# Blueprint: obj"))
    ) ;let

    ;; pi-execute-blueprint-next
    (let ((text (result-text (pi-execute-blueprint-next '()))))
      (check-true (string-contains? text "## Next Task: 1.1 - Task 1.1"))
      (check-true (string-contains? text "**File targets:**"))
    ) ;let

    ;; pi-execute-blueprint-update：完成 1.1 → 解阻 1.2 并落盘。
    (let ((res (pi-execute-blueprint-update
                 (list (cons "task_id" "1.1") (cons "status" "completed"))
               ) ;pi-execute-blueprint-update
          ) ;
         ) ;
      (check (pi-tool-result-is-error res) => #f)
      (check-true (string-contains? (result-text res) "Task 1.1 updated to completed."))
      (check-true (string-contains? (result-text res) "Next task: 1.2"))
      (check-true (path-exists? (path state)))
      (let* ((loaded (pi-blueprint-load-state state))
             (t1 (pi-blueprint-find-task (pi-blueprint-all-tasks loaded) "1.1"))
             (t2 (pi-blueprint-find-task (pi-blueprint-all-tasks loaded) "1.2"))
            ) ;
        (check (pi-task-status t1) => "completed")
        (check (pi-task-status t2) => "pending")
      ) ;let*
    ) ;let

    ;; in_progress / skipped 与 notes。
    (let ((res (pi-execute-blueprint-update
                 (list (cons "task_id" "1.2")
                   (cons "status" "in_progress")
                   (cons "notes" "started")
                 ) ;list
               ) ;pi-execute-blueprint-update
          ) ;
         ) ;
      (check (pi-tool-result-is-error res) => #f)
      (check (pi-task-status (pi-blueprint-find-task (pi-blueprint-all-tasks
                                                       (pi-blueprint-current)
                                                     ) ;pi-blueprint-all-tasks
                         "1.2"
                       ) ;pi-blueprint-find-task
             ) ;pi-task-status
        =>
        "in_progress"
      ) ;check
      (check (pi-task-notes (pi-blueprint-find-task (pi-blueprint-all-tasks
                                                      (pi-blueprint-current)
                                                    ) ;pi-blueprint-all-tasks
                        "1.2"
                      ) ;pi-blueprint-find-task
             ) ;pi-task-notes
        =>
        "started"
      ) ;check
    ) ;let
    (let ((res (pi-execute-blueprint-update
                 (list (cons "task_id" "1.2") (cons "status" "skipped"))
               ) ;pi-execute-blueprint-update
          ) ;
         ) ;
      (check (pi-tool-result-is-error res) => #f)
    ) ;let

    ;; 非法参数。
    (check-true (pi-tool-result-is-error
                  (pi-execute-blueprint-update (list (cons "status" "completed")))
                ) ;pi-tool-result-is-error
    ) ;check-true
    (check-true (pi-tool-result-is-error
                  (pi-execute-blueprint-update
                    (list (cons "task_id" "1.1") (cons "status" "bogus"))
                  ) ;pi-execute-blueprint-update
                ) ;pi-tool-result-is-error
    ) ;check-true
    (check-true (pi-tool-result-is-error
                  (pi-execute-blueprint-update
                    (list (cons "task_id" "9.9") (cons "status" "completed"))
                  ) ;pi-execute-blueprint-update
                ) ;pi-tool-result-is-error
    ) ;check-true
  ) ;lambda
  (lambda () (pi-blueprint-clear!) (cleanup!))
) ;dynamic-wind

(check-report)

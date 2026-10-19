(import (liii check)
        (liii list)
        (scheme file)
        (liii string)
        (liii path)
        (liii pi blueprint types)
        (liii pi blueprint graph)
        (liii pi blueprint transition)
        (liii pi blueprint state)
        (liii pi blueprint store)
) ;import

(check-set-mode! 'report-failed)

(define (task id deps)
  (pi-blueprint-task id (string-append "Task " id) "desc" '() '() deps)
) ;define

(define bp
  (pi-blueprint-create "bp-1" "obj" "proj"
    (list (pi-blueprint-phase "1" "Phase 1" ""
            (list (task "1.1" '()) (task "1.2" '("1.1")))
            (list (pi-blueprint-gate "custom_command" "g1" "true")
              (pi-blueprint-gate "user_approval" "g2" #f)
            ) ;list
          ) ;pi-blueprint-phase
      (pi-blueprint-phase "2" "Phase 2" ""
        (list (task "2.1" '("1.2")) (task "2.2" '("2.1")))
        '()
      ) ;pi-blueprint-phase
    ) ;list
  ) ;pi-blueprint-create
) ;define

(define dir (path-join (path-temp-dir) "gf-bp-load-test"))
(define state (path->string (path-join dir "blueprint.json")))

(define (cleanup!)
  (for-each
    (lambda (f) (when (path-exists? (path f)) (delete-file f)))
    (list state (pi-blueprint-plan-path state) (pi-blueprint-history-path state))
  ) ;for-each
  (when (path-exists? dir) (path-rmdir dir))
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    ;; 缺失文件返回 #f。
    (check (pi-blueprint-load-state state) => #f)
    ;; 保存后可回读，结构等价。
    (pi-blueprint-save-state! state bp)
    (let ((loaded (pi-blueprint-load-state state)))
      (check (pi-blueprint-id loaded) => "bp-1")
      (check (pi-blueprint-objective loaded) => "obj")
      (check (length (pi-blueprint-phases loaded)) => 2)
      (check (pi-blueprint-active-phase-id loaded) => "1")
      (check (pi-blueprint-active-task-id loaded) => "1.1")
      (check (pi-task-status (pi-blueprint-find-task (pi-blueprint-all-tasks loaded) "1.2"))
        =>
        "blocked"
      ) ;check
    ) ;let
    ;; JSON 布尔值 false/true 回读为 Scheme 布尔值（而非真值符号）。
    (let* ((bp1 (pi-blueprint-set-gate bp "1" 0 #t #f))
           (bp2 (pi-blueprint-set-gate bp1 "1" 1 #f #f))
          ) ;
      (pi-blueprint-save-state! state bp2)
      (let ((loaded (pi-blueprint-load-state state)))
        (check (pi-gate-passed?
                 (list-ref (pi-phase-gates (pi-blueprint-find-phase loaded "1")) 0)
               ) ;pi-gate-passed?
          =>
          #t
        ) ;check
        (check (pi-gate-passed?
                 (list-ref (pi-phase-gates (pi-blueprint-find-phase loaded "1")) 1)
               ) ;pi-gate-passed?
          =>
          #f
        ) ;check
      ) ;let
    ) ;let*

    ;; 审计流水逐行追加。
    (pi-blueprint-append-history! state
      (pi-blueprint-history-entry "blueprint_created" #f #f "obj")
    ) ;pi-blueprint-append-history!
    (pi-blueprint-append-history! state
      (pi-blueprint-history-entry "task_completed" "1" "1.1" "done")
    ) ;pi-blueprint-append-history!
    (let* ((text (path-read-text (path (pi-blueprint-history-path state))))
           (lines (filter (lambda (s) (not (string-null? s)))
                    (string-split text "\n")
                  ) ;filter
           ) ;lines
          ) ;
      (check (length lines) => 2)
      (check-true (string-contains? (car lines) "\"blueprint_created\""))
      (check-true (string-contains? (cadr lines) "\"1.1\""))
    ) ;let*
  ) ;lambda
  (lambda () (cleanup!))
) ;dynamic-wind

(check-report)

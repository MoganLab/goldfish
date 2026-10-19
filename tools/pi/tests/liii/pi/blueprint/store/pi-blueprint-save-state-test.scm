(import (liii check)
        (scheme file)
        (liii string)
        (liii path)
        (liii json)
        (liii pi blueprint types)
        (liii pi blueprint transition)
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
            (list (pi-blueprint-gate "custom_command" "gate" "true"))
          ) ;pi-blueprint-phase
      (pi-blueprint-phase "2" "Phase 2" ""
        (list (task "2.1" '("1.2")) (task "2.2" '("2.1")))
        '()
      ) ;pi-blueprint-phase
    ) ;list
  ) ;pi-blueprint-create
) ;define

(define dir (path-join (path-temp-dir) "gf-bp-save-test"))
(define state (path->string (path-join dir "blueprint.json")))

(define (cleanup!)
  (for-each
    (lambda (f) (when (path-exists? (path f)) (delete-file f)))
    (list state (pi-blueprint-plan-path state) (pi-blueprint-history-path state))
  ) ;for-each
  (when (path-exists? dir) (path-rmdir dir))
) ;define

(dynamic-wind (lambda () (when (path-exists? dir) (cleanup!)))
  (lambda ()
    ;; pi-blueprint-save-state!
    (pi-blueprint-save-state! state bp)
    (check-true (path-exists? (path state)))
    (check-true (path-exists? (path (pi-blueprint-plan-path state))))
    (check-true (string-ends? (pi-blueprint-plan-path state) "blueprint.md"))
    (check-true (string-ends? (pi-blueprint-history-path state)
                  "blueprint.history.jsonl"
                ) ;string-ends?
    ) ;check-true
    ;; JSON 文件是 string-key 对象，可直接解析。
    (let* ((text (path-read-text (path state)))
           (parsed (string->json text))
          ) ;
      (check (cdr (assoc "id" parsed)) => "bp-1")
      (check (vector-length (cdr (assoc "phases" parsed))) => 2)
    ) ;let*
    ;; 人读计划被重新渲染。
    (check-true (string-contains? (path-read-text (path (pi-blueprint-plan-path state)))
                  "# Blueprint: obj"
                ) ;string-contains?
    ) ;check-true
  ) ;lambda
  (lambda () (cleanup!))
) ;dynamic-wind

(check-report)

;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi blueprint store)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii path)
    (liii json)
    (liii pi time)
    (liii pi blueprint types)
    (liii pi blueprint render)
  ) ;import
  (export pi-blueprint-default-state-path pi-blueprint-resolve-path
    pi-blueprint-plan-path pi-blueprint-history-path pi-blueprint-load-state
    pi-blueprint-save-state! pi-blueprint-append-history!
    pi-blueprint-history-entry
  ) ;export
  (begin

    (define (pi-blueprint-default-state-path cwd)
      (path->string (path-join (path cwd) ".pi" "blueprint.json"))
    ) ;define

    (define (pi-blueprint-resolve-path raw cwd)
      (if (path-absolute? (path raw))
        (path->string (path raw))
        (path->string (path-join (path cwd) raw))
      ) ;if
    ) ;define

    (define (%strip-json-suffix state-path)
      (if (string-ends? state-path ".json")
        (substring state-path 0 (- (string-length state-path) 5))
        state-path
      ) ;if
    ) ;define

    (define (pi-blueprint-plan-path state-path)
      (string-append (%strip-json-suffix state-path) ".md")
    ) ;define

    (define (pi-blueprint-history-path state-path)
      (string-append (%strip-json-suffix state-path) ".history.jsonl")
    ) ;define

    (define (%ensure-parent! file-path)
      (let ((parent (path-parent (path file-path))))
        (unless (path-exists? parent)
          (path-mkdir parent :parents #t :exist-ok #t)
        ) ;unless
      ) ;let
    ) ;define

    (define (pi-blueprint-load-state state-path)
      (if (not (path-exists? (path state-path)))
        #f
        (let ((parsed
                (catch #t
                  (lambda () (string->json (path-read-text (path state-path))))
                  (lambda _ #f)
                ) ;catch
              ) ;parsed
             ) ;
          (if (pair? parsed) (pi-blueprint-json-normalize parsed) #f)
        ) ;let
      ) ;if
    ) ;define

    (define (pi-blueprint-save-state! state-path bp)
      (%ensure-parent! state-path)
      (path-write-text (path state-path) (json->string bp))
      (path-write-text (path (pi-blueprint-plan-path state-path))
        (pi-render-blueprint-plan bp)
      ) ;path-write-text
      state-path
    ) ;define

    (define (pi-blueprint-history-entry event phase-id task-id details)
      (list (cons "timestamp" (pi-iso-8601-utc-now))
        (cons "event" event)
        (cons "phase_id" (if phase-id phase-id 'null))
        (cons "task_id" (if task-id task-id 'null))
        (cons "session_id" "")
        (cons "details" (if details details ""))
      ) ;list
    ) ;define

    (define (pi-blueprint-append-history! state-path entry)
      (let ((file (pi-blueprint-history-path state-path)))
        (%ensure-parent! file)
        (path-append-text (path file) (string-append (json->string entry) "\n"))
        file
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(import (scheme base)
  (liii string)
  (liii pi extension)
  (liii pi red-green state)
  (liii pi red-green report)
  (liii pi red-green tools)
  (liii pi red-green gate)
  (liii pi red-green observe)
  (liii pi red-green prompt)
  (liii pi modes red-green)
) ;import

(define (%ref parsed key)
  (let ((p (assoc key parsed)))
    (if p (cdr p) #f)
  ) ;let
) ;define

(define (red-green-extension api . opt-context)
  ;; 1. 注册 flags
  (register-flag! api
    (make-extension-flag-def "--red-green" "TDD mode" :key 'red-green)
  ) ;register-flag!

  ;; 2. 注册 commands：/tdd 与 /tdd-status
  (register-command! api
    (make-extension-command-def "/tdd"
      "Activate or inspect TDD workflow (/tdd <task>, /tdd off, /tdd)"
      (lambda (args ctx)
        (let ((arg-str (if (string? args) (string-trim-both args) (if (pair? args) (car args) ""))
              ) ;arg-str
             ) ;
          (cond ((string=? arg-str "off") (pi-tdd-reset!) "TDD deactivated")
                ((not (string-null? arg-str))
                 (pi-tdd-start! arg-str)
                 (string-append "TDD activated (RED phase): " arg-str)
                ) ;
                (else
                  (let ((st (pi-tdd-current-state)))
                    (if st (pi-format-tdd-summary st) "TDD is currently inactive")
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;lambda
      :params '()
    ) ;make-extension-command-def
  ) ;register-command!

  (register-command! api
    (make-extension-command-def "tdd"
      "Activate or inspect TDD workflow (tdd <task>, tdd off, tdd)"
      (lambda (args ctx)
        (let ((arg-str (if (string? args) (string-trim-both args) (if (pair? args) (car args) ""))
              ) ;arg-str
             ) ;
          (cond ((string=? arg-str "off") (pi-tdd-reset!) "TDD deactivated")
                ((not (string-null? arg-str))
                 (pi-tdd-start! arg-str)
                 (string-append "TDD activated (RED phase): " arg-str)
                ) ;
                (else
                  (let ((st (pi-tdd-current-state)))
                    (if st (pi-format-tdd-summary st) "TDD is currently inactive")
                  ) ;let
                ) ;else
          ) ;cond
        ) ;let
      ) ;lambda
      :params '()
    ) ;make-extension-command-def
  ) ;register-command!

  (register-command! api
    (make-extension-command-def "/tdd-status"
      "Show current TDD status and phase (/tdd-status)"
      (lambda (args ctx)
        (let ((st (pi-tdd-current-state)))
          (if st (pi-format-tdd-summary st) "TDD is currently inactive")
        ) ;let
      ) ;lambda
      :params '()
    ) ;make-extension-command-def
  ) ;register-command!

  (register-command! api
    (make-extension-command-def "tdd-status"
      "Show current TDD status and phase (tdd-status)"
      (lambda (args ctx)
        (let ((st (pi-tdd-current-state)))
          (if st (pi-format-tdd-summary st) "TDD is currently inactive")
        ) ;let
      ) ;lambda
      :params '()
    ) ;make-extension-command-def
  ) ;register-command!

  ;; 3. 注册 tools
  (register-tool! api (pi-tdd-status-definition))
  (register-tool! api (pi-tdd-advance-definition))
  (register-tool! api (pi-tdd-reset-definition))

  ;; 4. 注册 validator
  (register-validator! api
    (lambda (parsed . extra)
      (let ((red-green (%ref parsed 'red-green))
            (code-review (%ref parsed 'code-review))
            (simplify (%ref parsed 'simplify))
            (apply? (%ref parsed 'apply))
            (staged (%ref parsed 'staged))
            (ref (%ref parsed 'review-ref))
            (msg (and (pair? extra) (car extra)))
            (rest (and (pair? extra) (pair? (cdr extra)) (cadr extra)))
           ) ;
        (when (and red-green (or code-review simplify))
          (error 'pi-arg-error
            "--red-green cannot be used with --code-review or --simplify"
          ) ;error
        ) ;when
        (when (and red-green (or apply? staged ref))
          (error 'pi-arg-error
            "--apply, --staged and --ref are not supported with --red-green"
          ) ;error
        ) ;when
        (when (and red-green (string? msg) (string-null? msg) (or (not rest) (null? rest)))
          (error 'pi-arg-error
            "--red-green requires a task description (message text or @file)"
          ) ;error
        ) ;when
      ) ;let
    ) ;lambda
  ) ;register-validator!

  ;; 5. 注册 flow
  (register-flow! api
    (make-extension-flow-def 'red-green
      (lambda (p) (%ref p 'red-green))
      (lambda (parsed cfg mode initial-msg)
        (run-red-green-flow parsed cfg mode initial-msg)
      ) ;lambda
      :local?   #f
      :priority 10
    ) ;make-extension-flow-def
  ) ;register-flow!

  ;; 6. 注册 prompt injector
  (register-prompt-injector! api
    (lambda (tool-calls)
      (let ((state (pi-tdd-current-state)))
        (if state
          (let ((block (pi-build-red-green-prompt state)))
            (if block (string-trim-both block) #f)
          ) ;let
          #f
        ) ;if
      ) ;let
    ) ;lambda
  ) ;register-prompt-injector!

  ;; 7. 注册 hooks
  (pi-on! api
    'tool_call
    (lambda (payload)
      (if (pi-tdd-current-state)
        (let* ((name (cdr (or (assq 'name payload) '(#f . ""))))
               (args (cdr (or (assq 'input payload) '(#f #_quote ()))))
               (gate-msg (pi-tdd-write-gate name args))
              ) ;
          (if gate-msg (list 'block gate-msg) 'continue)
        ) ;let*
        'continue
      ) ;if
    ) ;lambda
  ) ;pi-on!

  (pi-on! api
    'tool_result
    (lambda (payload)
      (when (pi-tdd-current-state)
        (let ((name (cdr (or (assq 'name payload) '(#f . ""))))
              (args (cdr (or (assq 'input payload) '(#f #_quote ()))))
              (text (cdr (or (assq 'result payload) '(#f . ""))))
              (is-err? (cdr (or (assq 'isError payload) '(#f . #f))))
             ) ;
          (pi-tdd-observe-tool! name args text is-err?)
        ) ;let
      ) ;when
    ) ;lambda
  ) ;pi-on!

  (pi-on! api
    'turn_start
    (lambda (payload) (when (pi-tdd-current-state) (pi-tdd-bump-turn!)))
  ) ;pi-on!

  "red-green-loaded"
) ;define

red-green-extension

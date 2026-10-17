(import (liii check)
  (liii string)
  (liii path)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

;; Anthropic stop reason mapping
(check (anthropic-map-stop-reason "end_turn") => "stop")
(check (anthropic-map-stop-reason "max_tokens") => "length")
(check (anthropic-map-stop-reason "tool_use") => "toolUse")
(check (anthropic-map-stop-reason "refusal") => "error")
(check (anthropic-map-stop-reason "pause_turn") => "stop")
(check (anthropic-map-stop-reason "stop_sequence") => "stop")
(check (anthropic-map-stop-reason "sensitive") => "error")
(check (anthropic-map-stop-reason "unknown") => "error")

;; Anthropic usage mapping
(let* ((raw-usage '(("input_tokens" . 100)
                    ("output_tokens" . 45)
                    ("cache_read_input_tokens" . 20)
                    ("cache_creation_input_tokens" . 10)
                   ) ;raw-usage
       ) ;raw-usage
       (mapped (anthropic-map-usage raw-usage))
      ) ;
  (check (cdr (assoc 'input mapped)) => 100)
  (check (cdr (assoc 'output mapped)) => 45)
  (check (cdr (assoc 'cacheRead mapped)) => 20)
  (check (cdr (assoc 'cacheWrite mapped)) => 10)
  (check (cdr (assoc 'totalTokens mapped)) => 145)
) ;let*

;; Anthropic tool declarations
(let* ((tool-defs (list (list (cons 'name "bash")
                          (cons 'description "Run bash")
                          (cons 'parameters '(("type" . "object")))
                        ) ;list
                  ) ;list
       ) ;tool-defs
       (decls (anthropic-build-tool-declarations tool-defs))
      ) ;
  (check (length decls) => 1)
  (let ((decl (car decls)))
    (check (cdr (assoc "name" decl)) => "bash")
    (check (cdr (assoc "description" decl)) => "Run bash")
    (check (cdr (assoc "input_schema" decl)) => '(("type" . "object")))
  ) ;let
) ;let*

;; Anthropic messages & tool results
(let* ((msgs (list (list (cons 'role "user") (cons 'content "hello"))
               (list (cons 'role "assistant")
                 (cons 'content
                   (list (list (cons 'type "toolCall")
                           (cons 'id "call_1")
                           (cons 'name "bash")
                           (cons 'arguments '(("command" . "ls")))
                         ) ;list
                   ) ;list
                 ) ;cons
               ) ;list
               (list (cons 'role "toolResult")
                 (cons 'toolCallId "call_1")
                 (cons 'toolName "bash")
                 (cons 'content (list (list (cons 'type "text") (cons 'text "file.txt"))))
                 (cons 'isError #f)
               ) ;list
             ) ;list
       ) ;msgs
       (built (anthropic-build-messages msgs))
      ) ;
  (check (vector? built) => #t)
  (check (vector-length built) => 3)
  ;; Check turn 1: user
  (let ((u1 (vector-ref built 0)))
    (check (cdr (assoc "role" u1)) => "user")
    (let ((c (vector-ref (cdr (assoc "content" u1)) 0)))
      (check (cdr (assoc "type" c)) => "text")
      (check (cdr (assoc "text" c)) => "hello")
    ) ;let
  ) ;let
  ;; Check turn 2: assistant with tool_use
  (let ((a1 (vector-ref built 1)))
    (check (cdr (assoc "role" a1)) => "assistant")
    (let ((c (vector-ref (cdr (assoc "content" a1)) 0)))
      (check (cdr (assoc "type" c)) => "tool_use")
      (check (cdr (assoc "id" c)) => "call_1")
      (check (cdr (assoc "name" c)) => "bash")
    ) ;let
  ) ;let
  ;; Check turn 3: toolResult wrapped in user turn
  (let ((u2 (vector-ref built 2)))
    (check (cdr (assoc "role" u2)) => "user")
    (let ((c (vector-ref (cdr (assoc "content" u2)) 0)))
      (check (cdr (assoc "type" c)) => "tool_result")
      (check (cdr (assoc "tool_use_id" c)) => "call_1")
      (check (cdr (assoc "content" c)) => "file.txt")
      (check (cdr (assoc "is_error" c)) => #f)
    ) ;let
  ) ;let
) ;let*

;; Anthropic build request body
(let* ((msgs #((("role" . "user") ("content" . #((("type" . "text") ("text" . "hi")))))))
       (body
         (anthropic-build-request-body "claude-3-7-sonnet-20250219" msgs "You are pi" '() 4096)
       ) ;body
      ) ;
  (check (cdr (assoc "model" body)) => "claude-3-7-sonnet-20250219")
  (check (cdr (assoc "max_tokens" body)) => 4096)
  (check (cdr (assoc "system" body)) => "You are pi")
  (check (cdr (assoc "messages" body)) => msgs)
) ;let*

;; Anthropic parse response
(let* ((resp '((("type" . "message")
                ("role" . "assistant")
                ("content" . #((("type" . "text") ("text" . "Hello from Claude"))))
                ("stop_reason" . "end_turn")
                ("usage" . (("input_tokens" . 12) ("output_tokens" . 6)))
               ) ;
              ) ;list
       ) ;resp
       (parsed (anthropic-parse-response (car resp) 200))
      ) ;
  (check (cdr (assoc 'role parsed)) => "assistant")
  (check (cdr (assoc 'stopReason parsed)) => "stop")
  (let* ((content (cdr (assoc 'content parsed)))
         (first-blk (car content))
        ) ;
    (check (cdr (assoc 'type first-blk)) => "text")
    (check (cdr (assoc 'text first-blk)) => "Hello from Claude")
  ) ;let*
) ;let*

;; Anthropic retry error detection
(check (anthropic-is-retriable-error? 200 "{}") => #f)
(check (anthropic-is-retriable-error? 400 "{}") => #f)
(check (anthropic-is-retriable-error? 408 "{}") => #t)
(check (anthropic-is-retriable-error? 409 "{}") => #t)
(check (anthropic-is-retriable-error? 429 "{}") => #t)
(check (anthropic-is-retriable-error? 500 "{}") => #t)
(check (anthropic-is-retriable-error? 529 "{}") => #t)
(check (anthropic-is-retriable-error? 0 "") => #t)

(check-report)

;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core prompt)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii path)
    (liii os)
    (liii list)
    (liii pi code core tools read)
    (liii pi code core prompt context)
  ) ;import
  (export pi-resolve-prompt-input pi-load-project-context pi-build-system-prompt
    PI_BUILTIN_TOOL_SNIPPETS PI_BUILTIN_TOOL_GUIDELINES build-initial-prompt
  ) ;export
  (begin

    (define PI_BUILTIN_TOOL_SNIPPETS
      '(("read" . "Read file contents")
        ("bash" . "Execute bash commands (ls, grep, find, etc.)")
        ("write" . "Create or overwrite files")
        ("edit"
         . "Make precise file edits with exact text replacement, including multiple disjoint edits in one call")
        ("ls" . "List directory contents")
        ("find" . "Find files by glob pattern (respects .gitignore)")
        ("grep" . "Search file contents for patterns (respects .gitignore)"))
    ) ;define

    (define PI_BUILTIN_TOOL_GUIDELINES
      '(("read" "Use read to examine files instead of cat or sed.")
        ("bash"
         "You can inspect PI_* environment variables for current model and session details.")
        ("write" "Use write only for new files or complete rewrites.")
        ("edit"
         "Use edit for precise changes (edits[].oldText must match exactly)"
         "When changing multiple separate locations in one file, use one edit call with multiple entries in edits[] instead of multiple edit calls"
         "Each edits[].oldText is matched against the original file, not after earlier edits are applied. Do not emit overlapping or nested edits. Merge nearby changes into one edit."
         "Keep edits[].oldText as small as possible while still being unique in the file. Do not pad with large unchanged regions.")
        ("ls")
        ("find")
        ("grep"))
    ) ;define

    (define (%find-snippet name custom-info)
      (let ((custom (assoc name custom-info)))
        (cond
         ((and custom (pair? (cdr custom)) (assoc 'snippet (cdr custom)))
          (cdr (assoc 'snippet (cdr custom)))
         ) ;
         ((and custom (string? (cdr custom))) (cdr custom))
         ((assoc name PI_BUILTIN_TOOL_SNIPPETS) => cdr)
         (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%find-guidelines name custom-info)
      (let ((custom (assoc name custom-info)))
        (cond
         ((and custom (pair? (cdr custom)) (assoc 'guidelines (cdr custom)))
          (let ((g (cdr (assoc 'guidelines (cdr custom)))))
            (if (list? g) g (list (object->string g)))
          ) ;let
         ) ;
         ((and custom (list? (cdr custom))) (cdr custom))
         ((assoc name PI_BUILTIN_TOOL_GUIDELINES) => cdr)
         (else '())
        ) ;cond
      ) ;let
    ) ;define

    (define (%build-tools-section tools custom-info)
      (let ((lines
              (filter (lambda (l) l)
                (map
                  (lambda (name)
                    (let ((snippet (%find-snippet name custom-info)))
                      (and (string? snippet)
                        (> (string-length snippet) 0)
                        (string-append "- " name ": " snippet)
                      ) ;and
                    ) ;let
                  ) ;lambda
                  tools
                ) ;map
              ) ;filter
            ) ;lines
           ) ;
        (if (null? lines)
          ""
          (string-append (string-join lines "\n")
            "\n\nIn addition to the tools above, you may have access to other custom tools depending on the project."
          ) ;string-append
        ) ;if
      ) ;let
    ) ;define

    (define (%collect-tool-guidelines tools custom-info)
      (append-map (lambda (name) (%find-guidelines name custom-info)) tools)
    ) ;define

    (define (%build-rules-section tools custom-info)
      (let* ((has-bash (member "bash" tools))
             (has-grep (member "grep" tools))
             (has-find (member "find" tools))
             (has-ls (member "ls" tools))
             (bash-rule (if (and has-bash (not has-grep) (not has-find) (not has-ls))
                          (list "Use bash for file operations like ls, rg, find")
                          '()
                        ) ;if
             ) ;bash-rule
             (tool-gl (%collect-tool-guidelines tools custom-info))
             (fixed-rules '("Be concise in your responses"
                            "Show file paths clearly when working with files")
             ) ;fixed-rules
             (all-rules (append bash-rule tool-gl fixed-rules))
             (unique-rules
               (reverse
                 (fold (lambda (rule acc) (if (member rule acc) acc (cons rule acc)))
                   '()
                   all-rules
                 ) ;fold
               ) ;reverse
             ) ;unique-rules
            ) ;
        (string-join (map (lambda (x) (string-append "- " x)) unique-rules) "\n")
      ) ;let*
    ) ;define

    (define (%format-session-budget session-timeout max-turns)
      (let* ((has-st? (and (number? session-timeout) (> session-timeout 0)))
             (st-line (if has-st?
                        (string-append "- Session Timeout: "
                          (number->string session-timeout)
                          " seconds (hard wall-clock deadline). When deadline is reached, the process terminates immediately."
                        ) ;string-append
                        "- Session Timeout: None (no wall-clock deadline configured)."
                      ) ;if
             ) ;st-line
             (has-mt? (and (number? max-turns) (> max-turns 0)))
             (mt-line (if has-mt?
                        (string-append "- Max Turns: "
                          (number->string max-turns)
                          " turns. When max turns limit is reached, execution stops."
                        ) ;string-append
                        "- Max Turns: Unlimited (no turn limit configured)."
                      ) ;if
             ) ;mt-line
             (pacing-line "- Pacing: Pace your work accordingly. If budget is running low, prioritize finalizing answers and providing summary/conclusions instead of starting new tool operations."
             ) ;pacing-line
            ) ;
        (string-append "<session_budget>\n" st-line "\n" mt-line "\n"
          pacing-line "\n</session_budget>"
        ) ;string-append
      ) ;let*
    ) ;define

    (define* (pi-build-system-prompt (tools '("read" "bash" "edit" "write"))
               (cwd ".")
               (project-context #f)
               (system-prompt-override #f)
               (append-system-prompts '())
               (custom-tool-info '())
               (session-timeout 600)
               (max-turns 200)
             ) ;pi-build-system-prompt
      (let* ((has-override?
               (and (string? system-prompt-override)
                 (not (string-null? (string-trim-both system-prompt-override)))
               ) ;and
             ) ;has-override?
             (preamble (if has-override?
                         system-prompt-override
                         "You are an expert coding assistant operating inside pi, a coding agent harness. You help users by reading files, executing commands, editing code, and writing new files."
                       ) ;if
             ) ;preamble
             (tools-sec
               (if has-override?
                 #f
                 (let ((t-str (%build-tools-section tools custom-tool-info)))
                   (if (string-null? t-str) #f (string-append "<tools>\n" t-str "\n</tools>"))
                 ) ;let
               ) ;if
             ) ;tools-sec
             (rules-sec
               (if has-override?
                 #f
                 (let ((r-str (%build-rules-section tools custom-tool-info)))
                   (if (string-null? r-str) #f (string-append "<rules>\n" r-str "\n</rules>"))
                 ) ;let
               ) ;if
             ) ;rules-sec
             (addendum-sec
               (if (and (pair? append-system-prompts) (> (length append-system-prompts) 0))
                 (let ((add-str (string-join append-system-prompts "\n\n")))
                   (if (string-null? (string-trim-both add-str))
                     #f
                     (string-append "<addendum>\n" add-str "\n</addendum>")
                   ) ;if
                 ) ;let
                 #f
               ) ;if
             ) ;addendum-sec
             (project-sec
               (if (pair? project-context)
                 (let ((p (cdr (assoc 'path project-context)))
                       (c (cdr (assoc 'content project-context)))
                      ) ;
                   (string-append "<project_context>\nProject-specific instructions and guidelines:\n\n<project_instructions path=\""
                     p "\">\n" c "\n</project_instructions>\n</project_context>"
                   ) ;string-append
                 ) ;let
                 #f
               ) ;if
             ) ;project-sec
             (cwd-sec
               (if (and (string? cwd) (not (string-null? cwd)))
                 (string-append "<cwd>\n" cwd "\n</cwd>")
                 #f
               ) ;if
             ) ;cwd-sec
             (budget-sec (%format-session-budget session-timeout max-turns))
             (sections
               (filter
                 (lambda (x) (and (string? x) (not (string-null? x))))
                 (list preamble tools-sec rules-sec addendum-sec project-sec cwd-sec budget-sec)
               ) ;filter
             ) ;sections
            ) ;
        (string-join sections "\n\n")
      ) ;let*
    ) ;define*

    (define (build-initial-prompt stdin-content file-text messages)
      (let* ((has-stdin? (and (string? stdin-content) (not (string-null? stdin-content))))
             (has-file? (and (string? file-text) (not (string-null? file-text))))
             (has-msg? (and (pair? messages) (string? (car messages))))
             (first-msg (if has-msg? (car messages) #f))
             (rest-msgs (if has-msg? (cdr messages) '()))
             (parts (append (if has-stdin? (list stdin-content) '())
                      (if has-file? (list file-text) '())
                      (if first-msg (list first-msg) '())
                    ) ;append
             ) ;parts
             (initial-message (if (null? parts) "" (apply string-append parts)))
            ) ;
        (values initial-message rest-msgs)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

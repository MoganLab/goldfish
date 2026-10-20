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

(define-library (liii pi prompt)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii path)
    (liii os)
    (liii list)
    (liii pi read)
  ) ;import
  (export pi-resolve-prompt-input pi-load-project-context pi-build-system-prompt
    PI_BUILTIN_TOOL_SNIPPETS PI_BUILTIN_TOOL_GUIDELINES
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
        ("grep" . "Search file contents for patterns (respects .gitignore)")
        ("tdd_status" . "Check the current TDD state and phase")
        ("tdd_advance"
         . "Manually advance the TDD phase when auto-detection doesn't trigger")
        ("tdd_reset" . "Reset the TDD session to idle state")
        ("codebase_map"
         . "Get or generate a structured map of the current codebase")
        ("code_tour" . "Get a guided code tour for a specific topic or area"))
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
        ("grep")
        ("tdd_status"
         "Use TDD tools to check, advance, or reset the current TDD session state.")
        ("tdd_advance"
         "Use TDD tools to check, advance, or reset the current TDD session state.")
        ("tdd_reset"
         "Use TDD tools to check, advance, or reset the current TDD session state.")
        ("codebase_map"
         "Use codebase_map to understand the overall project structure. Use code_tour to walk through specific areas in detail.")
        ("code_tour"
         "Use codebase_map to understand the overall project structure. Use code_tour to walk through specific areas in detail."))
    ) ;define

    (define (pi-resolve-prompt-input input)
      (if (and (string? input) (file-exists? input))
        (catch #t
          (lambda ()
            (let ((content (path-read-text (path input))))
              (if (string? content) content input)
            ) ;let
          ) ;lambda
          (lambda (k . args) input)
        ) ;catch
        input
      ) ;if
    ) ;define

    (define (%find-first-context-file dir candidates)
      (let loop-dir
        ((cur-dir dir))
        (let loop-cand
          ((cands candidates))
          (if (null? cands)
            (let* ((p (path cur-dir)) (parent (path-parent p)))
              (if (or (not parent) (equal? parent p) (string-null? (path->string parent)))
                #f
                (loop-dir (path->string parent))
              ) ;if
            ) ;let*
            (let* ((filename (car cands))
                   (filepath (path->string (path-join (path cur-dir) (path filename))))
                  ) ;
              (if (path-file? (path filepath))
                (let ((content
                        (catch #t (lambda () (path-read-text (path filepath))) (lambda (k . args) #f))
                      ) ;content
                     ) ;
                  (if (string? content)
                    (list (cons 'path filepath) (cons 'content content))
                    (loop-cand (cdr cands))
                  ) ;if
                ) ;let
                (loop-cand (cdr cands))
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (pi-load-project-context dir)
      (let ((candidates '("AGENTS.override.md" "AGENTS.md" "AGENTS.MD"
                          "CLAUDE.md" "CLAUDE.MD")
            ) ;candidates
           ) ;
        (%find-first-context-file dir candidates)
      ) ;let
    ) ;define

    (define (%build-tools-section tools custom-info)
      (let loop
        ((ts tools) (lines '()))
        (if (null? ts)
          (if (null? lines)
            ""
            (string-append (string-join (reverse lines) "\n")
              "\n\nIn addition to the tools above, you may have access to other custom tools depending on the project."
            ) ;string-append
          ) ;if
          (let* ((name (car ts))
                 (snippet-pair (or (assoc name custom-info) (assoc name PI_BUILTIN_TOOL_SNIPPETS))
                 ) ;snippet-pair
                 (snippet (if snippet-pair (cdr snippet-pair) #f))
                ) ;
            (if snippet
              (loop (cdr ts) (cons (string-append "- " name ": " snippet) lines))
              (loop (cdr ts) lines)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%collect-tool-guidelines tools custom-info)
      (let loop
        ((ts tools) (acc '()))
        (if (null? ts)
          (reverse acc)
          (let* ((name (car ts))
                 (g-pair (or (assoc name custom-info) (assoc name PI_BUILTIN_TOOL_GUIDELINES)))
                 (gl (if g-pair (cdr g-pair) '()))
                ) ;
            (loop (cdr ts) (append (reverse gl) acc))
          ) ;let*
        ) ;if
      ) ;let
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
               (let loop
                 ((r all-rules) (seen '()) (out '()))
                 (if (null? r)
                   (reverse out)
                   (let ((item (car r)))
                     (if (member item seen)
                       (loop (cdr r) seen out)
                       (loop (cdr r) (cons item seen) (cons item out))
                     ) ;if
                   ) ;let
                 ) ;if
               ) ;let
             ) ;unique-rules
            ) ;
        (string-join (map (lambda (x) (string-append "- " x)) unique-rules) "\n")
      ) ;let*
    ) ;define

    (define* (pi-build-system-prompt (tools '("read" "bash" "edit" "write"))
               (cwd ".")
               (project-context #f)
               (system-prompt-override #f)
               (append-system-prompts '())
               (custom-tool-info '())
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
             (sections
               (filter
                 (lambda (x) (and (string? x) (not (string-null? x))))
                 (list preamble tools-sec rules-sec addendum-sec project-sec cwd-sec)
               ) ;filter
             ) ;sections
            ) ;
        (string-join sections "\n\n")
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library

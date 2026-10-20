;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi simplify prompt)
  (import (scheme base) (liii string) (liii list) (liii pi simplify scope))
  (export pi-build-simplify-prompt pi-build-simplify-apply-prompt)
  (begin

    (define PI_SIMPLIFY_MAX_FILE_CHARS 4000)

    (define %principles
      "## Principles\n\n- **Preserve functionality**: Never change what the code does. All existing tests must continue to pass.\n- **Apply project standards**: Follow any conventions from CLAUDE.md or AGENTS.md in this project.\n- **Enhance clarity**: Reduce unnecessary complexity and nesting, eliminate redundant code and abstractions, improve variable and function names, and consolidate related logic. Keep valuable comments that explain design rationale, business rules, non-obvious behaviour, or intent. Remove only truly redundant noise. Avoid nested ternary operators.\n- **Maintain balance**: Do not over-simplify. Avoid overly clever solutions that are hard to understand. Do not combine too many concerns into single functions. Do not remove helpful abstractions. Prioritize readability over fewer lines.\n"
    ) ;define

    (define %prohibitions
      "Do NOT add new features, change public APIs, or suggest changes outside the listed line ranges. If a worthwhile simplification would require editing unchanged code, leave it alone and mention it in the summary instead."
    ) ;define

    (define (%field key entry default)
      (let ((pair (assoc key entry)))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%truncate content)
      (if (<= (string-length content) PI_SIMPLIFY_MAX_FILE_CHARS)
        content
        (string-append (substring content 0 PI_SIMPLIFY_MAX_FILE_CHARS) "\n[truncated]")
      ) ;if
    ) ;define

    (define (%lines content)
      (if (string-null? content) '() (string-split content "\n"))
    ) ;define

    (define (%range-start range)
      (%field 'start range 1)
    ) ;define

    (define (%range-end range)
      (%field 'end range 1)
    ) ;define

    (define (%clamp lo hi value)
      (max lo (min hi value))
    ) ;define

    (define (%context-ranges ranges line-count)
      (let loop
        ((rest ranges) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((start
                   (%clamp 1 line-count (- (%range-start (car rest)) 2))
                 ) ;start
                 (end
                   (%clamp 1 line-count (+ (%range-end (car rest)) 2))
                 ) ;end
                 (prev (if (pair? acc) (car acc) #f))
                ) ;
            (if
              (and prev (<= start (+ (%field 'end prev 0) 1)))
              (loop (cdr rest)
                (cons
                  (list (cons 'start (%field 'start prev start))
                    (cons 'end (max (%field 'end prev 0) end))
                  ) ;list
                  (cdr acc)
                ) ;cons
              ) ;loop
              (loop (cdr rest) (cons (list (cons 'start start) (cons 'end end)) acc))
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%range-text ranges)
      (string-join (map pi-format-range ranges) ", ")
    ) ;define

    (define (%excerpt lines ranges)
      (let loop
        ((rest ranges) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((start (%field 'start (car rest) 1))
                 (end (%field 'end (car rest) 1))
                 (body
                   (let inner
                     ((i start) (rows '()))
                     (if (> i end)
                       (string-join (reverse rows) "\n")
                       (inner (+ i 1)
                         (cons (string-append (number->string i) " | " (list-ref lines (- i 1))) rows)
                       ) ;inner
                     ) ;if
                   ) ;let
                 ) ;body
                ) ;
            (loop (cdr rest) (cons body acc))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (%format-file-block file)
      (let* ((path (%field 'path file ""))
             (status (%field 'status file "modified"))
             (ranges (%field 'ranges file #f))
             (available? (%field 'ranges-available? file #f))
             (content (%field 'content file ""))
             (lines (%lines content))
             (line-count (length lines))
             (header (string-append "### " path " (" status ")"))
            ) ;
        (cond
         ((and (not (string=? status "added")) (not available?))
          (string-append header
            "\nChanged lines unavailable; full file content follows.\n```\n"
            (%truncate content)
            "\n```"
          ) ;string-append
         ) ;
         ((string=? status "added")
          (string-append header
            "\nEntire file is in scope.\n```\n"
            (%truncate content)
            "\n```"
          ) ;string-append
         ) ;
         ((null? ranges)
          (string-append header "\nDeletions only; no current lines to simplify.\n")
         ) ;
         ((null? lines)
          (string-append header
            "\nChanged line excerpts unavailable (file content is empty or unreadable).\n"
          ) ;string-append
         ) ;
         (else
           (let* ((contexts (%context-ranges ranges line-count))
                  (body (%excerpt lines contexts))
                 ) ;
             (string-append header
               "\nChanged lines: "
               (%range-text ranges)
               "\n```\n"
               (%truncate (string-join body "\n...\n"))
               "\n```"
             ) ;string-append
           ) ;let*
         ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (%format-files files)
      (string-join (map %format-file-block files) "\n\n")
    ) ;define

    (define (%scope-section files context-note)
      (string-append "## Scope\n\nOnly review and propose changes within the changed lines listed below. Changed line numbers refer to the current file contents. "
        context-note
        " For added files, the entire file is considered changed.\n\n"
        (pi-build-scope-lines files)
        "\n"
      ) ;string-append
    ) ;define

    (define (pi-build-simplify-prompt files)
      (string-append "Review the following recently changed files and propose simplification improvements. All relevant code is provided below; do not call or request any tools. Return only the JSON suggestions object requested at the end.\n\n"
        %principles
        "\n"
        (%scope-section files
          "Surrounding context excerpts are supplied below; you have no tools and must not request any."
        ) ;%scope-section
        "\n## Process\n\n"
        "1. Read each changed file excerpt listed above.\n"
        "2. Identify concrete improvements within the changed lines (dead code, unclear names, redundant logic, inconsistent patterns).\n"
        "3. Do not modify any file; describe each opportunity precisely.\n"
        "4. Keep suggestions minimal, readable, and behavior-preserving.\n"
        "5. Summarize the opportunities as structured JSON.\n\n"
        "## Output Format\n\n"
        "Return JSON in this exact format:\n\n"
        "```json\n"
        "{\"suggestions\": [{\"file\": \"path\", \"line\": 42, \"category\": \"naming\", \"message\": \"description\", \"suggestion\": \"concrete change\"}]}\n"
        "```\n\n"
        "Use categories such as dead-code, naming, redundant-logic, clarity, or consistency. Include the current file line number in `line`. If no opportunities are found, return: `{\"suggestions\": []}`\n\n"
        %prohibitions
        "\n\n## Files\n\n"
        (%format-files files)
      ) ;string-append
    ) ;define

    (define (pi-build-simplify-apply-prompt files)
      (string-append "Review the following recently changed files and apply simplification improvements.\n\n"
        %principles
        "\n"
        (%scope-section files
          "You may read surrounding code for context, but must not edit outside the listed line ranges."
        ) ;%scope-section
        "\n## Process\n\n"
        "1. Read each file listed above and inspect its changed lines.\n"
        "2. Identify concrete improvements within those lines (dead code, unclear names, redundant logic, inconsistent patterns).\n"
        "3. Apply changes one file at a time, keeping every edit within the listed line ranges.\n"
        "4. Run existing tests to verify nothing is broken.\n"
        "5. Summarize what you changed and why.\n\n"
        "In the final summary, include an **Outside scope** section listing worthwhile simplifications that would require editing unchanged code.\n\n"
        %prohibitions
      ) ;string-append
    ) ;define

  ) ;begin
) ;define-library

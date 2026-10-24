;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code review prompt)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi code review files)
    (liii pi code review language)
  ) ;import
  (export pi-build-review-prompt pi-build-fallback-prompt)
  (begin

    (define (%field key entry default)
      (let ((pair (assoc key entry)))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%truncate content)
      (if (<= (string-length content) PI_REVIEW_MAX_FILE_CHARS)
        content
        (string-append (string-take content PI_REVIEW_MAX_FILE_CHARS) "\n[truncated]")
      ) ;if
    ) ;define

    (define (%unique items)
      (let loop
        ((rest items) (acc '()))
        (cond ((null? rest) (reverse acc))
              ((member (car rest) acc) (loop (cdr rest) acc))
              (else (loop (cdr rest) (cons (car rest) acc)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%languages files)
      (%unique
        (filter (lambda (l) l) (map (lambda (f) (%field 'language f #f)) files))
      ) ;%unique
    ) ;define

    (define (%format-file-block entry)
      (let* ((path (%field 'path entry ""))
             (language (%field 'language entry #f))
             (content (%truncate (or (%field 'content entry "") "")))
            ) ;
        (string-append "### "
          path
          (if language (string-append " (" language ")") "")
          "\n```\n"
          content
          "\n```"
        ) ;string-append
      ) ;let*
    ) ;define

    (define (%format-files files)
      (string-join (map %format-file-block files) "\n\n")
    ) ;define

    (define (%review-prompt-body lang-section file-blocks)
      (string-append "Review the following files for code quality issues. All file contents are provided below; do not call or request any tools. Return your findings as JSON in this exact format:\n\n"
        "```json\n"
        "{\"findings\": [{\"severity\": \"CRITICAL|HIGH|MEDIUM|INFO\", \"file\": \"path\", \"line\": 42, \"category\": \"category\", \"message\": \"description\", \"suggestion\": \"fix\"}]}\n"
        "```\n\n" "Severity guide:\n"
        "- CRITICAL: Security vulnerabilities, data loss risks, crash-inducing bugs\n"
        "- HIGH: Bugs, unhandled errors, type safety violations\n"
        "- MEDIUM: Anti-patterns, maintainability issues, naming problems\n"
        "- INFO: Style improvements, minor optimizations\n\n"
        "Focus areas by language:" lang-section "\n\n"
        "If no issues are found, return: `{\"findings\": []}`\n\n"
        "## Files to Review\n\n" file-blocks
      ) ;string-append
    ) ;define

    (define (pi-build-review-prompt files)
      (%review-prompt-body (pi-build-language-section (%languages files))
        (%format-files files)
      ) ;%review-prompt-body
    ) ;define

    (define (%format-file-list files)
      (string-join
        (map (lambda (f) (string-append "- " (%field 'path f ""))) files)
        "\n"
      ) ;string-join
    ) ;define

    (define (pi-build-fallback-prompt files)
      (string-append "Review the following recently changed files for code quality issues. Read each file, then report findings.\n\n"
        "## Format\n\n"
        "For each finding, state:\n"
        "- **Severity**: CRITICAL, HIGH, MEDIUM, or INFO\n"
        "- **File** and **line number**\n"
        "- **Category** (security, error-handling, type-safety, naming, etc.)\n"
        "- **Issue** and **suggestion**\n\n"
        "Focus areas:"
        (pi-build-language-section (%languages files))
        "\n\n"
        "## Files to Review\n\n"
        (%format-file-list files)
        "\n\n"
        "Read each file, identify issues, and report your findings. Do not modify any file."
      ) ;string-append
    ) ;define

  ) ;begin
) ;define-library

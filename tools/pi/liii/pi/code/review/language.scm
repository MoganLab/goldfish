;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code review language)
  (import (scheme base) (scheme char) (liii string) (liii list))
  (export pi-detect-language pi-language-checklist pi-build-language-section)
  (begin

    (define %language-map
      '((".ts" . "typescript")
        (".tsx" . "typescript")
        (".js" . "typescript")
        (".jsx" . "typescript")
        (".py" . "python")
        (".go" . "go")
        (".rs" . "rust")
        (".java" . "java")
        (".php" . "php")
        (".scm" . "scheme"))
    ) ;define

    (define %language-checklists
      '(("typescript"
         "Type safety: any usage, missing return types on exports, unsafe type assertions"
         "Error handling: unhandled promises, missing try-catch at boundaries, swallowed errors"
         "Null safety: missing optional chaining, unchecked array access, nullable values"
         "Naming: unclear variable/function names, inconsistent conventions"
         "Security: unsanitized input, injection vectors, hardcoded secrets")
        ("python"
         "Type hints: missing annotations on public functions, incorrect types"
         "Error handling: bare except, missing error context, swallowed exceptions"
         "Resource management: missing context managers, unclosed handles"
         "Naming: PEP 8 violations, unclear names"
         "Security: input validation, path traversal, injection risks")
        ("go"
         "Error handling: unchecked errors, missing error wrapping, bare returns"
         "Concurrency: goroutine leaks, missing synchronization, channel misuse"
         "Interface design: overly broad interfaces, unused interface methods"
         "Naming: non-idiomatic names, unexported when should be exported"
         "Security: input validation, SQL injection, path traversal")
        ("rust"
         "Ownership: unnecessary clones, lifetime issues, borrow conflicts"
         "Error handling: unwrap in production code, missing error context"
         "Unsafe: unnecessary unsafe blocks, missing safety docs"
         "Naming: non-idiomatic names, unclear module structure"
         "Security: unchecked input, integer overflow, buffer issues")
        ("java" "Null safety: missing null checks, Optional misuse"
         "Error handling: catching Exception/Throwable, empty catch blocks"
         "Concurrency: thread safety, shared mutable state"
         "Naming: unclear names, convention violations"
         "Security: injection risks, insecure deserialization")
        ("php" "Type safety: missing type declarations, loose comparisons"
         "Error handling: suppressed errors, missing exception handling"
         "Security: SQL injection, XSS, CSRF, path traversal"
         "Naming: PSR violations, unclear names"
         "Resource management: unclosed connections, memory leaks")
        ("scheme"
         "Data abstraction: records instead of ad-hoc alists, validated accessors"
         "Error handling: readable type-error/value-error, no swallowed exceptions"
         "Resource management: closed ports/files, every subprocess has a timeout"
         "Naming: consistent pi- prefixes, small single-purpose functions"
         "Security: input validation, no shell interpolation, no secrets in logs"))
    ) ;define

    (define (%basename file-path)
      (let ((parts (string-split file-path "/")))
        (if (null? parts) file-path (last parts))
      ) ;let
    ) ;define

    (define (%extension file-path)
      (let* ((name (%basename file-path)) (dot (string-index-right name #\.)))
        (if dot (string-downcase (substring name dot (string-length name))) "")
      ) ;let*
    ) ;define

    (define (pi-detect-language file-path)
      (let ((pair (assoc (%extension file-path) %language-map)))
        (if pair (cdr pair) #f)
      ) ;let
    ) ;define

    (define (pi-language-checklist language)
      (let ((entry (assoc language %language-checklists)))
        (if entry (cdr entry) '())
      ) ;let
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

    (define (%capitalize word)
      (if (string-null? word)
        word
        (string-append (string-upcase (substring word 0 1))
          (substring word 1 (string-length word))
        ) ;string-append
      ) ;if
    ) ;define

    (define (%format-language-block language)
      (let ((items (pi-language-checklist language)))
        (if (null? items)
          #f
          (string-append "### "
            (%capitalize language)
            "\n"
            (string-join (map (lambda (item) (string-append "- " item)) items) "\n")
          ) ;string-append
        ) ;if
      ) ;let
    ) ;define

    (define (pi-build-language-section languages)
      (let* ((blocks (filter (lambda (b) b) (map %format-language-block (%unique languages)))
             ) ;blocks
            ) ;
        (if (null? blocks) "" (string-append "\n\n" (string-join blocks "\n\n")))
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

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

(define-library (liii pi tools)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii pi truncate)
    (liii pi tools types)
    (liii pi tools record)
    (liii pi read)
    (liii pi write)
    (liii pi edit)
    (liii pi bash)
    (liii pi ls)
    (liii pi find)
    (liii pi grep)
    (liii pi web-search)
    (liii pi red-green tools)
    (liii pi blueprint tools)
    (liii pi compass tools)
  ) ;import
  (export PI_DEFAULT_TOOL_NAMES PI_ALL_TOOL_NAMES parse-tools-selection
    exclude-tools-from-list get-tool-definition execute-tool-call pi-read-schema
    pi-execute-read pi-write-schema pi-execute-write pi-edit-schema
    pi-execute-edit pi-bash-schema pi-execute-bash pi-ls-schema pi-execute-ls
    pi-find-schema pi-execute-find pi-grep-schema pi-execute-grep
    pi-web-search-schema pi-validate-web-search-args pi-format-web-search-result
    pi-execute-web-search pi-format-size pi-truncate-line pi-truncate-head
    pi-truncate-tail PI_DEFAULT_MAX_LINES PI_DEFAULT_MAX_BYTES
    PI_GREP_MAX_LINE_LENGTH pi-tool-definition pi-tool-definition?
    pi-tool-definition-name pi-tool-definition-description
    pi-tool-definition-parameters pi-tool-definition-execute
    pi-tool-definition-snippet pi-tool-definition-guidelines
    pi-tool-definition->alist pi-tool-result pi-tool-result?
    pi-tool-result-content pi-tool-result-is-error pi-tool-result-isError
    pi-tool-result->alist pi-trunc-result pi-trunc-result?
    pi-trunc-result-content pi-trunc-result-truncated pi-trunc-result-truncated?
    pi-trunc-result-truncated-by pi-trunc-result-total-lines
    pi-trunc-result-total-bytes pi-trunc-result-output-lines
    pi-trunc-result-output-bytes pi-trunc-result-first-line-exceeds-limit?
    pi-trunc-result-first-line-truncated pi-trunc-result-last-line-partial?
    pi-trunc-result-last-line-truncated pi-trunc-result->alist
  ) ;export
  (begin

    (define PI_DEFAULT_TOOL_NAMES '("read" "bash" "edit" "write"))
    (define PI_ALL_TOOL_NAMES
      '("read" "bash" "edit" "write" "grep" "find" "ls" "web_search")
    ) ;define

    (define (%apply-delta-item item current)
      (cond
       ((string-starts? item "+")
        (let ((name (substring item 1 (string-length item))))
          (if (member name current) current (append current (list name)))
        ) ;let
       ) ;
       ((string-starts? item "-")
        (let ((name (substring item 1 (string-length item))))
          (filter (lambda (x) (not (string=? x name))) current)
        ) ;let
       ) ;
       (else current)
      ) ;cond
    ) ;define

    (define (%parse-delta-tools items current)
      (let loop
        ((rest items) (acc current))
        (if (null? rest) acc (loop (cdr rest) (%apply-delta-item (car rest) acc)))
      ) ;let
    ) ;define

    (define (parse-tools-selection spec)
      (if (or (not spec) (string-null? (string-trim-both spec)))
        PI_DEFAULT_TOOL_NAMES
        (let ((items (map string-trim-both (string-split spec ","))))
          (if
            (and (pair? items)
              (or (string-starts? (car items) "+") (string-starts? (car items) "-"))
            ) ;and
            (%parse-delta-tools items PI_DEFAULT_TOOL_NAMES)
            items
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (exclude-tools-from-list tools-list spec)
      (if (or (not spec) (not (string? spec)) (string-null? (string-trim-both spec)))
        tools-list
        (let* ((raw-items (string-split spec ","))
               (excluded
                 (filter (lambda (s) (not (string-null? s))) (map string-trim-both raw-items))
               ) ;excluded
              ) ;
          (filter (lambda (t) (not (member t excluded))) tools-list)
        ) ;let*
      ) ;if
    ) ;define

    (define (%make-tool-def name desc schema exec snippet guidelines)
      (pi-tool-definition name desc schema exec snippet guidelines)
    ) ;define

    (define (%tool-def-read)
      (%make-tool-def "read"
        "Read file contents. Supports text files and images."
        (pi-read-schema)
        pi-execute-read
        "Read file contents"
        '("Use read to examine files instead of cat or sed.")
      ) ;%make-tool-def
    ) ;define

    (define (%tool-def-bash)
      (%make-tool-def "bash"
        "Execute a bash command in the current working directory."
        (pi-bash-schema)
        pi-execute-bash
        "Execute bash commands (ls, grep, find, etc.)"
        '("You can inspect PI_* environment variables for current model and session details.")
      ) ;%make-tool-def
    ) ;define

    (define (%tool-def-write)
      (%make-tool-def "write"
        "Write content to a file. Creates the file if it doesn't exist, overwrites if it does."
        (pi-write-schema)
        pi-execute-write
        "Create or overwrite files"
        '("Use write only for new files or complete rewrites.")
      ) ;%make-tool-def
    ) ;define

    (define %edit-guidelines
      '("Use edit for precise changes (edits[].oldText must match exactly)"
        "When changing multiple separate locations in one file, use one edit call with multiple entries in edits[] instead of multiple edit calls"
        "Each edits[].oldText is matched against the original file, not after earlier edits are applied. Do not emit overlapping or nested edits. Merge nearby changes into one edit."
        "Keep edits[].oldText as small as possible while still being unique in the file. Do not pad with large unchanged regions.")
    ) ;define

    (define (%tool-def-edit)
      (%make-tool-def "edit"
        "Edit a single file using exact text replacement."
        (pi-edit-schema)
        pi-execute-edit
        "Make precise file edits with exact text replacement, including multiple disjoint edits in one call"
        %edit-guidelines
      ) ;%make-tool-def
    ) ;define

    (define (%tool-def-ls)
      (%make-tool-def "ls"
        "List directory contents."
        (pi-ls-schema)
        pi-execute-ls
        "List directory contents"
        '()
      ) ;%make-tool-def
    ) ;define

    (define (%tool-def-find)
      (%make-tool-def "find"
        "Find files by glob pattern (respects .gitignore)."
        (pi-find-schema)
        pi-execute-find
        "Find files by glob pattern (respects .gitignore)"
        '()
      ) ;%make-tool-def
    ) ;define

    (define (%tool-def-grep)
      (%make-tool-def "grep"
        "Search file contents for patterns (respects .gitignore)."
        (pi-grep-schema)
        pi-execute-grep
        "Search file contents for patterns (respects .gitignore)"
        '()
      ) ;%make-tool-def
    ) ;define

    (define (%tool-def-web-search)
      (%make-tool-def "web_search"
        "Search the web using Gemini Google Search grounding."
        (pi-web-search-schema)
        pi-execute-web-search
        "Search the web for up-to-date information"
        '("Use web_search when you need real-time information or external references.")
      ) ;%make-tool-def
    ) ;define

    (define (get-tool-definition name)
      (cond ((string=? name "read") (%tool-def-read))
            ((string=? name "bash") (%tool-def-bash))
            ((string=? name "write") (%tool-def-write))
            ((string=? name "edit") (%tool-def-edit))
            ((string=? name "ls") (%tool-def-ls))
            ((string=? name "find") (%tool-def-find))
            ((string=? name "grep") (%tool-def-grep))
            ((string=? name "web_search") (%tool-def-web-search))
            ((string=? name "tdd_status") (pi-tdd-status-definition))
            ((string=? name "tdd_advance") (pi-tdd-advance-definition))
            ((string=? name "tdd_reset") (pi-tdd-reset-definition))
            ((string=? name "blueprint_update") (pi-blueprint-update-definition))
            ((string=? name "blueprint_status") (pi-blueprint-status-definition))
            ((string=? name "blueprint_next") (pi-blueprint-next-definition))
            ((string=? name "codebase_map") (pi-codebase-map-definition))
            ((string=? name "code_tour") (pi-code-tour-definition))
            (else #f)
      ) ;cond
    ) ;define

    (define (%make-tool-result text is-error?)
      (pi-tool-result text is-error?)
    ) ;define

    (define (%format-error-info info)
      (cond ((null? info) "An error occurred")
            ((string? (car info)) (car info))
            ((pair? (car info)) (%format-error-info (car info)))
            (else (object->string info))
      ) ;cond
    ) ;define

    (define (%run-tool-exec exec-fn name args opts)
      (cond
       ((string=? name "bash")
        (let-values (((out is-err) (apply exec-fn args opts)))
          (%make-tool-result out is-err)
        ) ;let-values
       ) ;
       ((string=? name "web_search")
        (let ((out (apply exec-fn args opts)))
          (cond ((pi-tool-result? out) out)
                ((and (pair? out) (assoc 'content out)) out)
                (else (%make-tool-result out #f))
          ) ;cond
        ) ;let
       ) ;
       (else
         (let ((out (apply exec-fn args opts)))
           (if (pi-tool-result? out) out (%make-tool-result out #f))
         ) ;let
       ) ;else
      ) ;cond
    ) ;define

    (define (execute-tool-call name args . opts)
      (let ((def (get-tool-definition name)))
        (if (not def)
          (%make-tool-result (string-append "Tool " name " not found") #t)
          (catch #t
            (lambda () (%run-tool-exec (pi-tool-definition-execute def) name args opts))
            (lambda (tag . info) (%make-tool-result (%format-error-info info) #t))
          ) ;catch
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
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
    (liii pi extension types)
    (liii pi extension registry)
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

    (define (%tool-def-read)
      (pi-tool-definition "read"
        "Read file contents. Supports text files and images."
        (pi-read-schema)
        pi-execute-read
        "Read file contents"
        '("Use read to examine files instead of cat or sed.")
      ) ;pi-tool-definition
    ) ;define

    (define (%tool-def-bash . maybe-default-timeout)
      (let ((def-timeout (if (pair? maybe-default-timeout) (car maybe-default-timeout) 600))
           ) ;
        (pi-tool-definition "bash"
          "Execute a bash command in the current working directory."
          (pi-bash-schema def-timeout)
          pi-execute-bash
          "Execute bash commands (ls, grep, find, etc.)"
          '("You can inspect PI_* environment variables for current model and session details.")
        ) ;pi-tool-definition
      ) ;let
    ) ;define

    (define (%tool-def-write)
      (pi-tool-definition "write"
        "Write content to a file. Creates the file if it doesn't exist, overwrites if it does."
        (pi-write-schema)
        pi-execute-write
        "Create or overwrite files"
        '("Use write only for new files or complete rewrites.")
      ) ;pi-tool-definition
    ) ;define

    (define %edit-guidelines
      '("Use edit for precise changes (edits[].oldText must match exactly)"
        "When changing multiple separate locations in one file, use one edit call with multiple entries in edits[] instead of multiple edit calls"
        "Each edits[].oldText is matched against the original file, not after earlier edits are applied. Do not emit overlapping or nested edits. Merge nearby changes into one edit."
        "Keep edits[].oldText as small as possible while still being unique in the file. Do not pad with large unchanged regions.")
    ) ;define

    (define (%tool-def-edit)
      (pi-tool-definition "edit"
        "Edit a single file using exact text replacement."
        (pi-edit-schema)
        pi-execute-edit
        "Make precise file edits with exact text replacement, including multiple disjoint edits in one call"
        %edit-guidelines
      ) ;pi-tool-definition
    ) ;define

    (define (%tool-def-ls)
      (pi-tool-definition "ls"
        "List directory contents."
        (pi-ls-schema)
        pi-execute-ls
        "List directory contents"
        '()
      ) ;pi-tool-definition
    ) ;define

    (define (%tool-def-find)
      (pi-tool-definition "find"
        "Find files by glob pattern (respects .gitignore)."
        (pi-find-schema)
        pi-execute-find
        "Find files by glob pattern (respects .gitignore)"
        '()
      ) ;pi-tool-definition
    ) ;define

    (define (%tool-def-grep)
      (pi-tool-definition "grep"
        "Search file contents for patterns (respects .gitignore)."
        (pi-grep-schema)
        pi-execute-grep
        "Search file contents for patterns (respects .gitignore)"
        '()
      ) ;pi-tool-definition
    ) ;define

    (define (%tool-def-web-search)
      (pi-tool-definition "web_search"
        "Search the web using Gemini Google Search grounding."
        (pi-web-search-schema)
        pi-execute-web-search
        "Search the web for up-to-date information"
        '("Use web_search when you need real-time information or external references.")
      ) ;pi-tool-definition
    ) ;define

    (define (%extension-tool-def->pi-tool-def ext-tool)
      (cond ((pi-tool-definition? ext-tool) ext-tool)
            ((extension-tool-def? ext-tool)
             (pi-tool-definition (extension-tool-def-name ext-tool)
               (extension-tool-def-description ext-tool)
               (extension-tool-def-parameters ext-tool)
               (extension-tool-def-execute ext-tool)
               (extension-tool-def-snippet ext-tool)
               (extension-tool-def-guidelines ext-tool)
             ) ;pi-tool-definition
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (get-tool-definition name . maybe-opts)
      (let ((def-timeout
              (cond ((null? maybe-opts) 600)
                    ((number? (car maybe-opts)) (car maybe-opts))
                    (else (pi-keyword-value :timeout maybe-opts 600))
              ) ;cond
            ) ;def-timeout
           ) ;
        (cond ((string=? name "read") (%tool-def-read))
              ((string=? name "bash") (%tool-def-bash def-timeout))
              ((string=? name "write") (%tool-def-write))
              ((string=? name "edit") (%tool-def-edit))
              ((string=? name "ls") (%tool-def-ls))
              ((string=? name "find") (%tool-def-find))
              ((string=? name "grep") (%tool-def-grep))
              ((string=? name "web_search") (%tool-def-web-search))
              (else
                (let ((reg (current-extension-registry)))
                  (if reg
                    (let ((tool (find-extension-tool reg name)))
                      (if tool (%extension-tool-def->pi-tool-def tool) #f)
                    ) ;let
                    #f
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
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
          (pi-tool-result out is-err)
        ) ;let-values
       ) ;
       ((string=? name "web_search")
        (let ((out (apply exec-fn args opts)))
          (cond ((pi-tool-result? out) out)
                ((and (pair? out) (assoc 'content out)) out)
                (else (pi-tool-result out #f))
          ) ;cond
        ) ;let
       ) ;
       (else
         (let* ((raw-out
                  ;; 扩展工具可能只接受固定个数的参数：先按 arity 判断能不能带
                  ;; opts。不用 catch 兜底后重跑——那会把工具自身的失败也重跑一遍
                  (if
                    (or (null? opts) (not (aritable? exec-fn (+ 1 (length opts)))))
                    (exec-fn args)
                    (apply exec-fn args opts)
                  ) ;if
                ) ;raw-out
                (out (if (pi-tool-result? raw-out) raw-out (pi-tool-result raw-out #f)))
               ) ;
           out
         ) ;let*
       ) ;else
      ) ;cond
    ) ;define

    (define (execute-tool-call name args . opts)
      (let ((def (get-tool-definition name)))
        (if (not def)
          (pi-tool-result (string-append "Tool " name " not found") #t)
          (catch #t
            (lambda () (%run-tool-exec (pi-tool-definition-execute def) name args opts))
            (lambda (tag . info) (pi-tool-result (%format-error-info info) #t))
          ) ;catch
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi read)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii path)
    (liii os)
    (liii base)
    (liii subprocess)
    (liii pi truncate)
  ) ;import
  (export pi-read-schema pi-execute-read pi-arg-ref pi-resolve-path
    pi-keyword-value pi-get-agent-dir pi-command-failure-detail pi-command-error
    pi-proc-accepts-args?
  ) ;export
  (begin

    ;; pi 端口统一的「子进程失败 → 工具错误」策略：诊断文本与错误标签
    ;; 只有这一处实现，各工具（grep/install/...）都经由它转换，
    ;; 避免每个工具各写一套消息格式。
    (define (pi-command-failure-detail err)
      (string-trim-right (process-error-message err))
    ) ;define

    (define (pi-command-error tool-name err)
      (error 'error
        (string-append tool-name " failed: " (pi-command-failure-detail err))
      ) ;error
    ) ;define

    ;; 过程能否接受 n 个参数：扩展提供的工具可能只接受固定个数的参数，
    ;; 调用前先按 arity 判断，而不是靠 catch 兜底后重跑——后者会把工具
    ;; 自身的失败也重跑一遍，还丢掉调用方传入的 :timeout。
    (define (pi-proc-accepts-args? proc n)
      (let ((ar (arity proc)))
        (if (pair? ar)
          (let ((max-args (cdr ar)))
            (if (number? max-args) (>= max-args n) #t)
          ) ;let
          #t
        ) ;if
      ) ;let
    ) ;define

    (define (pi-arg-ref args key . default-opt)
      (let ((default-val (if (pair? default-opt) (car default-opt) #f))
            (str-key (if (symbol? key) (symbol->string key) key))
            (sym-key (if (string? key) (string->symbol key) key))
           ) ;
        (cond ((assoc str-key args) => cdr)
              ((assoc sym-key args) => cdr)
              (else default-val)
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-resolve-path raw-path cwd)
      (let* ((p (path raw-path)) (expanded (path-expanduser p)))
        (if (path-absolute? expanded)
          (path->string (path-resolve expanded))
          (path->string (path-resolve (path-join (path cwd) expanded)))
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-get-agent-dir . opt-dir)
      (let ((custom (and (pair? opt-dir) (car opt-dir)))
            (env-dir (get-environment-variable "PI_CODING_AGENT_DIR"))
           ) ;
        (cond
         ((and (string? custom) (not (string-null? (string-trim-both custom))))
          (pi-resolve-path custom (getcwd))
         ) ;
         ((and (string? env-dir) (not (string-null? (string-trim-both env-dir))))
          (pi-resolve-path env-dir (getcwd))
         ) ;
         (else (pi-resolve-path "~/.pi/agent" (getcwd)))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-read-schema)
      (list (cons "type" "object")
        (cons "properties"
          (list (cons "path"
                  (list (cons "type" "string")
                    (cons "description" "Path to the file to read (relative or absolute)")
                  ) ;list
                ) ;cons
            (cons "offset"
              (list (cons "type" "number")
                (cons "description" "Line number to start reading from (1-indexed)")
              ) ;list
            ) ;cons
            (cons "limit"
              (list (cons "type" "number")
                (cons "description" "Maximum number of lines to read")
              ) ;list
            ) ;cons
          ) ;list
        ) ;cons
        (cons "required" #("path"))
      ) ;list
    ) ;define

    (define (pi-keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%slice-lines lines start count)
      (let loop
        ((lst lines) (i 0) (acc '()))
        (cond ((null? lst) (reverse acc))
              ((< i start) (loop (cdr lst) (+ i 1) acc))
              ((>= i (+ start count)) (reverse acc))
              (else (loop (cdr lst) (+ i 1) (cons (car lst) acc)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%format-read-output truncation raw-path start-display total-lines user-lines)
      (let* ((output (pi-trunc-result-content truncation))
             (by (pi-trunc-result-truncated-by truncation))
             (out-lines (pi-trunc-result-output-lines truncation))
             (end-display (+ start-display out-lines -1))
             (next-offset (+ end-display 1))
            ) ;
        (cond ((pi-trunc-result-first-line-exceeds-limit? truncation)
               (string-append "[Line "
                 (number->string start-display)
                 " is large, exceeds 50.0KB limit. Use bash: sed -n '"
                 (number->string start-display)
                 "p' "
                 raw-path
                 " | head -c 51200]"
               ) ;string-append
              ) ;
              ((pi-trunc-result-truncated truncation)
               (let ((limit-note (if (eq? by 'lines) "" " (50.0KB limit)")))
                 (string-append output
                   "\n\n[Showing lines "
                   (number->string start-display)
                   "-"
                   (number->string end-display)
                   " of "
                   (number->string total-lines)
                   limit-note
                   ". Use offset="
                   (number->string next-offset)
                   " to continue.]"
                 ) ;string-append
               ) ;let
              ) ;
              ((and user-lines (< (+ start-display user-lines -1) total-lines))
               (let* ((remaining (- total-lines (+ start-display user-lines -1)))
                      (user-next (+ start-display user-lines))
                     ) ;
                 (string-append output
                   "\n\n["
                   (number->string remaining)
                   (if (= remaining 1)
                     " more line in file. Use offset="
                     " more lines in file. Use offset="
                   ) ;if
                   (number->string user-next)
                   " to continue.]"
                 ) ;string-append
               ) ;let*
              ) ;
              (else output)
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-execute-read args . opts)
      (let* ((timed-out? (pi-keyword-value :timed-out? opts #f))
             (cwd (pi-keyword-value :cwd opts (getcwd)))
             (raw-path (pi-arg-ref args "path"))
             (offset (pi-arg-ref args "offset" #f))
             (limit (pi-arg-ref args "limit" #f))
            ) ;
        (when timed-out?
          (error 'error "Command timed out")
        ) ;when
        (unless (string? raw-path)
          (error 'error "path must be a string")
        ) ;unless
        (let ((abs-path (pi-resolve-path raw-path cwd)))
          (unless (path-exists? (path abs-path))
            (error 'error (string-append "Path not found: " raw-path))
          ) ;unless
          (when (path-dir? (path abs-path))
            (error 'error (string-append "Not a file: " raw-path))
          ) ;when
          (let* ((file-text (path-read-text (path abs-path)))
                 (all-lines (pi-split-lines file-text))
                 (total-lines (length all-lines))
                 (start-line (if offset (max 0 (- offset 1)) 0))
                 (start-display (+ start-line 1))
                ) ;
            (when (and offset (>= start-line total-lines))
              (error 'error
                (string-append "Offset "
                  (number->string offset)
                  " is beyond end of file ("
                  (number->string total-lines)
                  " lines total)"
                ) ;string-append
              ) ;error
            ) ;when
            (let* ((user-lines (if limit (min limit (- total-lines start-line)) #f))
                   (count (if limit user-lines (- total-lines start-line)))
                   (selected (%slice-lines all-lines start-line count))
                   (selected-text (string-join selected "\n"))
                   (truncation (pi-truncate-head selected-text))
                  ) ;
              (%format-read-output truncation raw-path start-display total-lines user-lines)
            ) ;let*
          ) ;let*
        ) ;let
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

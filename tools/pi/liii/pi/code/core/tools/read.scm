;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core tools read)
  (import (scheme base)
    (scheme process-context)
    (liii string)
    (liii list)
    (liii path)
    (liii os)
    (liii pi code core tools truncate)
  ) ;import
  (export pi-read-schema pi-execute-read pi-arg-ref pi-resolve-path pi-abs-path
    pi-keyword-value pi-get-agent-dir pi-int-arg
  ) ;export
  (begin

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

    ;; 工具参数中的整数规范化：
    ;; 模型偶尔用 1.0/90.0 这类浮点形式表示行号（JSON 解析后是 double），
    ;; 数值上确实是整数的按整数处理；1.5 这类无法安全转换的给出清晰的
    ;; 参数错误，避免把 list-drop/list-take 的内部类型错误泄露给调用方。
    (define (pi-int-arg val name)
      (cond ((exact-integer? val) val)
            ((not (real? val)) (error 'error (string-append name " must be an integer")))
            ((not (< (abs val) 9007199254740992))
             ;; 超出 double 可精确表示的范围，无法安全转换为整数
             (error 'error (string-append name " must be an integer"))
            ) ;
            (else
              (let ((f (floor val)))
                ;; 要求数值上恰好是整数
                (if (= f val)
                  (inexact->exact f)
                  (error 'error (string-append name " must be an integer"))
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (pi-resolve-path raw-path cwd)
      (let* ((p (path raw-path)) (expanded (path-expanduser p)))
        (if (path-absolute? expanded)
          (path->string (path-resolve expanded))
          (path->string (path-resolve (path-join (path cwd) expanded)))
        ) ;if
      ) ;let*
    ) ;define

    ;; 绝对化路径（~ 展开 + 相对路径按 cwd 解析），返回 path 对象
    (define (pi-abs-path raw-path)
      (path-resolve (path-expanduser (path raw-path)))
    ) ;define

    ;; agent 目录：会话（sessions/）、扩展（extensions/）等非 JSON 配置的默认位置
    ;; 默认为 ~/.pi/agent：gf pi 与 node 版 pi 共享会话历史与扩展，
    ;; 不随 JSON 配置目录（~/.gf/agent）迁移
    ;; 返回 path 对象
    (define (pi-get-agent-dir . opt-dir)
      (let ((custom (and (pair? opt-dir) (car opt-dir)))
            (env-dir (get-environment-variable "PI_CODING_AGENT_DIR"))
           ) ;
        (cond
         ((and (string? custom) (not (string-null? (string-trim-both custom))))
          (pi-abs-path custom)
         ) ;
         ((and (string? env-dir) (not (string-null? (string-trim-both env-dir))))
          (pi-abs-path env-dir)
         ) ;
         (else (pi-abs-path "~/.pi/agent"))
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
      (list-take (list-drop lines start) count)
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
             (raw-offset (pi-arg-ref args "offset" #f))
             (raw-limit (pi-arg-ref args "limit" #f))
             ;; 模型偶尔用 1.0/90.0 这类浮点形式表示行号，这里统一规范化为整数
             (offset (if raw-offset (pi-int-arg raw-offset "offset") #f))
             (limit (if raw-limit (pi-int-arg raw-limit "limit") #f))
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

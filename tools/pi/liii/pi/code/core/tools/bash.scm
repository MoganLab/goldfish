;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core tools bash)
  (import (scheme base)
    (scheme file)
    (scheme inexact)
    (scheme time)
    (liii string)
    (liii os)
    (liii path)
    (liii either)
    (liii subprocess)
    (liii tui text-width)
    (liii pi code utils shell)
    (liii pi code core tools read)
    (liii pi code core tools truncate)
  ) ;import
  (export pi-bash-schema pi-execute-bash pi-find-bash-path pi-find-grep-path
    pi-truncate-bash-output pi-bash-clean-output PI_MAX_TIMEOUT_SECONDS
  ) ;export
  (begin

    (define (%find-windows-bash)
      (let* ((pf (getenv "ProgramFiles"))
             (local-app (getenv "LOCALAPPDATA"))
             (pf86 (getenv "ProgramFiles(x86)"))
             (candidates
               (append (list "C:\\Program Files\\Git\\bin\\bash.exe"
                         "C:\\Program Files\\Git\\usr\\bin\\bash.exe"
                       ) ;list
                 (if (and pf (not (string-null? pf)))
                   (list (string-append pf "\\Git\\bin\\bash.exe")
                     (string-append pf "\\Git\\usr\\bin\\bash.exe")
                   ) ;list
                   '()
                 ) ;if
                 (if (and local-app (not (string-null? local-app)))
                   (list (string-append local-app "\\Programs\\Git\\bin\\bash.exe")
                     (string-append local-app "\\Programs\\Git\\usr\\bin\\bash.exe")
                   ) ;list
                   '()
                 ) ;if
                 (list "C:\\Program Files (x86)\\Git\\bin\\bash.exe"
                   "C:\\Program Files (x86)\\Git\\usr\\bin\\bash.exe"
                 ) ;list
                 (if (and pf86 (not (string-null? pf86)))
                   (list (string-append pf86 "\\Git\\bin\\bash.exe")
                     (string-append pf86 "\\Git\\usr\\bin\\bash.exe")
                   ) ;list
                   '()
                 ) ;if
                 (list "bash.exe" "bash")
               ) ;append
             ) ;candidates
            ) ;
        (let loop
          ((rest candidates))
          (cond ((null? rest) "bash")
                ((or (string=? (car rest) "bash") (string=? (car rest) "bash.exe")) (car rest))
                ((file-exists? (car rest)) (car rest))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;let*
    ) ;define

    (define (%find-posix-bash)
      (cond ((file-exists? "/bin/bash") "/bin/bash")
            ((file-exists? "/usr/bin/bash") "/usr/bin/bash")
            (else "bash")
      ) ;cond
    ) ;define

    (define (pi-find-bash-path . maybe-os)
      (let ((os (if (null? maybe-os) (if (os-windows?) 'windows 'posix) (car maybe-os))))
        (case os
         ((windows) (%find-windows-bash))
         (else (%find-posix-bash))
        ) ;case
      ) ;let
    ) ;define

    (define (pi-find-grep-path)
      (if (os-windows?)
        (let ((bash (pi-find-bash-path)))
          (if (string-contains? bash "Git")
            (let* ((git-dir
                     (path-parent (if (string-contains? bash "usr") (path-parent (path bash)) (path bash))
                     ) ;path-parent
                   ) ;git-dir
                   (grep-p (path-join git-dir "usr" "bin" "grep.exe"))
                  ) ;
              (if (path-file? grep-p) (path->string grep-p) "grep")
            ) ;let*
            "grep"
          ) ;if
        ) ;let
        "grep"
      ) ;if
    ) ;define

    (define PI_MAX_TIMEOUT_SECONDS 2147483.647)

    (define (pi-bash-schema . maybe-default-timeout)
      (let* ((def-timeout
               (if (and (pair? maybe-default-timeout)
                     (number? (car maybe-default-timeout))
                     (> (car maybe-default-timeout) 0)
                   ) ;and
                 (car maybe-default-timeout)
                 600
               ) ;if
             ) ;def-timeout
             (desc (string-append "Timeout in seconds (optional, defaults to "
                     (number->string def-timeout)
                     "s)"
                   ) ;string-append
             ) ;desc
            ) ;
        (list (cons "type" "object")
          (cons "properties"
            (list (cons "command"
                    (list (cons "type" "string") (cons "description" "Shell command to execute"))
                  ) ;cons
              (cons "timeout" (list (cons "type" "number") (cons "description" desc)))
            ) ;list
          ) ;cons
          (cons "required" #("command"))
        ) ;list
      ) ;let*
    ) ;define

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (car rest)) (pair? (caar rest)) (assq key (car rest)))
               (cdr (assq key (car rest)))
              ) ;
              ((and (pair? (car rest)) (eq? (caar rest) key)) (cdar rest))
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (%format-tail-notice trunc)
      (let* ((by (pi-trunc-result-truncated-by trunc))
             (total (pi-trunc-result-total-lines trunc))
             (out-lines (pi-trunc-result-output-lines trunc))
             (out-bytes (pi-trunc-result-output-bytes trunc))
             (start-line (+ (- total out-lines) 1))
             (end-line total)
            ) ;
        (cond ((pi-trunc-result-last-line-partial? trunc)
               (string-append "\n\n[Showing last "
                 (pi-format-size out-bytes)
                 " of line "
                 (number->string end-line)
                 ".]"
               ) ;string-append
              ) ;
              ((eq? by 'lines)
               (string-append "\n\n[Showing lines "
                 (number->string start-line)
                 "-"
                 (number->string end-line)
                 " of "
                 (number->string total)
                 ".]"
               ) ;string-append
              ) ;
              (else (string-append "\n\n[Showing lines "
                      (number->string start-line)
                      "-"
                      (number->string end-line)
                      " of "
                      (number->string total)
                      " (50.0KB limit).]"
                    ) ;string-append
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-truncate-bash-output text)
      (let ((trunc (pi-truncate-tail text)))
        (if (pi-trunc-result-truncated trunc)
          (string-append (pi-trunc-result-content trunc) (%format-tail-notice trunc))
          text
        ) ;if
      ) ;let
    ) ;define

    (define (%bash-now-ms)
      (* 1000.0 (/ (current-jiffy) (jiffies-per-second)))
    ) ;define

    ;; 上游 Bash 工具按 100ms 节流发送累计输出快照。
    (define %bash-update-throttle-ms 100)

    ;; 对齐上游 bash-executor appendText：
    ;; sanitizeBinaryOutput(stripAnsi(rawText)).replace(/\r/g, "")
    ;; 即：剥 ANSI（CSI+OSC）、删控制字符、删 \r，保留 \t\n
    (define (pi-bash-clean-output raw)
      (string-replace (sanitize-binary-output (strip-ansi raw)) "\r" "")
    ) ;define

    ;; 基于 (liii subprocess) 公开的 run-streaming-chunks 实现流式执行：
    ;; stdout/stderr 合并到同一管道，按原始字节块收集输出，
    ;; 并按 100ms 节流回调累计文本快照。
    ;; 返回值：(values 累计文本 退出码)，超时强杀时退出码为 -1。
    ;; 后台流式执行：累计文本保留原始字节，回调与返回值都是清理后的文本。
    ;; 清理只在通过 100ms 节流后才执行（高频字节块不触发挥霍性的全量清理，
    ;; 否则大输出流式时 CPU 会飕升）；每次对累计全文重新清理，
    ;; 跨块不完整转义序列在下一帧自愈（等效上游 splitIncompleteAnsiSuffix）
    (define (%run-bash-streaming command cwd timeout on-update)
      (let* ((text "") (last-update 0) (last-raw ""))
        (define (send-update! force?)
          (let ((now (%bash-now-ms)))
            (when
              (and (not (string=? text last-raw))
                (or force? (>= (- now last-update) %bash-update-throttle-ms))
              ) ;and
              (on-update (pi-bash-clean-output text))
              (set! last-raw text)
              (set! last-update now)
            ) ;when
          ) ;let
        ) ;define
        (define (append-chunk! chunk)
          (when (and (string? chunk) (not (string-null? chunk)))
            (set! text (string-append text chunk))
            (send-update! #f)
          ) ;when
        ) ;define
        (let ((ret (run-streaming-chunks (list (pi-find-bash-path) "-c" command)
                     :cwd      cwd
                     :on-chunk append-chunk!
                     :timeout  timeout
                   ) ;run-streaming-chunks
              ) ;ret
             ) ;
          (send-update! #t)
          (let ((clean (pi-bash-clean-output text)))
            (if (eq? ret #f) (values clean -1) (values clean ret))
          ) ;let
        ) ;let
      ) ;let*
    ) ;define

    (define (%format-bash-result raw-text exit-code timeout)
      (let* ((clean-text (pi-bash-clean-output raw-text))
             (text (pi-truncate-bash-output clean-text))
             (trimmed-text (string-trim-both text))
            ) ;
        (cond ((= exit-code 0) (values text #f))
              ((= exit-code -1)
               (let ((msg (string-append "Command timed out after " (number->string timeout) " seconds")
                     ) ;msg
                    ) ;
                 (values (if (string-null? trimmed-text) msg (string-append text "\n\n" msg)) #t)
               ) ;let
              ) ;
              (else
                (let ((msg (string-append "Command exited with code " (number->string exit-code))))
                  (values (if (string-null? trimmed-text) msg (string-append text "\n\n" msg)) #t)
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-execute-bash args . opts)
      (let* ((timed-out? (%keyword-value :timed-out? opts #f))
             (cwd (%keyword-value :cwd opts (getcwd)))
             (raw-def (%keyword-value :timeout opts 600))
             (def-timeout (if (and (number? raw-def) (> raw-def 0)) raw-def 600))
             (has-timeout? (or (assoc "timeout" args) (assoc 'timeout args)))
             (timeout
               (if has-timeout?
                 (let ((raw-arg (cdr has-timeout?)))
                   (cond
                    ((or (not (finite? raw-arg)) (<= raw-arg 0))
                     (error 'error "Invalid timeout: must be a finite number of seconds")
                    ) ;
                    ((> raw-arg PI_MAX_TIMEOUT_SECONDS)
                     (error 'error
                       (string-append "Invalid timeout: maximum is "
                         (number->string PI_MAX_TIMEOUT_SECONDS)
                         " seconds"
                       ) ;string-append
                     ) ;error
                    ) ;
                    (else raw-arg)
                   ) ;cond
                 ) ;let
                 def-timeout
               ) ;if
             ) ;timeout
             (command (pi-arg-ref args "command"))
             (on-update (%keyword-value :on-update opts #f))
            ) ;
        (if timed-out?
          (values (string-append "Command timed out after " (number->string timeout) " seconds")
            #t
          ) ;values
          (begin
            (unless (string? command)
              (error 'error "command must be a string")
            ) ;unless
            (when (and on-update (not (procedure? on-update)))
              (error 'type-error "on-update must be a procedure")
            ) ;when
            (if on-update
              (let-values (((text code) (%run-bash-streaming command cwd timeout on-update)))
                (%format-bash-result text code timeout)
              ) ;let-values
              (let ((res (run-either (list (pi-find-bash-path) "-c" command)
                           :cwd     cwd
                           :stdin   'null
                           :stdout  'capture
                           :stderr  'stdout
                           :timeout timeout
                         ) ;run-either
                    ) ;res
                   ) ;
                (if (either-right? res)
                  (%format-bash-result (to-right res) 0 timeout)
                  (let ((err (to-left res)))
                    (%format-bash-result (process-error-stdout err)
                      (process-error-code err)
                      timeout
                    ) ;%format-bash-result
                  ) ;let
                ) ;if
              ) ;let
            ) ;if
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

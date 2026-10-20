;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi bash)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii os)
    (liii path)
    (liii either)
    (liii subprocess)
    (liii pi read)
    (liii pi truncate)
  ) ;import
  (export pi-bash-schema pi-execute-bash pi-find-bash-path pi-find-grep-path)
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

    (define (pi-bash-schema)
      (list (cons "type" "object")
        (cons "properties"
          (list (cons "command"
                  (list (cons "type" "string") (cons "description" "Shell command to execute"))
                ) ;cons
            (cons "timeout"
              (list (cons "type" "number")
                (cons "description" "Timeout in seconds (optional, no default timeout)")
              ) ;list
            ) ;cons
          ) ;list
        ) ;cons
        (cons "required" #("command"))
      ) ;list
    ) ;define

    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
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

    (define (%truncate-bash-output text)
      (let ((trunc (pi-truncate-tail text)))
        (if (pi-trunc-result-truncated trunc)
          (string-append (pi-trunc-result-content trunc) (%format-tail-notice trunc))
          text
        ) ;if
      ) ;let
    ) ;define

    (define (%format-bash-result raw-text exit-code timeout)
      (let* ((text (%truncate-bash-output raw-text)) (trimmed-text (string-trim-both text)))
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
             (def-timeout (%keyword-value :timeout opts 120))
             (command (pi-arg-ref args "command"))
             (arg-timeout (pi-arg-ref args "timeout" #f))
             (timeout (or arg-timeout def-timeout 120))
            ) ;
        (if timed-out?
          (values (string-append "Command timed out after " (number->string timeout) " seconds")
            #t
          ) ;values
          (begin
            (unless (string? command)
              (error 'error "command must be a string")
            ) ;unless
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
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

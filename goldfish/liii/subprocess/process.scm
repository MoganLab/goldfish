;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii subprocess process)
  (export process? process-pid process-spawn process-alive? process-poll
    process-wait process-terminate process-kill process-kill-tree process-close
    process-close-stdin process-last-activity-time process-idle-seconds
    process-write process-write-stdin process-read process-read-line
    %keyword-value %valid-env? %normalize-stream-spec %normalize-cwd
  ) ;export
  (import (scheme base) (liii error) (liii list) (liii path))
  (begin

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

    (define (%valid-env? env)
      (or (not env)
        (and (proper-list? env)
          (every
            (lambda (x) (and (pair? x) (string? (car x)) (string? (cdr x))))
            env
          ) ;every
        ) ;and
      ) ;or
    ) ;define

    (define (%normalize-stream-spec caller kw val allowed-symbols default)
      (cond ((not val) default)
            ((path? val) (path->string val))
            ((string? val) val)
            ((symbol? val)
             (unless (memq val allowed-symbols)
               (value-error (format #f
                              "Invalid ~a value: ~a, expected one of ~a or a string/path"
                              kw val allowed-symbols
                            ) ;format
               ) ;value-error
             ) ;unless
             val
            ) ;
            (else (type-error (format #f
                                "~a: ~a must be a symbol, string, or path, got: ~a"
                                caller
                                kw
                                (object->string val)
                              ) ;format
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    (define (%normalize-cwd caller cwd)
      (cond ((not cwd) #f)
            ((path? cwd) (path->string cwd))
            ((string? cwd) cwd)
            (else (type-error (format #f "~a: :cwd must be a string or path" caller)))
      ) ;cond
    ) ;define

    (define (process? obj)
      (g_process? obj)
    ) ;define

    (define (process-pid proc)
      (unless (process? proc)
        (type-error "process-pid: argument must be a process")
      ) ;unless
      (g_subprocess-pid proc)
    ) ;define

    (define (process-alive? proc)
      (unless (process? proc)
        (type-error "process-alive?: argument must be a process")
      ) ;unless
      (g_subprocess-alive? proc)
    ) ;define

    (define (process-poll proc)
      (unless (process? proc)
        (type-error "process-poll: argument must be a process")
      ) ;unless
      (g_subprocess-poll proc)
    ) ;define

    (define (process-wait proc . args)
      (unless (process? proc)
        (type-error "process-wait: first argument must be a process")
      ) ;unless
      (let ((timeout-ms (if (pair? args) (car args) -1)))
        (unless (integer? timeout-ms)
          (type-error "process-wait: timeout must be an integer")
        ) ;unless
        (g_subprocess-wait proc timeout-ms)
      ) ;let
    ) ;define

    (define (process-terminate proc)
      (unless (process? proc)
        (type-error "process-terminate: argument must be a process")
      ) ;unless
      (g_subprocess-terminate proc)
    ) ;define

    (define (process-kill proc)
      (unless (process? proc)
        (type-error "process-kill: argument must be a process")
      ) ;unless
      (g_subprocess-kill proc)
    ) ;define

    (define (process-kill-tree proc-or-pid . opts)
      (unless (or (process? proc-or-pid) (and (integer? proc-or-pid) (exact? proc-or-pid)))
        (type-error "process-kill-tree: argument must be a process or exact integer")
      ) ;unless
      (when (and (integer? proc-or-pid) (not (process? proc-or-pid)))
        (unless (> proc-or-pid 0)
          (value-error "process-kill-tree: pid must be a positive integer")
        ) ;unless
      ) ;when
      (let ((signal (%keyword-value :signal opts 'sigkill))
            (timeout (%keyword-value :timeout opts 3))
           ) ;
        (unless (symbol? signal)
          (type-error "process-kill-tree: :signal must be a symbol")
        ) ;unless
        (unless (or (eq? signal 'sigkill) (eq? signal 'sigterm))
          (value-error "process-kill-tree: :signal must be 'sigkill or 'sigterm")
        ) ;unless
        (unless (real? timeout)
          (type-error "process-kill-tree: :timeout must be a real number")
        ) ;unless
        (when (< timeout 0)
          (value-error "process-kill-tree: :timeout must be non-negative")
        ) ;when
        (if (and (process? proc-or-pid) (not (process-alive? proc-or-pid)))
          #t
          (let ((timeout-ms (exact (round (* timeout 1000)))))
            (g_subprocess-kill-tree proc-or-pid signal timeout-ms)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (process-close proc)
      (unless (process? proc)
        (type-error "process-close: argument must be a process")
      ) ;unless
      (g_subprocess-close proc)
    ) ;define

    (define (process-close-stdin proc)
      (unless (process? proc)
        (type-error "process-close-stdin: argument must be a process")
      ) ;unless
      (g_subprocess-close-stdin proc)
    ) ;define

    (define (process-last-activity-time proc)
      (unless (process? proc)
        (type-error "process-last-activity-time: argument must be a process")
      ) ;unless
      (g_subprocess-last-activity-time proc)
    ) ;define

    (define (process-idle-seconds proc)
      (unless (process? proc)
        (type-error "process-idle-seconds: argument must be a process")
      ) ;unless
      (g_subprocess-idle-seconds proc)
    ) ;define

    (define (process-write-stdin proc data)
      (unless (process? proc)
        (type-error "process-write-stdin: first argument must be a process")
      ) ;unless
      (unless (or (string? data) (bytevector? data))
        (type-error "process-write-stdin: second argument must be a string or bytevector"
        ) ;type-error
      ) ;unless
      (g_subprocess-write proc data)
    ) ;define

    (define process-write process-write-stdin)

    (define (process-read proc . opts)
      (unless (process? proc)
        (type-error "process-read: first argument must be a process")
      ) ;unless
      (let ((max-bytes (%keyword-value :max-bytes opts 65536))
            (stream (%keyword-value :stream opts 'stdout))
           ) ;
        (unless (integer? max-bytes)
          (type-error "process-read: :max-bytes must be an integer")
        ) ;unless
        (unless (> max-bytes 0)
          (value-error "process-read: :max-bytes must be a positive integer")
        ) ;unless
        (unless (symbol? stream)
          (type-error "process-read: :stream must be a symbol")
        ) ;unless
        (unless (or (eq? stream 'stdout) (eq? stream 'stderr))
          (value-error "process-read: :stream must be 'stdout or 'stderr")
        ) ;unless
        (g_subprocess-read proc max-bytes stream)
      ) ;let
    ) ;define

    (define (process-read-line proc . opts)
      (unless (process? proc)
        (type-error "process-read-line: first argument must be a process")
      ) ;unless
      (let ((stream (%keyword-value :stream opts 'stdout)))
        (unless (symbol? stream)
          (type-error "process-read-line: :stream must be a symbol")
        ) ;unless
        (unless (or (eq? stream 'stdout) (eq? stream 'stderr))
          (value-error "process-read-line: :stream must be 'stdout or 'stderr")
        ) ;unless
        (g_subprocess-read-line proc stream)
      ) ;let
    ) ;define

    (define (%normalize-cmd-arg arg)
      (cond ((string? arg) arg)
            ((symbol? arg) (symbol->string arg))
            ((path? arg) (path->string arg))
            (else (type-error (format #f
                                "process-spawn: command element must be a string, symbol, or path, got: ~a"
                                (object->string arg)
                              ) ;format
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    (define (%normalize-process-command command)
      (cond ((string? command) command)
            ((proper-list? command)
             (if (null? command)
               (value-error "process-spawn: command list cannot be empty")
               (map %normalize-cmd-arg command)
             ) ;if
            ) ;
            (else (type-error (format #f
                                "process-spawn: command must be a string or a proper list, got: ~a"
                                (object->string command)
                              ) ;format
                  ) ;type-error
            ) ;else
      ) ;cond
    ) ;define

    (define (process-spawn command . opts)
      (let ((cwd (%keyword-value :cwd opts #f))
            (env (%keyword-value :env opts #f))
            (stdin (%keyword-value :stdin opts #f))
            (stdout (%keyword-value :stdout opts #f))
            (stderr (%keyword-value :stderr opts #f))
            (norm-cmd (%normalize-process-command command))
           ) ;
        (set! cwd (%normalize-cwd "process-spawn" cwd))
        (when env
          (unless (%valid-env? env)
            (type-error "process-spawn: :env must be a list of (key . value) string pairs")
          ) ;unless
        ) ;when
        (let ((norm-stdin (%normalize-stream-spec "process-spawn" :stdin stdin '(pipe null
                                                                                  inherit) #f)
              ) ;norm-stdin
              (norm-stdout (%normalize-stream-spec "process-spawn"
                             :stdout stdout
                             '(pipe capture discard inherit)
                             #f
                           ) ;%normalize-stream-spec
              ) ;norm-stdout
              (norm-stderr (%normalize-stream-spec "process-spawn"
                             :stderr stderr
                             '(pipe capture discard inherit stdout)
                             #f
                           ) ;%normalize-stream-spec
              ) ;norm-stderr
             ) ;
          (g_subprocess-spawn norm-cmd cwd env norm-stdin norm-stdout norm-stderr)
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

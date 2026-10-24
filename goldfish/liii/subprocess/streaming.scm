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

(define-library (liii subprocess streaming)
  (export run-streaming)
  (import (scheme base) (scheme time) (liii error) (liii subprocess process))
  (begin

    (define (%current-time-ms)
      (* 1000.0 (/ (current-jiffy) (jiffies-per-second)))
    ) ;define

    (define (%invoke-on-idle on-idle proc idle-secs)
      (let ((ar (car (arity on-idle))))
        (if (= ar 1) (on-idle idle-secs) (on-idle proc idle-secs))
      ) ;let
    ) ;define

    (define (%validate-streaming-opts! on-stdout on-stderr on-idle timeout idle-timeout)
      (when (and on-stdout (not (procedure? on-stdout)))
        (type-error "run-streaming: :on-stdout must be a procedure")
      ) ;when
      (when (and on-stderr (not (procedure? on-stderr)))
        (type-error "run-streaming: :on-stderr must be a procedure")
      ) ;when
      (when (and on-idle (not (procedure? on-idle)))
        (type-error "run-streaming: :on-idle must be a procedure")
      ) ;when
      (when (and timeout (not (real? timeout)))
        (type-error "run-streaming: :timeout must be a real number")
      ) ;when
      (when (and timeout (<= timeout 0))
        (value-error "run-streaming: :timeout must be a positive number")
      ) ;when
      (when (and idle-timeout (not (real? idle-timeout)))
        (type-error "run-streaming: :idle-timeout must be a real number")
      ) ;when
      (when (and idle-timeout (<= idle-timeout 0))
        (value-error "run-streaming: :idle-timeout must be a positive number")
      ) ;when
      (when (and timeout idle-timeout (> idle-timeout timeout))
        (value-error "run-streaming: :idle-timeout cannot be greater than :timeout")
      ) ;when
    ) ;define

    (define (%read-and-dispatch! proc stream callback)
      (let ((line (process-read-line proc :stream stream)))
        (when (and callback (not (eof-object? line)))
          (callback line)
        ) ;when
      ) ;let
    ) ;define

    (define (%drain-stream! proc stream callback)
      (when callback
        (let drain
          ()
          (let ((line (process-read-line proc :stream stream)))
            (when (not (eof-object? line))
              (callback line)
              (drain)
            ) ;when
          ) ;let
        ) ;let
      ) ;when
    ) ;define

    (define (%calc-rem-ms deadline-ms)
      (if (not deadline-ms)
        -1
        (let ((rem (inexact->exact (round (- deadline-ms (%current-time-ms))))))
          (if (<= rem 0) 0 rem)
        ) ;let
      ) ;if
    ) ;define

    (define (%calc-idle-rem-ms idle-timeout idle-secs)
      (if (not idle-timeout)
        -1
        (let ((rem-ms (inexact->exact (round (* (- idle-timeout idle-secs) 1000)))))
          (if (<= rem-ms 0) 0 rem-ms)
        ) ;let
      ) ;if
    ) ;define

    (define (%finish-stream-process! proc on-stdout on-stderr rem-ms)
      (%drain-stream! proc 'stdout on-stdout)
      (%drain-stream! proc 'stderr on-stderr)
      (process-close proc)
      (process-wait proc rem-ms)
    ) ;define

    (define (%streaming-loop proc on-stdout on-stderr on-idle idle-timeout deadline-ms)
      (let loop
        ()
        (let* ((rem-ms (%calc-rem-ms deadline-ms))
               (idle-secs (and (or idle-timeout on-idle) (g_subprocess-idle-seconds proc)))
               (idle-rem-ms (%calc-idle-rem-ms idle-timeout idle-secs))
              ) ;
          (cond
           ((or (and deadline-ms (<= rem-ms 0)) (and idle-timeout (<= idle-rem-ms 0)))
            (process-kill-tree proc)
            (process-close proc)
            (process-wait proc 500)
            #f
           ) ;
           (else
             (let* ((poll-timeout
                      (max 1
                        (min 50 (if (>= rem-ms 0) rem-ms 50) (if (>= idle-rem-ms 0) idle-rem-ms 50))
                      ) ;max
                    ) ;poll-timeout
                    (ev (g_subprocess-poll-io proc poll-timeout))
                   ) ;
               (cond ((eq? ev 'stdout) (%read-and-dispatch! proc 'stdout on-stdout) (loop))
                     ((eq? ev 'stderr) (%read-and-dispatch! proc 'stderr on-stderr) (loop))
                     ((eq? ev 'both)
                      (%read-and-dispatch! proc 'stdout on-stdout)
                      (%read-and-dispatch! proc 'stderr on-stderr)
                      (loop)
                     ) ;
                     ((eq? ev 'eof) (%finish-stream-process! proc on-stdout on-stderr rem-ms))
                     (else
                       (if (and (not (process-alive? proc))
                             (g_subprocess-eof? proc 'stdout)
                             (g_subprocess-eof? proc 'stderr)
                           ) ;and
                         (%finish-stream-process! proc on-stdout on-stderr rem-ms)
                         (begin
                           (when on-idle
                             (%invoke-on-idle on-idle proc idle-secs)
                           ) ;when
                           (loop)
                         ) ;begin
                       ) ;if
                     ) ;else
               ) ;cond
             ) ;let*
           ) ;else
          ) ;cond
        ) ;let*
      ) ;let
    ) ;define

    (define (run-streaming command . opts)
      (let ((cwd (%keyword-value :cwd opts #f))
            (env (%keyword-value :env opts #f))
            (on-stdout (%keyword-value :on-stdout opts #f))
            (on-stderr (%keyword-value :on-stderr opts #f))
            (timeout (%keyword-value :timeout opts #f))
            (idle-timeout (%keyword-value :idle-timeout opts #f))
            (on-idle (%keyword-value :on-idle opts #f))
           ) ;
        (%validate-streaming-opts! on-stdout on-stderr on-idle timeout idle-timeout)
        (let* ((capture-out? (or on-stdout idle-timeout on-idle))
               (capture-err? (or on-stderr idle-timeout on-idle))
               (proc (process-spawn command
                       :cwd    cwd
                       :env    env
                       :stdout (if capture-out? 'pipe 'inherit)
                       :stderr (if capture-err? 'pipe 'inherit)
                     ) ;process-spawn
               ) ;proc
               (timeout-ms
                 (if timeout (inexact->exact (round (* timeout 1000))) #f)
               ) ;timeout-ms
               (deadline-ms (if timeout-ms (+ (%current-time-ms) timeout-ms) #f))
              ) ;
          (if (and (not capture-out?) (not capture-err?))
            (let ((code (process-wait proc (or timeout-ms -1))))
              (process-close proc)
              code
            ) ;let
            (%streaming-loop proc on-stdout on-stderr on-idle idle-timeout deadline-ms)
          ) ;if
        ) ;let*
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

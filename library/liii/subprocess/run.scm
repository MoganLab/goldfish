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

(define-library (liii subprocess run)
  (export run run-either %run-values %symbol-keyword? %split-args)
  (import (scheme base)
    (liii either)
    (liii error)
    (liii list)
    (liii os)
    (liii path)
    (liii string)
    (liii subprocess process-error)
    (liii subprocess registry)
    (liii subprocess process)
  ) ;import
  (begin

    (define (%string-prefix? str prefix)
      (let ((len-str (string-length str)) (len-pre (string-length prefix)))
        (and (>= len-str len-pre) (string=? (substring str 0 len-pre) prefix))
      ) ;let
    ) ;define

    (define (%check-cwd-conflict command cwd)
      (when cwd
        (let ((has-cd?
                (cond ((string? command)
                       (or (%string-prefix? command "cd ") (string-contains? command " cd "))
                      ) ;
                      ((and (pair? command) (list? command))
                       (let ((first (car command)))
                         (or (and (string? first) (string=? first "cd")) (eq? first 'cd))
                       ) ;let
                      ) ;
                      (else #f)
                ) ;cond
              ) ;has-cd?
             ) ;
          (when has-cd?
            (value-error "Cannot set :cwd when command explicitly contains cd")
          ) ;when
        ) ;let
      ) ;when
    ) ;define

    (define (%command->exec-spec command)
      (cond ((string? command) command)
            ((not (proper-list? command))
             (type-error (format #f
                           "Command must be a proper list or a string, got: ~a"
                           (object->string command)
                         ) ;format
             ) ;type-error
            ) ;
            ((and (pair? command) (symbol? (car command)))
             (%resolve-symbol-command (car command) (cdr command))
            ) ;
            ((and (pair? command) (string? (car command)))
             (%resolve-symbol-command (string->symbol (car command)) (cdr command))
            ) ;
            (else
              (value-error
                (let ((suggestion
                        (if (and (pair? command) (string? (car command)))
                          (format #f "'(~a ...)" (string->symbol (car command)))
                          "'(xxx \"yyy\" \"zzz\")"
                        ) ;if
                      ) ;suggestion
                     ) ;
                  (format #f
                    "Command list must start with a symbol, e.g. ~a, got: ~a"
                    suggestion
                    (object->string command)
                  ) ;format
                ) ;let
              ) ;value-error
            ) ;else
      ) ;cond
    ) ;define

    (define (%run-values command . opts)
      (let ((cwd (%keyword-value :cwd opts #f))
            (env (%keyword-value :env opts #f))
            (input (%keyword-value :input opts #f))
            (timeout (%keyword-value :timeout opts #f))
            (stdout (%keyword-value :stdout opts #f))
            (stdout-mode (%keyword-value :stdout-mode opts #f))
            (stderr (%keyword-value :stderr opts #f))
            (stderr-mode (%keyword-value :stderr-mode opts #f))
            (stdin (%keyword-value :stdin opts #f))
            (cmd-spec (%command->exec-spec command))
            (orig-dir #f)
           ) ;
        (%check-cwd-conflict command cwd)
        (when (string? cmd-spec)
          (%check-string-command cmd-spec)
        ) ;when
        (set! cwd (%normalize-cwd "run" cwd))
        (set! stdout
          (%normalize-stream-spec "run"
            :stdout stdout
            '(capture discard inherit tee)
            'capture
          ) ;%normalize-stream-spec
        ) ;set!
        (set! stderr
          (%normalize-stream-spec "run"
            :stderr stderr
            '(capture discard inherit tee stdout)
            'capture
          ) ;%normalize-stream-spec
        ) ;set!
        (set! stdin
          (%normalize-stream-spec "run" :stdin stdin '(inherit discard pipe null) #f)
        ) ;set!
        (unless (%valid-env? env)
          (type-error "run-values: :env must be a list of (key . value) string pairs")
        ) ;unless
        (when cwd
          (set! orig-dir (getcwd))
          (chdir cwd)
        ) ;when
        (let-values (((out err code)
                      (cond
                       ((and (pair? cmd-spec) (eq? (car cmd-spec) 'lambda))
                        (apply (cadr cmd-spec) (cddr cmd-spec))
                        (values "" "" 0)
                       ) ;
                       ((pair? cmd-spec)
                        (g_subprocess-run-values cmd-spec cwd env input timeout
                          stdout stdout-mode stderr stderr-mode stdin
                        ) ;g_subprocess-run-values
                       ) ;
                       (else (g_subprocess-run-values cmd-spec cwd env input
                               timeout stdout stdout-mode stderr stderr-mode
                               stdin
                             ) ;g_subprocess-run-values
                       ) ;else
                      ) ;cond
                     ) ;
                    ) ;
          (when orig-dir
            (chdir orig-dir)
          ) ;when
          (values out err code)
        ) ;let-values
      ) ;let
    ) ;define

    (define (run command . opts)
      (let ((cwd (%keyword-value :cwd opts #f))
            (env (%keyword-value :env opts #f))
            (input (%keyword-value :input opts #f))
            (timeout (%keyword-value :timeout opts #f))
            (stdout (%keyword-value :stdout opts #f))
            (stderr (%keyword-value :stderr opts #f))
            (stdin (%keyword-value :stdin opts #f))
            (cmd-spec (%command->exec-spec command))
            (orig-dir #f)
           ) ;
        (%check-cwd-conflict command cwd)
        (when (string? cmd-spec)
          (%check-string-command cmd-spec)
        ) ;when
        (when cwd
          (set! orig-dir (getcwd))
          (chdir cwd)
        ) ;when
        (let ((result
                (cond
                 ((and (pair? cmd-spec) (eq? (car cmd-spec) 'lambda))
                  (apply (cadr cmd-spec) (cddr cmd-spec))
                  0
                 ) ;
                 ((or (pair? cmd-spec) env input timeout stdout stderr stdin)
                  (let-values (((out err code)
                                (%run-values command
                                  :cwd     cwd
                                  :env     env
                                  :input   input
                                  :timeout timeout
                                  :stdout  (or stdout 'inherit)
                                  :stderr  (or stderr 'inherit)
                                  :stdin   stdin
                                ) ;%run-values
                               ) ;
                              ) ;
                    code
                  ) ;let-values
                 ) ;
                 (else (os-call cmd-spec))
                ) ;cond
              ) ;result
             ) ;
          (when orig-dir
            (chdir orig-dir)
          ) ;when
          result
        ) ;let
      ) ;let
    ) ;define

    (define (run-either command . opts)
      (let-values (((out err code) (apply %run-values command opts)))
        (if (zero? code)
          (from-right out)
          (from-left (make-process-error code err out command))
        ) ;if
      ) ;let-values
    ) ;define

    (define (%symbol-keyword? sym)
      (and (symbol? sym)
        (let ((s (symbol->string sym)))
          (and (> (string-length s) 0) (char=? (string-ref s 0) #\:))
        ) ;let
      ) ;and
    ) ;define

    (define (%split-args args)
      (let loop
        ((rev (reverse args)) (opts '()))
        (cond ((null? rev) (values '() opts))
              ((and (pair? (cdr rev)) (%symbol-keyword? (cadr rev)))
               (loop (cddr rev) (append (list (cadr rev) (car rev)) opts))
              ) ;
              (else (values (reverse rev) opts))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

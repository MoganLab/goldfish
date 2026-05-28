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

(define-library (liii subprocess)
  (export run
    run-string
    run-values
    run-set!
    run-get
    run-allow!
    run-ban!
    run-and
    run-or
    run-sequence
    run-pipe
    run-if
    run-when
  ) ;export
  (import (scheme base)
    (liii base)
    (liii either)
    (liii error)
    (liii hash-table)
    (liii list)
    (liii os)
    (liii path)
    (liii string)
  ) ;import
  (begin

    (define %run-registry (make-hash-table))
    (define %run-allow-list '())
    (define %run-ban-list '())

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

    (define (%check-symbol-command sym)
      (when (memq sym %run-ban-list)
        (value-error (string-append "Symbol command '" (symbol->string sym) "' has been banned")
        ) ;value-error
      ) ;when
      (when (and (not (null? %run-allow-list)) (not (memq sym %run-allow-list)))
        (value-error (string-append "Symbol command '"
                       (symbol->string sym)
                       "' is not in the allow list"
                     ) ;string-append
        ) ;value-error
      ) ;when
    ) ;define

    (define (%resolve-symbol-command sym args)
      (%check-symbol-command sym)
      (let ((val (hash-table-ref/default %run-registry sym #f)))
        (cond ((procedure? val) (cons 'lambda (cons val args)))
              ((string? val) (cons val args))
              ((path? val) (cons (path->string val) args))
              (else (string-join (cons (symbol->string sym) args) " "))
        ) ;cond
      ) ;let
    ) ;define

    (define (%string-prefix? str prefix)
      (let ((len-str (string-length str)) (len-pre (string-length prefix)))
        (and (>= len-str len-pre) (string=? (substring str 0 len-pre) prefix))
      ) ;let
    ) ;define

    (define (%check-cwd-conflict command cwd)
      (when cwd
        (let ((has-cd? (cond ((string? command)
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
            ((and (pair? command) (symbol? (car command)))
             (%resolve-symbol-command (car command) (cdr command))
            ) ;
            ((and (list? command) (every string? command)) command)
            (else (type-error "(run command ...): command must be string or list of strings")
            ) ;else
      ) ;cond
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
        (when cwd
          (set! orig-dir (getcwd))
          (chdir cwd)
        ) ;when
        (let ((result
               (cond ((and (pair? cmd-spec) (eq? (car cmd-spec) 'lambda))
                      (apply (cadr cmd-spec) (cddr cmd-spec))
                      0
                     ) ;lambda
                     ((or env input timeout stdout stderr stdin)
                      (let-values (((out err code)
                                    (run-values command
                                      :cwd cwd :env env
                                      :input input :timeout timeout
                                      :stdout stdout :stderr stderr
                                      :stdin stdin
                                    ) ;run-values
                                   ) ;
                                  ) ;let-values
                        (display out)
                        (display err (current-error-port))
                        code
                      ) ;let-values
                     ) ;has-keywords
                     ((pair? cmd-spec)
                      (let-values (((out err code) (run-values command)))
                        (display out)
                        (display err (current-error-port))
                        code
                      ) ;let-values
                     ) ;list-form
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

    (define (run-set! symbol value)
      (hash-table-set! %run-registry symbol value)
    ) ;define

    (define (run-get symbol)
      (let ((val (hash-table-ref/default %run-registry symbol #f)))
        (cond ((not val) #f)
              ((string? val) (path val))
              (else val)
        ) ;cond
      ) ;let
    ) ;define

    (define (run-allow! symbol-or-list)
      (set! %run-allow-list
        (cond ((null? symbol-or-list) '())
              ((symbol? symbol-or-list) (list symbol-or-list))
              ((list? symbol-or-list) symbol-or-list)
              (else (type-error "(run-allow! symbol-or-list): must be symbol or list of symbols")
              ) ;else
        ) ;cond
      ) ;set!
    ) ;define

    (define (run-ban! symbol)
      (set! %run-ban-list (cons symbol %run-ban-list))
    ) ;define

    (define (run-values command . opts)
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
        (when cwd
          (set! orig-dir (getcwd))
          (chdir cwd)
        ) ;when
        (let-values (((out err code)
                      (cond ((and (pair? cmd-spec) (eq? (car cmd-spec) 'lambda))
                             (apply (cadr cmd-spec) (cddr cmd-spec))
                             (values "" "" 0)
                            ) ;lambda
                            ((pair? cmd-spec)
                             (g_subprocess-run-values cmd-spec
                               cwd
                               env
                               input
                               timeout
                               stdout
                               stderr
                               stdin
                             ) ;g_subprocess-run-values
                            ) ;list
                            (else
                             (g_subprocess-run-values cmd-spec
                               cwd
                               env
                               input
                               timeout
                               stdout
                               stderr
                               stdin
                             ) ;g_subprocess-run-values
                            ) ;string
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

    (define (run-string command . opts)
      (let-values (((out err code) (apply run-values command opts)))
        (if (zero? code)
          (from-right out)
          (from-left code)
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

    (define (run-and . args)
      (let-values (((commands opts) (%split-args args)))
        (let ((cwd (%keyword-value :cwd opts #f))
              (env (%keyword-value :env opts #f))
              (timeout (%keyword-value :timeout opts #f))
              (input (%keyword-value :input opts #f))
              (stdin (%keyword-value :stdin opts #f))
              (stdout (%keyword-value :stdout opts #f))
              (stderr (%keyword-value :stderr opts #f))
             ) ;
          (let loop
            ((cmds commands) (first? #t))
            (cond ((null? cmds) (from-right 0))
                  ((null? (cdr cmds))
                   (let-values (((out err code)
                                 (run-values (car cmds)
                                   :cwd
                                   cwd
                                   :env
                                   env
                                   :timeout
                                   timeout
                                   :input
                                   (if first? input #f)
                                   :stdin
                                   (if first? stdin #f)
                                   :stdout
                                   stdout
                                   :stderr
                                   stderr
                                 ) ;run-values
                                ) ;
                               ) ;
                     (if (zero? code)
                       (from-right code)
                       (from-left (list code (car cmds)))
                     ) ;if
                   ) ;let-values
                  ) ;
                  (else (let-values (((out err code)
                                      (run-values (car cmds)
                                        :cwd
                                        cwd
                                        :env
                                        env
                                        :timeout
                                        timeout
                                        :input
                                        (if first? input #f)
                                        :stdin
                                        (if first? stdin #f)
                                      ) ;run-values
                                     ) ;
                                    ) ;
                          (if (zero? code)
                            (loop (cdr cmds) #f)
                            (from-left (list code (car cmds)))
                          ) ;if
                        ) ;let-values
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;let-values
    ) ;define

    (define (run-or . args)
      (let-values (((commands opts) (%split-args args)))
        (let ((cwd (%keyword-value :cwd opts #f))
              (env (%keyword-value :env opts #f))
              (timeout (%keyword-value :timeout opts #f))
             ) ;
          (let loop
            ((cmds commands))
            (cond ((null? cmds) (from-right 0))
                  ((null? (cdr cmds))
                   (let-values (((out err code) (run-values (car cmds) :cwd cwd :env env :timeout timeout)))
                     (if (zero? code)
                       (from-right code)
                       (from-left (list code (car cmds)))
                     ) ;if
                   ) ;let-values
                  ) ;
                  (else (let-values (((out err code) (run-values (car cmds) :cwd cwd :env env :timeout timeout)))
                          (if (zero? code)
                            (from-right 0)
                            (loop (cdr cmds))
                          ) ;if
                        ) ;let-values
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;let-values
    ) ;define

    (define (run-sequence . args)
      (let-values (((commands opts) (%split-args args)))
        (let ((cwd (%keyword-value :cwd opts #f))
              (env (%keyword-value :env opts #f))
              (timeout (%keyword-value :timeout opts #f))
             ) ;
          (let loop
            ((cmds commands) (last-code 0))
            (cond ((null? cmds)
                   (if (zero? last-code)
                     (from-right last-code)
                     (from-left last-code)
                   ) ;if
                  ) ;
                  (else (let-values (((out err code) (run-values (car cmds) :cwd cwd :env env :timeout timeout)))
                          (loop (cdr cmds) code)
                        ) ;let-values
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;let-values
    ) ;define

    (define (run-pipe . args)
      (let-values (((commands opts) (%split-args args)))
        (let ((cwd (%keyword-value :cwd opts #f))
              (env (%keyword-value :env opts #f))
              (timeout (%keyword-value :timeout opts #f))
              (stdin (%keyword-value :stdin opts #f))
             ) ;
          (let loop
            ((cmds commands) (input #f))
            (cond ((null? cmds) (from-right ""))
                  ((null? (cdr cmds))
                   (let-values (((out err code)
                                 (run-values (car cmds)
                                   :cwd
                                   cwd
                                   :env
                                   env
                                   :timeout
                                   timeout
                                   :input
                                   input
                                   :stdin
                                   stdin
                                 ) ;run-values
                                ) ;
                               ) ;
                     (if (zero? code)
                       (from-right out)
                       (from-left (list code (car cmds)))
                     ) ;if
                   ) ;let-values
                  ) ;
                  (else (let-values (((out err code)
                                      (run-values (car cmds)
                                        :cwd
                                        cwd
                                        :env
                                        env
                                        :timeout
                                        timeout
                                        :input
                                        input
                                        :stdin
                                        stdin
                                      ) ;run-values
                                     ) ;
                                    ) ;
                          (if (zero? code)
                            (loop (cdr cmds) out)
                            (from-left (list code (car cmds)))
                          ) ;if
                        ) ;let-values
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;let-values
    ) ;define

    (define (run-if condition then-cmd . else-cmds)
      (let-values (((out err code) (run-values condition)))
        (if (zero? code)
          (let-values (((out err code) (run-values then-cmd)))
            (if (zero? code)
              (from-right code)
              (from-left (list code then-cmd))
            ) ;if
          ) ;let-values
          (if (null? else-cmds)
            (from-right code)
            (let-values (((out err code) (run-values (car else-cmds))))
              (if (zero? code)
                (from-right code)
                (from-left (list code (car else-cmds)))
              ) ;if
            ) ;let-values
          ) ;if
        ) ;if
      ) ;let-values
    ) ;define

    (define (run-when condition command)
      (let-values (((out err code) (run-values condition)))
        (if (zero? code)
          (from-right 0)
          (let-values (((out err code) (run-values command)))
            (if (zero? code)
              (from-right code)
              (from-left (list code command))
            ) ;if
          ) ;let-values
        ) ;if
      ) ;let-values
    ) ;define

  ) ;begin
) ;define-library

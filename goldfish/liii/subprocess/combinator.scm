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

(define-library (liii subprocess combinator)
  (export run-and run-or run-sequence run-pipe run-if run-when)
  (import (scheme base)
    (liii either)
    (liii subprocess process)
    (liii subprocess run)
  ) ;import
  (begin

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
                   (let ((res (run-either (car cmds)
                                :cwd     cwd
                                :env     env
                                :timeout timeout
                                :input   (if first? input #f)
                                :stdin   (if first? stdin #f)
                                :stdout  (or stdout 'inherit)
                                :stderr  (or stderr 'inherit)
                              ) ;run-either
                         ) ;res
                        ) ;
                     (either-map (lambda (_) 0) res)
                   ) ;let
                  ) ;
                  (else
                    (let ((res (run-either (car cmds)
                                 :cwd     cwd
                                 :env     env
                                 :timeout timeout
                                 :input   (if first? input #f)
                                 :stdin   (if first? stdin #f)
                                 :stdout  'inherit
                                 :stderr  'inherit
                               ) ;run-either
                          ) ;res
                         ) ;
                      (if (either-right? res) (loop (cdr cmds) #f) res)
                    ) ;let
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
                  (else
                    (let ((res (run-either (car cmds)
                                 :cwd     cwd
                                 :env     env
                                 :timeout timeout
                                 :stdout  'inherit
                                 :stderr  'inherit
                               ) ;run-either
                          ) ;res
                         ) ;
                      (cond ((either-right? res) (from-right 0))
                            ((null? (cdr cmds)) res)
                            (else (loop (cdr cmds)))
                      ) ;cond
                    ) ;let
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
            ((cmds commands) (last-res (from-right 0)))
            (if (null? cmds)
              last-res
              (let ((res (run-either (car cmds)
                           :cwd     cwd
                           :env     env
                           :timeout timeout
                           :stdout  'inherit
                           :stderr  'inherit
                         ) ;run-either
                    ) ;res
                   ) ;
                (loop (cdr cmds) (either-map (lambda (_) 0) res))
              ) ;let
            ) ;if
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
                   (run-either (car cmds)
                     :cwd     cwd
                     :env     env
                     :timeout timeout
                     :input   input
                     :stdin   stdin
                     :stdout  'capture
                   ) ;run-either
                  ) ;
                  (else
                    (let ((res (run-either (car cmds)
                                 :cwd     cwd
                                 :env     env
                                 :timeout timeout
                                 :input   input
                                 :stdin   stdin
                                 :stdout  'capture
                               ) ;run-either
                          ) ;res
                         ) ;
                      (if (either-right? res) (loop (cdr cmds) (to-right res)) res)
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;let-values
    ) ;define

    (define (run-if condition then-cmd else-cmd)
      (let ((chosen-cmd (if (zero? (run condition :stdout 'inherit :stderr 'inherit)) then-cmd else-cmd)
            ) ;chosen-cmd
           ) ;
        (let ((res (run-either chosen-cmd :stdout 'inherit :stderr 'inherit)))
          (either-map (lambda (_) 0) res)
        ) ;let
      ) ;let
    ) ;define

    (define (run-when condition command)
      (if (zero? (run condition :stdout 'inherit :stderr 'inherit))
        (from-right 0)
        (let ((res (run-either command :stdout 'inherit :stderr 'inherit)))
          (either-map (lambda (_) 0) res)
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library

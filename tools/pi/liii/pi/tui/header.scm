;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
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

(define-library (liii pi tui header)
  (export make-pi-header pi-header?)
  (import (scheme base) (liii base) (liii string) (liii tui component))
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

    (define (pi-header? obj)
      (and (component? obj)
        (let ((d (component-data obj)))
          (and (pair? d) (eq? (car d) 'pi-header))
        ) ;let
      ) ;and
    ) ;define

    (define (%render-header version width)
      (let* ((line0 "")
             (line1 (string-append " \x1b;[1;38;2;138;190;183mpi\x1b;[0m\x1b;[38;2;102;102;102m v"
                      version
                      "\x1b;[0m"
                    ) ;string-append
             ) ;line1
             (dot "\x1b;[38;2;128;128;128m · \x1b;[0m")
             (k-esc "\x1b;[38;2;102;102;102mescape\x1b;[0m\x1b;[38;2;128;128;128m interrupt\x1b;[0m"
             ) ;k-esc
             (k-exit "\x1b;[38;2;102;102;102mctrl+c/ctrl+d\x1b;[0m\x1b;[38;2;128;128;128m clear/exit\x1b;[0m"
             ) ;k-exit
             (k-cmd "\x1b;[38;2;102;102;102m/\x1b;[0m\x1b;[38;2;128;128;128m commands\x1b;[0m"
             ) ;k-cmd
             (k-bash "\x1b;[38;2;102;102;102m!\x1b;[0m\x1b;[38;2;128;128;128m bash\x1b;[0m")
             (k-more "\x1b;[38;2;102;102;102mctrl+o\x1b;[0m\x1b;[38;2;128;128;128m more\x1b;[0m"
             ) ;k-more
             (line2 (string-append " " k-esc dot k-exit dot k-cmd dot k-bash dot k-more))
             (line3 " \x1b;[38;2;102;102;102mPress ctrl+o to show full startup help and loaded resources.\x1b;[0m"
             ) ;line3
             (line4 "")
             (line5 " \x1b;[38;2;102;102;102mPi can explain its own features and look up its docs. Ask it how to use or extend Pi.\x1b;[0m"
             ) ;line5
             (line6 "")
            ) ;
        (list line0 line1 line2 line3 line4 line5 line6)
      ) ;let*
    ) ;define

    (define (make-pi-header . args)
      (let* ((version (%keyword-value :version args "0.85.1"))
             (comp (make-component :render (lambda (self width) (%render-header version width)))
             ) ;comp
            ) ;
        (component-data-set! comp (list 'pi-header version))
        comp
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

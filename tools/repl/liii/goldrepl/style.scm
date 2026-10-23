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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldrepl style)
  (import (scheme base)
    (scheme char)
    (liii base)
    (liii list)
    (liii string)
    (liii goldrepl complete)
  ) ;import
  (export *repl-styles* %repl-style %repl-styled %repl-gray *repl-keywords*
    %repl-highlight-line
  ) ;export
  (begin

    (define *repl-styles*
      '(("keyword" . "\x1b;[35m")
        ("symbol" . "\x1b;[36m")
        ("number" . "\x1b;[33m")
        ("constant" . "\x1b;[34m")
        ("string" . "\x1b;[32m")
        ("comment" . "\x1b;[90m")
        ("prompt" . "\x1b;[33m")
        ("brand" . "\x1b;[1;33m")
        ("version" . "\x1b;[1;35m")
        ("dim" . "\x1b;[2m")
        ("italic" . "\x1b;[3m")
        ("bold" . "\x1b;[1m")
        ("error" . "\x1b;[31m")
        ("gray" . "\x1b;[90m")
        ("reset" . "\x1b;[0m"))
    ) ;define

    (define (%repl-style name)
      (let ((p (assoc name *repl-styles*)))
        (if p (cdr p) "")
      ) ;let
    ) ;define

    (define (%repl-styled name text)
      (let ((style (%repl-style name)))
        (if (string-null? style) text (string-append style text "\x1b;[0m"))
      ) ;let
    ) ;define

    (define (%repl-gray text)
      (%repl-styled "gray" text)
    ) ;define

    (define *repl-keywords*
      '("define" "lambda" "if" "else" "let" "let*" "letrec" "begin" "quote"
        "set!" "cond" "case" "and" "or" "do" "delay" "quasiquote" "unquote"
        "unquote-splicing")
    ) ;define

    (define (%repl-highlight-string chars n i loop-k)
      (let scan
        ((j (+ i 1)))
        (cond
         ((>= j n) (loop-k n (%repl-styled "string" (list->string (drop chars i)))))
         ((char=? (list-ref chars j) #\\) (scan (+ j 2)))
         ((char=? (list-ref chars j) #\")
          (let ((s (list->string (take (drop chars i) (+ 1 (- j i))))))
            (loop-k (+ j 1) (%repl-styled "string" s))
          ) ;let
         ) ;
         (else (scan (+ j 1)))
        ) ;cond
      ) ;let
    ) ;define
    (define (%repl-highlight-token chars n i loop-k)
      (let scan
        ((j i))
        (if (and (< j n) (%repl-symbol-char? (list-ref chars j)))
          (scan (+ j 1))
          (let ((token (list->string (take (drop chars i) (- j i)))))
            (loop-k j
              (if (member token *repl-keywords*) (%repl-styled "keyword" token) token)
            ) ;loop-k
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (%repl-highlight-number chars n i loop-k)
      (let scan
        ((j i))
        (if (and (< j n) (char-numeric? (list-ref chars j)))
          (scan (+ j 1))
          (let ((num (list->string (take (drop chars i) (- j i)))))
            (loop-k j (%repl-styled "number" num))
          ) ;let
        ) ;if
      ) ;let
    ) ;define
    (define (%repl-highlight-line line)
      (let ((chars (string->list line)) (n (string-length line)))
        (let loop
          ((i 0) (acc '()))
          (cond ((>= i n) (apply string-append (reverse acc)))
                ((char=? (list-ref chars i) #\;)
                 (apply string-append
                   (reverse
                     (cons (%repl-styled "comment" (list->string (drop chars i))) acc)
                   ) ;reverse
                 ) ;apply
                ) ;
                ((char=? (list-ref chars i) #\")
                 (%repl-highlight-string chars
                   n
                   i
                   (lambda (next-i piece) (loop next-i (cons piece acc)))
                 ) ;%repl-highlight-string
                ) ;
                ((and (char=? (list-ref chars i) #\#)
                   (< (+ i 1) n)
                   (let ((c2 (list-ref chars (+ i 1))))
                     (or (char=? c2 #\t) (char=? c2 #\f))
                   ) ;let
                 ) ;and
                 (loop (+ i 2)
                   (cons
                     (%repl-styled "constant" (list->string (take (drop chars i) 2)))
                     acc
                   ) ;cons
                 ) ;loop
                ) ;
                ((char-numeric? (list-ref chars i))
                 (%repl-highlight-number chars
                   n
                   i
                   (lambda (next-i piece) (loop next-i (cons piece acc)))
                 ) ;%repl-highlight-number
                ) ;
                ((%repl-symbol-char? (list-ref chars i))
                 (%repl-highlight-token chars
                   n
                   i
                   (lambda (next-i piece) (loop next-i (cons piece acc)))
                 ) ;%repl-highlight-token
                ) ;
                (else
                  (loop (+ i 1) (cons (string (list-ref chars i)) acc))
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library

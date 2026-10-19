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

(define-library (liii pi compact serialize)
  (import (scheme base) (scheme write) (liii string) (liii pi message))
  (export pi-truncate-for-summary pi-serialize-conversation)
  (begin

    (define (%ref obj key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (or (assoc str-key obj) (assoc sym-key obj)))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    (define (%normalize-message msg)
      (if (pi-message? msg) (pi-message->alist msg) msg)
    ) ;define

    (define (%blocks content)
      (cond ((vector? content) (vector->list content))
            ((null? content) '())
            ((pair? content) content)
            (else '())
      ) ;cond
    ) ;define

    (define (%block-text blk)
      (if (pi-block? blk) (pi-block->alist blk) blk)
    ) ;define

    (define (%content-text content)
      (cond ((string? content) content)
            (else
              (let loop
                ((rest (%blocks content)) (acc '()))
                (if (null? rest)
                  (string-join (reverse acc) "")
                  (let* ((blk (%block-text (car rest))) (text (%ref blk "text")))
                    (loop (cdr rest)
                      (if (and (equal? (%ref blk "type") "text") (string? text) (not (string-null? text)))
                        (cons text acc)
                        acc
                      ) ;if
                    ) ;loop
                  ) ;let*
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    (define (%json-value v)
      (cond ((string? v) (string-append "\"" v "\""))
            ((number? v) (number->string v))
            ((eq? v #t) "true")
            ((eq? v #f) "false")
            (else (object->string v))
      ) ;cond
    ) ;define

    (define (%format-args args)
      (if (not (pair? args))
        ""
        (let ((pairs
                (if
                  (and (pair? (car args)) (or (string? (caar args)) (symbol? (caar args))))
                  args
                  '()
                ) ;if
              ) ;pairs
             ) ;
          (string-join
            (map
              (lambda (p)
                (string-append (if (symbol? (car p)) (symbol->string (car p)) (car p))
                  "="
                  (%json-value (cdr p))
                ) ;string-append
              ) ;lambda
              pairs
            ) ;map
            ", "
          ) ;string-join
        ) ;let
      ) ;if
    ) ;define

    (define (%assistant-tool-calls content)
      (let loop
        ((rest (%blocks content)) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let ((blk (%block-text (car rest))))
            (if (equal? (%ref blk "type") "toolCall")
              (loop (cdr rest)
                (cons (string-append (%ref blk "name") "(" (%format-args (%ref blk "arguments")) ")")
                  acc
                ) ;cons
              ) ;loop
              (loop (cdr rest) acc)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%has-text-block? content)
      (let loop
        ((rest (%blocks content)))
        (cond ((null? rest) #f)
              ((and
                 (equal? (%ref (%block-text (car rest)) "type") "text")
                 (string? (%ref (%block-text (car rest)) "text"))
               ) ;and
               #t
              ) ;
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-truncate-for-summary text max-chars)
      (if (<= (string-length text) max-chars)
        text
        (string-append (substring text 0 max-chars)
          "\n\n[... "
          (number->string (- (string-length text) max-chars))
          " more characters truncated]"
        ) ;string-append
      ) ;if
    ) ;define

    (define (%assistant-parts content text)
      (let ((calls (%assistant-tool-calls content)))
        (append
          (if (and (%has-text-block? content) (not (string-null? text)))
            (list (string-append "[Assistant]: " text))
            '()
          ) ;if
          (if (pair? calls)
            (list (string-append "[Assistant tool calls]: " (string-join calls "; ")))
            '()
          ) ;if
        ) ;append
      ) ;let
    ) ;define

    (define (%message-parts msg)
      (let* ((msg (%normalize-message msg))
             (role (%ref msg "role"))
             (content (%ref msg "content"))
             (text (%content-text content))
            ) ;
        (cond
         ((equal? role "user")
          (if (string-null? text) '() (list (string-append "[User]: " text)))
         ) ;
         ((equal? role "assistant") (%assistant-parts content text))
         ((equal? role "toolResult")
          (if (string-null? text)
            '()
            (list (string-append "[Tool result]: " (pi-truncate-for-summary text 2000)))
          ) ;if
         ) ;
         (else '())
        ) ;cond
      ) ;let*
    ) ;define

    (define (pi-serialize-conversation msgs)
      (let loop
        ((rest msgs) (parts '()))
        (if (null? rest)
          (string-join (reverse parts) "\n\n")
          (loop (cdr rest) (append (reverse (%message-parts (car rest))) parts))
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

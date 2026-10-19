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

(define-library (liii pi compact fileops)
  (import (scheme base) (liii string) (liii list) (liii sort) (liii pi message))
  (export pi-extract-file-ops pi-compute-file-lists pi-format-file-operations)
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
            ((pair? content) content)
            (else '())
      ) ;cond
    ) ;define

    (define (%block-alist blk)
      (if (pi-block? blk) (pi-block->alist blk) blk)
    ) ;define

    (define (%add-unique lst x)
      (if (member x lst) lst (append lst (list x)))
    ) ;define

    (define (%tool-paths msg)
      (let loop
        ((rest (%blocks (%ref msg "content"))) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((blk (%block-alist (car rest)))
                 (name (%ref blk "name"))
                 (path (%ref (%ref blk "arguments") "path"))
                ) ;
            (loop (cdr rest)
              (if (and (equal? (%ref blk "type") "toolCall")
                    (string? path)
                    (member name '("read" "write" "edit"))
                  ) ;and
                (cons (cons name path) acc)
                acc
              ) ;if
            ) ;loop
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (pi-extract-file-ops msgs)
      (let loop
        ((rest msgs) (read '()) (written '()) (edited '()))
        (if (null? rest)
          (list (cons 'read read) (cons 'written written) (cons 'edited edited))
          (let ((msg (%normalize-message (car rest))))
            (if (equal? (%ref msg "role") "assistant")
              (let ops-loop
                ((ops (%tool-paths msg)) (read read) (written written) (edited edited))
                (if (null? ops)
                  (loop (cdr rest) read written edited)
                  (let ((name (caar ops)) (path (cdar ops)))
                    (cond ((equal? name "read")
                           (ops-loop (cdr ops) (%add-unique read path) written edited)
                          ) ;
                          ((equal? name "write")
                           (ops-loop (cdr ops) read (%add-unique written path) edited)
                          ) ;
                          (else (ops-loop (cdr ops) read written (%add-unique edited path)))
                    ) ;cond
                  ) ;let
                ) ;if
              ) ;let
              (loop (cdr rest) read written edited)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (pi-compute-file-lists file-ops)
      (let* ((read (%ref file-ops "read"))
             (written (%ref file-ops "written"))
             (edited (%ref file-ops "edited"))
             (modified
               (let loop
                 ((rest written) (acc edited))
                 (if (null? rest) acc (loop (cdr rest) (%add-unique acc (car rest))))
               ) ;let
             ) ;modified
             (read-only
               (list-sort string<? (filter (lambda (p) (not (member p modified))) read))
             ) ;read-only
            ) ;
        (list (cons 'read-files read-only)
          (cons 'modified-files (list-sort string<? modified))
        ) ;list
      ) ;let*
    ) ;define

    (define (%section tag files)
      (if (null? files)
        ""
        (string-append "<" tag ">\n" (string-join files "\n") "\n</" tag ">")
      ) ;if
    ) ;define

    (define (pi-format-file-operations read-files modified-files)
      (string-join
        (filter (lambda (s) (not (string-null? s)))
          (list (%section "read-files" read-files)
            (%section "modified-files" modified-files)
          ) ;list
        ) ;filter
        "\n\n"
      ) ;string-join
    ) ;define

  ) ;begin
) ;define-library

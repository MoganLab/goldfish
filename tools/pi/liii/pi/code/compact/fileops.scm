;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code compact fileops)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii sort)
    (liii pi ai message)
  ) ;import
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
      (map
        (lambda (blk) (cons (%ref blk "name") (%ref (%ref blk "arguments") "path")))
        (filter
          (lambda (blk)
            (and (equal? (%ref blk "type") "toolCall")
              (string? (%ref (%ref blk "arguments") "path"))
              (member (%ref blk "name") '("read" "write" "edit"))
            ) ;and
          ) ;lambda
          (map %block-alist (%blocks (%ref msg "content")))
        ) ;filter
      ) ;map
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
             (modified (fold (lambda (p acc) (%add-unique acc p)) edited written))
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

;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core compaction fileops)
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
      (let* ((ops
               (append-map
                 (lambda (msg)
                   (let ((normalized (%normalize-message msg)))
                     (if (equal? (%ref normalized "role") "assistant") (%tool-paths normalized) '())
                   ) ;let
                 ) ;lambda
                 msgs
               ) ;append-map
             ) ;ops
             (groups
               (fold
                 (lambda (op groups)
                   (let ((name (car op))
                         (path (cdr op))
                         (read (car groups))
                         (written (cadr groups))
                         (edited (caddr groups))
                        ) ;
                     (cond ((equal? name "read") (list (%add-unique read path) written edited))
                           ((equal? name "write") (list read (%add-unique written path) edited))
                           (else (list read written (%add-unique edited path)))
                     ) ;cond
                   ) ;let
                 ) ;lambda
                 '(() () ())
                 ops
               ) ;fold
             ) ;groups
            ) ;
        (list (cons 'read (car groups))
          (cons 'written (cadr groups))
          (cons 'edited (caddr groups))
        ) ;list
      ) ;let*
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

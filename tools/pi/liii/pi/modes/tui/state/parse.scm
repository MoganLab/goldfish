;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui state parse)
  (import (scheme base) (liii pi modes tui slash-menu))
  (export %parse-state-args)
  (begin

    (define (%parse-state-args args)
      (let loop
        ((rest args)
         (status 'idle)
         (messages '())
         (input-editor "")
         (active-tool #f)
         (scroll-offset 0)
         (dirty? #t)
         (cols 80)
         (rows 24)
         (slash-menu #f)
        ) ;
        (cond
         ((null? rest)
          (values status
            messages
            input-editor
            active-tool
            scroll-offset
            dirty?
            cols
            rows
            (or slash-menu (make-pi-tui-slash-menu))
          ) ;values
         ) ;
         ((eq? (car rest) :status)
          (loop (cddr rest)
            (cadr rest)
            messages
            input-editor
            active-tool
            scroll-offset
            dirty?
            cols
            rows
            slash-menu
          ) ;loop
         ) ;
         ((eq? (car rest) :messages)
          (loop (cddr rest)
            status
            (cadr rest)
            input-editor
            active-tool
            scroll-offset
            dirty?
            cols
            rows
            slash-menu
          ) ;loop
         ) ;
         ((eq? (car rest) :input-editor)
          (loop (cddr rest)
            status
            messages
            (cadr rest)
            active-tool
            scroll-offset
            dirty?
            cols
            rows
            slash-menu
          ) ;loop
         ) ;
         ((eq? (car rest) :active-tool)
          (loop (cddr rest)
            status
            messages
            input-editor
            (cadr rest)
            scroll-offset
            dirty?
            cols
            rows
            slash-menu
          ) ;loop
         ) ;
         ((eq? (car rest) :scroll-offset)
          (loop (cddr rest)
            status
            messages
            input-editor
            active-tool
            (cadr rest)
            dirty?
            cols
            rows
            slash-menu
          ) ;loop
         ) ;
         ((or (eq? (car rest) :dirty?) (eq? (car rest) :dirty))
          (loop (cddr rest)
            status
            messages
            input-editor
            active-tool
            scroll-offset
            (cadr rest)
            cols
            rows
            slash-menu
          ) ;loop
         ) ;
         ((eq? (car rest) :cols)
          (loop (cddr rest)
            status
            messages
            input-editor
            active-tool
            scroll-offset
            dirty?
            (cadr rest)
            rows
            slash-menu
          ) ;loop
         ) ;
         ((eq? (car rest) :rows)
          (loop (cddr rest)
            status
            messages
            input-editor
            active-tool
            scroll-offset
            dirty?
            cols
            (cadr rest)
            slash-menu
          ) ;loop
         ) ;
         ((eq? (car rest) :slash-menu)
          (loop (cddr rest)
            status
            messages
            input-editor
            active-tool
            scroll-offset
            dirty?
            cols
            rows
            (cadr rest)
          ) ;loop
         ) ;
         (else
           (let ((len (length rest)))
             (values (if (>= len 1) (list-ref rest 0) status)
               (if (>= len 2) (list-ref rest 1) messages)
               (if (>= len 3) (list-ref rest 2) input-editor)
               (if (>= len 4) (list-ref rest 3) active-tool)
               (if (>= len 5) (list-ref rest 4) scroll-offset)
               (if (>= len 6) (list-ref rest 5) dirty?)
               (if (>= len 7) (list-ref rest 6) cols)
               (if (>= len 8) (list-ref rest 7) rows)
               (if (>= len 9) (list-ref rest 8) (or slash-menu (make-pi-tui-slash-menu)))
             ) ;values
           ) ;let
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

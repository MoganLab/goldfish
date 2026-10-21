;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui confirm detect)
  (import (scheme base)
    (liii error)
    (liii string)
    (liii pi modes tui confirm types)
  ) ;import
  (export tool-dangerous? classify-tool-danger extract-target-summary
    extract-diff-preview make-confirm-tool-call-interceptor
  ) ;export
  (begin

    (define (%get-arg-val args key)
      (let loop
        ((rest args))
        (cond ((null? rest) #f)
              ((not (pair? (car rest))) (loop (cdr rest)))
              (else
                (let ((k (caar rest)) (v (cdar rest)))
                  (if
                    (or (and (string? k) (string=? k key))
                      (and (symbol? k) (string=? (symbol->string k) key))
                    ) ;or
                    v
                    (loop (cdr rest))
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%validate-detect-args tool-name args caller)
      (unless (string? tool-name)
        (type-error (string-append caller ": expected string for tool-name"))
      ) ;unless
      (unless (list? args)
        (type-error (string-append caller ": expected list for args"))
      ) ;unless
    ) ;define

    (define (%critical-bash-command? cmd)
      (or (string-contains cmd "rm -rf")
        (string-contains cmd "rm -f")
        (string-contains cmd "rm ")
        (string-starts? (string-trim-both cmd) "rm ")
        (string=? "rm" (string-trim-both cmd))
        (string-contains cmd "git reset --hard")
        (string-contains cmd "git push")
        (string-contains cmd "dd if=")
        (string-contains cmd "mkfs")
      ) ;or
    ) ;define

    (define (%high-bash-command? cmd)
      (or (string-contains cmd "git clean")
        (string-contains cmd "kill ")
        (string-starts? (string-trim-both cmd) "kill ")
        (string-contains cmd "killall")
        (string-contains cmd "chmod -R")
        (string-contains cmd "chown -R")
        (string-contains cmd "shutdown")
        (string-contains cmd "reboot")
      ) ;or
    ) ;define

    (define (classify-tool-danger tool-name args)
      (%validate-detect-args tool-name args "classify-tool-danger")
      (cond ((string=? tool-name "write") 'high)
            ((string=? tool-name "edit") 'medium)
            ((string=? tool-name "bash")
             (let ((cmd (%get-arg-val args "command")))
               (cond ((not (string? cmd)) #f)
                     ((%critical-bash-command? cmd) 'critical)
                     ((%high-bash-command? cmd) 'high)
                     (else #f)
               ) ;cond
             ) ;let
            ) ;
            (else #f)
      ) ;cond
    ) ;define

    (define (tool-dangerous? tool-name args)
      (%validate-detect-args tool-name args "tool-dangerous?")
      (and (classify-tool-danger tool-name args) #t)
    ) ;define

    (define (extract-target-summary tool-name args)
      (%validate-detect-args tool-name args "extract-target-summary")
      (cond
       ((string=? tool-name "write")
        (let ((p (%get-arg-val args "path")))
          (if (string? p) (string-append "write: " p) "write")
        ) ;let
       ) ;
       ((string=? tool-name "edit")
        (let ((p (%get-arg-val args "path")))
          (if (string? p) (string-append "edit: " p) "edit")
        ) ;let
       ) ;
       ((string=? tool-name "bash")
        (let ((c (%get-arg-val args "command")))
          (if (string? c) (string-append "bash: " c) "bash")
        ) ;let
       ) ;
       (else tool-name)
      ) ;cond
    ) ;define

    (define (extract-diff-preview tool-name args)
      (%validate-detect-args tool-name args "extract-diff-preview")
      (cond
       ((string=? tool-name "write")
        (let ((c (%get-arg-val args "content")))
          (if (string? c) c "")
        ) ;let
       ) ;
       ((string=? tool-name "edit")
        (let ((edits (%get-arg-val args "edits")))
          (if (and (list? edits) (pair? edits))
            (let* ((first-edit (car edits))
                   (old-t (%get-arg-val first-edit "oldText"))
                   (new-t (%get-arg-val first-edit "newText"))
                  ) ;
              (string-append (if (string? old-t) (string-append "- " old-t "\n") "")
                (if (string? new-t) (string-append "+ " new-t) "")
              ) ;string-append
            ) ;let*
            ""
          ) ;if
        ) ;let
       ) ;
       ((string=? tool-name "bash")
        (let ((c (%get-arg-val args "command")))
          (if (string? c) c "")
        ) ;let
       ) ;
       (else "")
      ) ;cond
    ) ;define

    (define (make-confirm-tool-call-interceptor)
      (lambda (call-payload)
        (let* ((id (%get-arg-val call-payload "id"))
               (name (or (%get-arg-val call-payload "name") (%get-arg-val call-payload "tool") "")
               ) ;name
               (input (%get-arg-val call-payload "input"))
               (args (if (list? input) input '()))
               (danger (and (string? name) (list? args) (classify-tool-danger name args)))
              ) ;
          (if danger
            (make-confirm-dialog
              :id             (if (string? id) id "")
              :tool-name      name
              :args           args
              :danger-level   danger
              :target-summary (extract-target-summary name args)
              :diff-preview   (extract-diff-preview name args)
            ) ;make-confirm-dialog
            #f
          ) ;if
        ) ;let*
      ) ;lambda
    ) ;define

  ) ;begin
) ;define-library

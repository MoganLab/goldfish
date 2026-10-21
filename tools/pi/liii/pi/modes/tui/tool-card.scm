;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes tui tool-card)
  (import (scheme base)
    (scheme write)
    (liii base)
    (liii error)
    (liii string)
    (liii tui text-width)
    (liii tui loader)
  ) ;import
  (export <pi-tui-tool-card> make-pi-tui-tool-card pi-tui-tool-card?
    tool-card-id tool-card-tool-name tool-card-args-summary tool-card-status
    tool-card-elapsed tool-card-result-summary tool-card-spinner
    tool-card-start-time tool-card-tick! tool-card-complete! tool-card-fail!
    render-tool-card format-tool-args-summary format-tool-result-summary
  ) ;export
  (begin

    (define-record-type <pi-tui-tool-card>
      (%make-pi-tui-tool-card id tool-name args-summary status elapsed
        result-summary spinner start-time
      ) ;%make-pi-tui-tool-card
      pi-tui-tool-card?
      (id tool-card-id %card-set-id!)
      (tool-name tool-card-tool-name %card-set-tool-name!)
      (args-summary tool-card-args-summary %card-set-args-summary!)
      (status tool-card-status %card-set-status!)
      (elapsed tool-card-elapsed %card-set-elapsed!)
      (result-summary tool-card-result-summary %card-set-result-summary!)
      (spinner tool-card-spinner %card-set-spinner!)
      (start-time tool-card-start-time %card-set-start-time!)
    ) ;define-record-type

    (define <pi-tui-tool-card> '<pi-tui-tool-card>)

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

    (define (%validate-card card caller)
      (unless (pi-tui-tool-card? card)
        (type-error (string-append caller ": expected <pi-tui-tool-card>"))
      ) ;unless
    ) ;define

    (define (make-pi-tui-tool-card . opts)
      (let* ((id (%keyword-value :id opts ""))
             (tool-name (%keyword-value :tool-name opts ""))
             (args-summary (%keyword-value :args-summary opts ""))
             (status (%keyword-value :status opts 'running))
             (elapsed (%keyword-value :elapsed opts 0.0))
             (result-summary (%keyword-value :result-summary opts ""))
             (spinner-val (%keyword-value :spinner opts #f))
             (start-time (%keyword-value :start-time opts #f))
            ) ;
        (unless (string? id)
          (type-error "make-pi-tui-tool-card: expected string for :id")
        ) ;unless
        (unless (string? tool-name)
          (type-error "make-pi-tui-tool-card: expected string for :tool-name")
        ) ;unless
        (unless (string? args-summary)
          (type-error "make-pi-tui-tool-card: expected string for :args-summary")
        ) ;unless
        (unless (memq status '(running success failed))
          (type-error "make-pi-tui-tool-card: expected status 'running, 'success, or 'failed"
          ) ;type-error
        ) ;unless
        (unless (number? elapsed)
          (type-error "make-pi-tui-tool-card: expected number for :elapsed")
        ) ;unless
        (unless (string? result-summary)
          (type-error "make-pi-tui-tool-card: expected string for :result-summary")
        ) ;unless
        (let ((sp (if (spinner? spinner-val) spinner-val (make-spinner))))
          (when (not (eq? status 'running))
            (spinner-stop! sp)
          ) ;when
          (%make-pi-tui-tool-card id
            tool-name
            args-summary
            status
            (exact->inexact elapsed)
            result-summary
            sp
            start-time
          ) ;%make-pi-tui-tool-card
        ) ;let
      ) ;let*
    ) ;define

    (define (tool-card-tick! card)
      (%validate-card card "tool-card-tick!")
      (when (eq? (tool-card-status card) 'running)
        (spinner-tick! (tool-card-spinner card))
      ) ;when
      card
    ) ;define

    (define (tool-card-complete! card elapsed result-summary)
      (%validate-card card "tool-card-complete!")
      (unless (number? elapsed)
        (type-error "tool-card-complete!: expected number for elapsed")
      ) ;unless
      (unless (string? result-summary)
        (type-error "tool-card-complete!: expected string for result-summary")
      ) ;unless
      (%card-set-status! card 'success)
      (%card-set-elapsed! card (exact->inexact elapsed))
      (%card-set-result-summary! card result-summary)
      (spinner-stop! (tool-card-spinner card))
      card
    ) ;define

    (define (tool-card-fail! card elapsed result-summary)
      (%validate-card card "tool-card-fail!")
      (unless (number? elapsed)
        (type-error "tool-card-fail!: expected number for elapsed")
      ) ;unless
      (unless (string? result-summary)
        (type-error "tool-card-fail!: expected string for result-summary")
      ) ;unless
      (%card-set-status! card 'failed)
      (%card-set-elapsed! card (exact->inexact elapsed))
      (%card-set-result-summary! card result-summary)
      (spinner-stop! (tool-card-spinner card))
      card
    ) ;define

    (define (%format-elapsed sec)
      (let* ((rounded (/ (round (* sec 10.0)) 10.0)) (str (number->string rounded)))
        (if (not (string-contains str ".")) (string-append str ".0") str)
      ) ;let*
    ) ;define

    (define (format-tool-args-summary tool-name args)
      (cond ((string? args) args)
            ((pair? args)
             (cond ((assoc "command" args) (cdr (assoc "command" args)))
                   ((assoc 'command args) (cdr (assoc 'command args)))
                   ((assoc "path" args) (cdr (assoc "path" args)))
                   ((assoc 'path args) (cdr (assoc 'path args)))
                   ((assoc "pattern" args) (cdr (assoc "pattern" args)))
                   ((assoc 'pattern args) (cdr (assoc 'pattern args)))
                   ((assoc "query" args) (cdr (assoc "query" args)))
                   ((assoc 'query args) (cdr (assoc 'query args)))
                   (else
                     (let loop
                       ((rest args) (acc ""))
                       (if (null? rest)
                         acc
                         (let* ((p (car rest))
                                (k (if (pair? p) (car p) ""))
                                (v (if (pair? p) (cdr p) ""))
                                (entry (string-append (if (string=? acc "") "" " ")
                                         (object->string k)
                                         "="
                                         (object->string v)
                                       ) ;string-append
                                ) ;entry
                               ) ;
                           (loop (cdr rest) (string-append acc entry))
                         ) ;let*
                       ) ;if
                     ) ;let
                   ) ;else
             ) ;cond
            ) ;
            (else "")
      ) ;cond
    ) ;define

    (define (format-tool-result-summary raw-res is-err?)
      (cond
       ((string? raw-res)
        (let* ((lines (string-split raw-res "\n"))
               (first-line (if (null? lines) "" (car lines)))
              ) ;
          (if (> (string-length first-line) 60)
            (string-append (substring first-line 0 57) "...")
            first-line
          ) ;if
        ) ;let*
       ) ;
       (else (object->string raw-res))
      ) ;cond
    ) ;define

    (define (render-tool-card card width)
      (%validate-card card "render-tool-card")
      (unless (and (exact-integer? width) (> width 0))
        (type-error "render-tool-card: expected positive integer width")
      ) ;unless
      (let* ((status (tool-card-status card))
             (tool-name (tool-card-tool-name card))
             (args-summary (tool-card-args-summary card))
             (elapsed (tool-card-elapsed card))
             (res-summary (tool-card-result-summary card))
            ) ;
        (case status
         ((running)
          (let* ((frame (spinner-current-frame (tool-card-spinner card)))
                 (text (if (string=? args-summary "")
                         tool-name
                         (string-append tool-name ": " args-summary)
                       ) ;if
                 ) ;text
                 (line (string-append "\x1b;[33m[" frame "]\x1b;[0m " text))
                ) ;
            (list (truncate-to-width line width ""))
          ) ;let*
         ) ;
         ((success)
          (let* ((time-str (string-append "(" (%format-elapsed elapsed) "s)"))
                 (line1 (string-append "\x1b;[32m[✓]\x1b;[0m " tool-name " " time-str))
                 (lines (list (truncate-to-width line1 width "")))
                ) ;
            (if (string=? res-summary "")
              lines
              (let* ((line2 (string-append "\x1b;[90m  " res-summary "\x1b;[0m")))
                (append lines (list (truncate-to-width line2 width "")))
              ) ;let*
            ) ;if
          ) ;let*
         ) ;
         ((failed)
          (let* ((time-str (string-append "(" (%format-elapsed elapsed) "s)"))
                 (line1 (string-append "\x1b;[31m[✗]\x1b;[0m " tool-name ": failed " time-str))
                 (lines (list (truncate-to-width line1 width "")))
                ) ;
            (if (string=? res-summary "")
              lines
              (let* ((line2 (string-append "\x1b;[90m  " res-summary "\x1b;[0m")))
                (append lines (list (truncate-to-width line2 width "")))
              ) ;let*
            ) ;if
          ) ;let*
         ) ;
         (else '())
        ) ;case
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

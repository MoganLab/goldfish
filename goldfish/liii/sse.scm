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

(define-library (liii sse)
  (export <sse-event> make-sse-event sse-event? sse-event-event sse-event-data
    sse-event-id sse-event-retry sse-event-json read-sse-event make-sse-decoder
    sse-event->string write-sse-event
  ) ;export
  (import (scheme base) (liii base) (liii error) (liii json) (liii string))
  (begin

    (define-record-type <sse-event>
      (%make-sse-event event data id retry)
      sse-event?
      (event sse-event-event)
      (data sse-event-data)
      (id sse-event-id)
      (retry sse-event-retry)
    ) ;define-record-type

    (define <sse-event> '<sse-event>)

    (define (%normalize-event ev)
      (cond
       ((or (not ev) (and (string? ev) (string=? ev ""))) "message")
       ((string? ev) ev)
       (else (type-error "make-sse-event: event must be a string or #f"))
      ) ;cond
    ) ;define

    (define (make-sse-event . args)
      (let* ((n (length args))
             (ev-arg (if (> n 1) (car args) "message"))
             (data (cond ((= n 1) (car args))
                         ((<= 2 n 4) (cadr args))
                         (else (type-error "make-sse-event: expected 1 to 4 arguments"))
                   ) ;cond
             ) ;data
             (id (and (> n 2) (caddr args)))
             (retry (and (= n 4) (cadddr args)))
             (ev (%normalize-event ev-arg))
            ) ;
        (unless (string? data)
          (type-error "make-sse-event: data must be a string")
        ) ;unless
        (when (and id (not (string? id)))
          (type-error "make-sse-event: id must be a string or #f")
        ) ;when
        (when
          (and retry (not (and (integer? retry) (exact? retry))))
          (type-error "make-sse-event: retry must be an integer or #f")
        ) ;when
        (when (and retry (< retry 0))
          (value-error "make-sse-event: retry must be non-negative")
        ) ;when
        (%make-sse-event ev data id retry)
      ) ;let*
    ) ;define

    (define (sse-event-json event)
      (unless (sse-event? event)
        (type-error "sse-event-json: expected <sse-event>")
      ) ;unless
      (string->json (sse-event-data event))
    ) ;define

    (define (strip-eol line)
      (let* ((n (string-length line))
             (has-nl
               (and (> n 0) (char=? (string-ref line (- n 1)) #\newline))
             ) ;has-nl
             (n1 (if has-nl (- n 1) n))
             (has-cr
               (and (> n1 0) (char=? (string-ref line (- n1 1)) #\return))
             ) ;has-cr
             (end (if has-cr (- n1 1) n1))
            ) ;
        (if (= end n) line (substring line 0 end))
      ) ;let*
    ) ;define

    (define (parse-sse-line line)
      (let ((i (string-index line #\:)))
        (if (not i)
          (values line "")
          (let ((s
                  (if
                    (and (< (+ i 1) (string-length line))
                      (char=? (string-ref line (+ i 1)) #\space)
                    ) ;and
                    (+ i 2)
                    (+ i 1)
                  ) ;if
                ) ;s
               ) ;
            (values (substring line 0 i) (substring line s (string-length line)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%parse-sse-retry val)
      (let ((n (string->number val 10)))
        (and (exact-integer? n) (>= n 0) n)
      ) ;let
    ) ;define

    (define (make-sse-decoder on-event)
      (unless (procedure? on-event)
        (type-error "make-sse-decoder: on-event must be a procedure")
      ) ;unless
      (let ((buffer "")
            (cur-event "message")
            (cur-lines '())
            (cur-id #f)
            (cur-retry #f)
            (has-field #f)
           ) ;

        (define (dispatch!)
          (when has-field
            (let ((ev (make-sse-event cur-event
                        (string-join (reverse cur-lines) "\n")
                        cur-id
                        cur-retry
                      ) ;make-sse-event
                  ) ;ev
                 ) ;
              (set! cur-event "message")
              (set! cur-lines '())
              (set! cur-id #f)
              (set! cur-retry #f)
              (set! has-field #f)
              (on-event ev)
            ) ;let
          ) ;when
        ) ;define

        (define (feed-field! name val)
          (cond ((string=? name "data")
                 (set! cur-lines (cons val cur-lines))
                 (set! has-field #t)
                ) ;
                ((string=? name "event") (set! cur-event val) (set! has-field #t))
                ((string=? name "id")
                 (unless (string-index val #\null)
                   (set! cur-id val)
                   (set! has-field #t)
                 ) ;unless
                ) ;
                ((string=? name "retry")
                 (let ((n (%parse-sse-retry val)))
                   (when n
                     (set! cur-retry n)
                     (set! has-field #t)
                   ) ;when
                 ) ;let
                ) ;
          ) ;cond
        ) ;define

        (define (feed-line! line)
          (let ((len (string-length line)))
            (cond ((= len 0) (dispatch!))
                  ((char=? (string-ref line 0) #\:) #f)
                  (else
                    (let-values (((name val) (parse-sse-line line)))
                      (feed-field! name val)
                    ) ;let-values
                  ) ;else
            ) ;cond
          ) ;let
        ) ;define

        (define (feed-chunk! chunk)
          (set! buffer (string-append buffer chunk))
          (let loop
            ()
            (let ((nl (string-index buffer #\newline)))
              (when nl
                (let ((raw (substring buffer 0 nl)))
                  (set! buffer (substring buffer (+ nl 1) (string-length buffer)))
                  (feed-line! (strip-eol raw))
                  (loop)
                ) ;let
              ) ;when
            ) ;let
          ) ;let
        ) ;define

        (lambda (chunk)
          (cond
           ((eof-object? chunk)
            (when (> (string-length buffer) 0)
              (let ((line (strip-eol buffer)))
                (set! buffer "")
                (feed-line! line)
              ) ;let
            ) ;when
            (dispatch!)
           ) ;
           ((string? chunk) (feed-chunk! chunk))
           (else (type-error "sse-decoder: chunk must be a string or eof-object"))
          ) ;cond
        ) ;lambda
      ) ;let
    ) ;define

    (define (read-sse-event . args)
      (let* ((port
               (cond ((null? args) (current-input-port))
                     ((and (null? (cdr args)) (input-port? (car args))) (car args))
                     ((null? (cdr args)) (type-error "read-sse-event: expected input-port"))
                     (else (type-error "read-sse-event: expected at most 1 argument"))
               ) ;cond
             ) ;port
             (result #f)
             (decoder (make-sse-decoder (lambda (ev) (set! result ev))))
            ) ;
        (let loop
          ()
          (let ((line (read-line port)))
            (cond ((eof-object? line) (decoder line) (or result line))
                  (else (decoder (string-append (strip-eol line) "\n")) (or result (loop)))
            ) ;cond
          ) ;let
        ) ;let
      ) ;let*
    ) ;define

    (define (sse-event->string event)
      (unless (sse-event? event)
        (type-error "sse-event->string: expected <sse-event>")
      ) ;unless
      (let ((ev (sse-event-event event))
            (id (sse-event-id event))
            (retry (sse-event-retry event))
            (p (open-output-string))
           ) ;
        (when (and (string? ev) (not (string=? ev "message")) (not (string=? ev "")))
          (display (string-append "event: " ev "\n") p)
        ) ;when
        (for-each (lambda (line) (display (string-append "data: " line "\n") p))
          (string-split (sse-event-data event) "\n")
        ) ;for-each
        (when (string? id)
          (display (string-append "id: " id "\n") p)
        ) ;when
        (when (and (integer? retry) (>= retry 0))
          (display (string-append "retry: " (number->string retry) "\n") p)
        ) ;when
        (newline p)
        (get-output-string p)
      ) ;let
    ) ;define

    (define (write-sse-event event . args)
      (unless (sse-event? event)
        (type-error "write-sse-event: expected <sse-event>")
      ) ;unless
      (let ((p
              (cond ((null? args) (current-output-port))
                    ((and (null? (cdr args)) (output-port? (car args))) (car args))
                    ((null? (cdr args)) (type-error "write-sse-event: expected output-port"))
                    (else (type-error "write-sse-event: expected at most 2 arguments"))
              ) ;cond
            ) ;p
           ) ;
        (display (sse-event->string event) p)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

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

(define-library (liii pi truncate)
  (import (scheme base) (scheme char) (liii string))
  (export PI_DEFAULT_MAX_LINES PI_DEFAULT_MAX_BYTES PI_GREP_MAX_LINE_LENGTH
    pi-byte-length pi-format-size pi-truncate-line pi-truncate-head
    pi-truncate-tail
  ) ;export
  (begin

    (define PI_DEFAULT_MAX_LINES 2000)
    (define PI_DEFAULT_MAX_BYTES 51200)
    (define PI_GREP_MAX_LINE_LENGTH 500)

    (define (pi-byte-length str)
      (bytevector-length (string->utf8 str))
    ) ;define

    (define (pi-format-size bytes)
      (cond ((< bytes 1024) (string-append (number->string bytes) "B"))
            ((< bytes (* 1024 1024))
             (let* ((scaled (/ bytes 1024.0))
                    (rounded (/ (round (* scaled 10)) 10))
                    (int-part (inexact->exact (floor rounded)))
                    (dec-part
                      (inexact->exact (round (* (- rounded int-part) 10)))
                    ) ;dec-part
                   ) ;
               (string-append (number->string int-part) "." (number->string dec-part) "KB")
             ) ;let*
            ) ;
            (else
              (let* ((scaled (/ bytes (* 1024.0 1024.0)))
                     (rounded (/ (round (* scaled 10)) 10))
                     (int-part (inexact->exact (floor rounded)))
                     (dec-part
                       (inexact->exact (round (* (- rounded int-part) 10)))
                     ) ;dec-part
                    ) ;
                (string-append (number->string int-part) "." (number->string dec-part) "MB")
              ) ;let*
            ) ;else
      ) ;cond
    ) ;define

    (define (pi-truncate-line line . max-chars-opt)
      (let* ((max-chars (if (pair? max-chars-opt) (car max-chars-opt) PI_GREP_MAX_LINE_LENGTH)
             ) ;max-chars
             (len (string-length line))
            ) ;
        (if (> len max-chars) (values (substring line 0 max-chars) #t) (values line #f))
      ) ;let*
    ) ;define

    (define (pi-split-lines content)
      (if (string-null? content)
        '()
        (let ((lines (string-split content "\n")))
          (if
            (and (string-ends? content "\n")
              (not (null? lines))
              (string-null? (car (reverse lines)))
            ) ;and
            (reverse (cdr (reverse lines)))
            lines
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (%trunc-result content truncated by total-lines total-bytes
              out-lines out-bytes flag-key flag-val
            ) ;%trunc-result
      (list (cons 'content content)
        (cons 'truncated truncated)
        (cons 'truncated-by by)
        (cons 'total-lines total-lines)
        (cons 'total-bytes total-bytes)
        (cons 'output-lines out-lines)
        (cons 'output-bytes out-bytes)
        (cons flag-key flag-val)
      ) ;list
    ) ;define

    (define (%truncate-head-loop lines max-lines max-bytes total-lines total-bytes)
      (let loop
        ((rest lines) (collected '()) (curr-bytes 0) (count 0))
        (cond
         ((or (null? rest) (>= count max-lines))
          (let* ((out-lines (reverse collected))
                 (out-str (string-join out-lines "\n"))
                 (out-bytes (pi-byte-length out-str))
                 (by (if (>= count max-lines) 'lines 'bytes))
                ) ;
            (%trunc-result out-str
              #t
              by
              total-lines
              total-bytes
              (length out-lines)
              out-bytes
              'first-line-exceeds-limit?
              #f
            ) ;%trunc-result
          ) ;let*
         ) ;
         (else
           (let* ((line (car rest))
                  (line-bytes (+ (pi-byte-length line) (if (null? collected) 0 1)))
                 ) ;
             (if (> (+ curr-bytes line-bytes) max-bytes)
               (let* ((out-lines (reverse collected))
                      (out-str (string-join out-lines "\n"))
                      (out-bytes (pi-byte-length out-str))
                     ) ;
                 (%trunc-result out-str
                   #t
                   'bytes
                   total-lines
                   total-bytes
                   (length out-lines)
                   out-bytes
                   'first-line-exceeds-limit?
                   #f
                 ) ;%trunc-result
               ) ;let*
               (loop (cdr rest) (cons line collected) (+ curr-bytes line-bytes) (+ count 1))
             ) ;if
           ) ;let*
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-truncate-head content . opts)
      (let* ((max-lines (if (pair? opts) (car opts) PI_DEFAULT_MAX_LINES))
             (max-bytes
               (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) PI_DEFAULT_MAX_BYTES)
             ) ;max-bytes
             (total-bytes (pi-byte-length content))
             (lines (pi-split-lines content))
             (total-lines (length lines))
            ) ;
        (cond ((and (<= total-lines max-lines) (<= total-bytes max-bytes))
               (%trunc-result content
                 #f
                 #f
                 total-lines
                 total-bytes
                 total-lines
                 total-bytes
                 'first-line-exceeds-limit?
                 #f
               ) ;%trunc-result
              ) ;
              ((and (pair? lines) (> (pi-byte-length (car lines)) max-bytes))
               (%trunc-result ""
                 #t
                 'bytes
                 total-lines
                 total-bytes
                 0
                 0
                 'first-line-exceeds-limit?
                 #t
               ) ;%trunc-result
              ) ;
              (else (%truncate-head-loop lines max-lines max-bytes total-lines total-bytes))
        ) ;cond
      ) ;let*
    ) ;define

    (define (%truncate-tail-overflow line max-bytes total-lines total-bytes)
      (let* ((sub-len (min max-bytes (string-length line)))
             (sub-line (substring line (- (string-length line) sub-len) (string-length line))
             ) ;sub-line
            ) ;
        (%trunc-result sub-line
          #t
          'bytes
          total-lines
          total-bytes
          1
          (pi-byte-length sub-line)
          'last-line-partial?
          #t
        ) ;%trunc-result
      ) ;let*
    ) ;define

    (define (%truncate-tail-loop lines max-lines max-bytes total-lines total-bytes)
      (let loop
        ((rev-lines (reverse lines)) (collected '()) (curr-bytes 0) (count 0))
        (cond
         ((or (null? rev-lines) (>= count max-lines))
          (let* ((out-str (string-join collected "\n")) (out-bytes (pi-byte-length out-str)))
            (%trunc-result out-str
              #t
              (if (>= count max-lines) 'lines 'bytes)
              total-lines
              total-bytes
              (length collected)
              out-bytes
              'last-line-partial?
              #f
            ) ;%trunc-result
          ) ;let*
         ) ;
         (else
           (let* ((line (car rev-lines))
                  (line-bytes (+ (pi-byte-length line) (if (null? collected) 0 1)))
                 ) ;
             (cond
              ((and (> (+ curr-bytes line-bytes) max-bytes) (null? collected))
               (%truncate-tail-overflow line max-bytes total-lines total-bytes)
              ) ;
              ((> (+ curr-bytes line-bytes) max-bytes)
               (let* ((out-str (string-join collected "\n")) (out-bytes (pi-byte-length out-str)))
                 (%trunc-result out-str
                   #t
                   'bytes
                   total-lines
                   total-bytes
                   (length collected)
                   out-bytes
                   'last-line-partial?
                   #f
                 ) ;%trunc-result
               ) ;let*
              ) ;
              (else (loop (cdr rev-lines)
                      (cons line collected)
                      (+ curr-bytes line-bytes)
                      (+ count 1)
                    ) ;loop
              ) ;else
             ) ;cond
           ) ;let*
         ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pi-truncate-tail content . opts)
      (let* ((max-lines (if (pair? opts) (car opts) PI_DEFAULT_MAX_LINES))
             (max-bytes
               (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) PI_DEFAULT_MAX_BYTES)
             ) ;max-bytes
             (total-bytes (pi-byte-length content))
             (lines (pi-split-lines content))
             (total-lines (length lines))
            ) ;
        (if (and (<= total-lines max-lines) (<= total-bytes max-bytes))
          (%trunc-result content
            #f
            #f
            total-lines
            total-bytes
            total-lines
            total-bytes
            'last-line-partial?
            #f
          ) ;%trunc-result
          (%truncate-tail-loop lines max-lines max-bytes total-lines total-bytes)
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library

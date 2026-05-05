;;
;; Copyright (C) 2024 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;  http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; SRFI 233: INI files
;; Authors: John Cowan (spec), Arvydas Silanskas (reference implementation)

(define-library (srfi srfi-233)
  (import (scheme base) (liii base))
  (export make-ini-file-generator make-ini-file-accumulator)
  (begin

    ;; Whitespace for SRFI 233: space and tab only
    (define (ini-whitespace? ch)
      (or (char=? ch #\space) (char=? ch #\tab))
    ) ;define

    ;; Find first occurrence of char in string, returns integer index or #f
    (define (char-index str ch)
      (let ((len (string-length str)))
        (let loop
          ((i 0))
          (cond ((= i len) #f)
                ((char=? (string-ref str i) ch) i)
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; Trim leading and trailing whitespace (space/tab)
    (define (ini-trim str)
      (let ((len (string-length str)))
        (let loop-start
          ((i 0))
          (if (and (< i len) (ini-whitespace? (string-ref str i)))
            (loop-start (+ i 1))
            (let loop-end
              ((j (- len 1)))
              (if (and (>= j i) (ini-whitespace? (string-ref str j)))
                (loop-end (- j 1))
                (substring str i (+ j 1))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; Remove comment from a line: find first comment-delim and truncate
    (define (remove-comment line comment-delim)
      (let ((idx (char-index line comment-delim)))
        (if idx (substring line 0 idx) line)
      ) ;let
    ) ;define

    ;; Split string at first occurrence of a character
    ;; Returns (before . after) or #f if not found
    (define (split-at-first str sep-char)
      (let ((idx (char-index str sep-char)))
        (if idx
          (cons (substring str 0 idx) (substring str (+ idx 1) (string-length str)))
          #f
        ) ;if
      ) ;let
    ) ;define

    ;; Parse a section header line like "[name]"
    ;; Returns section name string, or #f if not a section header
    (define (parse-section-name line)
      (let* ((trimmed (ini-trim line)) (len (string-length trimmed)))
        (if (and (> len 1)
              (char=? (string-ref trimmed 0) #\[)
              (char=? (string-ref trimmed (- len 1)) #\])
            ) ;and
          (ini-trim (substring trimmed 1 (- len 1)))
          #f
        ) ;if
      ) ;let*
    ) ;define

    ;; make-ini-file-generator
    ;; Returns a thunk that yields (section key value) lists from the INI port
    (define* (make-ini-file-generator inport (key-value-sep #\=) (comment-delim #\;))
      (let ((current-section #f))
        (lambda ()
          (let loop
            ()
            (let ((line (read-line inport)))
              (cond ((eof-object? line) (eof-object))
                    (else (let* ((no-comment (remove-comment line comment-delim))
                                 (trimmed (ini-trim no-comment))
                                ) ;
                            (if (string=? trimmed "")
                              (loop)
                              (let ((section-name (parse-section-name trimmed)))
                                (cond (section-name (set! current-section (string->symbol section-name)) (loop))
                                      (else (let ((kv (split-at-first trimmed key-value-sep)))
                                              (if kv
                                                (let ((key-str (ini-trim (car kv))) (val-str (ini-trim (cdr kv))))
                                                  (list current-section (string->symbol key-str) val-str)
                                                ) ;let
                                                (list current-section (string->symbol trimmed) #f)
                                              ) ;if
                                            ) ;let
                                      ) ;else
                                ) ;cond
                              ) ;let
                            ) ;if
                          ) ;let*
                    ) ;else
              ) ;cond
            ) ;let
          ) ;let
        ) ;lambda
      ) ;let
    ) ;define*

    ;; make-ini-file-accumulator
    ;; Returns a procedure that writes INI data to outport
    (define* (make-ini-file-accumulator outport (key-value-sep #\=) (comment-delim #\;))
      (let ((current-section '*ini-none*) (closed #f))
        (lambda (item)
          (cond (closed (error 'ini-file "accumulator called after EOF"))
                ((eof-object? item) (set! closed #t) (eof-object))
                ((string? item)
                 (when (char-index item #\newline)
                   (error 'ini-file "comment string contains newline")
                 ) ;when
                 (display comment-delim outport)
                 (display #\space outport)
                 (display item outport)
                 (newline outport)
                ) ;
                ((and (list? item) (= (length item) 3))
                 (let ((section (car item)) (key (cadr item)) (value (caddr item)))
                   ;; Write section header if changed
                   (when (not (eq? section current-section))
                     (when section
                       (display #\[ outport)
                       (display (symbol->string section) outport)
                       (display #\] outport)
                       (newline outport)
                     ) ;when
                     (set! current-section section)
                   ) ;when
                   ;; Write key-value line
                   (display (symbol->string key) outport)
                   (when value
                     (display key-value-sep outport)
                     (display value outport)
                   ) ;when
                   (newline outport)
                 ) ;let
                ) ;
                (else (error 'ini-file "invalid argument to accumulator"))
          ) ;cond
        ) ;lambda
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library

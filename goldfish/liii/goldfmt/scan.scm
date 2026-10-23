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
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(define-library (liii goldfmt scan)
  (export scan scan-string scan-source-string scan-file stem-mode?
    call-with-stem-mode
  ) ;export
  (import (liii base)
    (liii path)
    (liii goldfmt scan normalize)
    (liii goldfmt scan literal)
    (rename (liii goldfmt tokenize)
      (tokenize source-tokenize)
      (tokens->string source-tokens->string)
    ) ;rename
  ) ;import
  (begin
    (define (scan-string-fast str)
      (let ((port (open-input-string str)))
        (let loop
          ((results '()))
          (let ((datum (read port)))
            (if (eof-object? datum)
              (list->vector (reverse results))
              (loop (cons (scan-datum (normalize-datum datum) 0) results))
            ) ;if
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    (define (scan-string str)
      (if (needs-rewrite-reader-literals? str)
        (scan-string-fast (rewrite-reader-literals str))
        (scan-string-fast str)
      ) ;if
    ) ;define

    (define (scan-source-string raw-content)
      (let* ((scanned (source-tokenize raw-content))
             (leading-blanks
               (let loop
                 ((i 0) (count 0))
                 (if (>= i (string-length raw-content))
                   count
                   (let ((c (string-ref raw-content i)))
                     (cond ((char=? c #\newline) (loop (+ i 1) (+ count 1)))
                           ((char=? c #\return) (loop (+ i 1) count))
                           ((or (char=? c #\space) (char=? c #\tab)) (loop (+ i 1) count))
                           (else count)
                     ) ;cond
                   ) ;let
                 ) ;if
               ) ;let
             ) ;leading-blanks
             (tokens-with-leading (if (> leading-blanks 0) (cons (cons 'newline leading-blanks) scanned) scanned)
             ) ;tokens-with-leading
             (tokens
               (if
                 (and (not (null? tokens-with-leading))
                   (> (string-length raw-content) 0)
                   (char=? (string-ref raw-content (- (string-length raw-content) 1)) #\newline)
                 ) ;and
                 (append tokens-with-leading (list (cons 'newline 1)))
                 tokens-with-leading
               ) ;if
             ) ;tokens
             (processed-content (source-tokens->string tokens))
            ) ;
        (scan-string processed-content)
      ) ;let*
    ) ;define

    (define (scan-file path)
      (scan-source-string (path-read-text path))
    ) ;define
  ) ;begin
) ;define-library

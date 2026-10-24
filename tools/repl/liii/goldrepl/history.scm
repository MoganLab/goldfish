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

(define-library (liii goldrepl history)
  (import (scheme base)
    (scheme file)
    (scheme write)
    (liii base)
    (liii list)
    (liii os)
    (liii path)
    (liii string)
    (liii tui)
  ) ;import
  (export %repl-history-dir %repl-history-file %repl-escape-history-entry
    %repl-unescape-history-entry %repl-history-file-lines %repl-load-history!
    %repl-save-history!
  ) ;export
  (begin

    (define (%repl-history-dir)
      (if (os-windows?)
        (let ((appdata (getenv "APPDATA")))
          (if (and appdata (not (string-null? appdata)))
            (string-append appdata "/goldfish")
            "."
          ) ;if
        ) ;let
        (let ((xdg-data (getenv "XDG_DATA_HOME")) (home (getenv "HOME")))
          (cond
           ((and xdg-data (not (string-null? xdg-data)))
            (string-append xdg-data "/goldfish")
           ) ;
           ((and home (not (string-null? home)))
            (string-append home "/.local/share/goldfish")
           ) ;
           (else ".")
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%repl-history-file)
      (string-append (%repl-history-dir) "/history")
    ) ;define

    (define (%repl-escape-history-entry entry)
      (let ((s1 (string-replace entry "\\" "\\\\")))
        (string-replace s1 "\n" "\\n")
      ) ;let
    ) ;define

    (define (%repl-unescape-history-entry line)
      (let ((placeholder (string (integer->char 1))))
        (let* ((s1 (string-replace line "\\\\" placeholder))
               (s2 (string-replace s1 "\\n" "\n"))
              ) ;
          (string-replace s2 placeholder "\\")
        ) ;let*
      ) ;let
    ) ;define

    (define (%repl-history-file-lines path)
      (if (not (path-file? path))
        (cons 0 '())
        (let* ((text (catch #t (lambda () (path-read-text path)) (lambda (t i) "")))
               (lines (string-split text "\n"))
              ) ;
          (let ((valid-lines
                  (filter
                    (lambda (line) (and (not (string-null? line)) (not (string-starts? line "#!"))))
                    lines
                  ) ;filter
                ) ;valid-lines
               ) ;
            (cons (length valid-lines) valid-lines)
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

    (define (%repl-load-history! hist)
      (let* ((path (%repl-history-file)) (res (%repl-history-file-lines path)))
        (for-each (lambda (raw) (history-push! hist (%repl-unescape-history-entry raw)))
          (cdr res)
        ) ;for-each
        res
      ) ;let*
    ) ;define

    (define (%repl-save-history! entries old-count)
      (let ((dir (%repl-history-dir)) (path (%repl-history-file)))
        (catch #t
          (lambda ()
            (path-mkdir dir #t #t)
            (let ((p (open-output-file path)))
              (display
                (string-append "#!isocline "
                  (number->string (+ old-count (length entries)))
                  "\n"
                ) ;string-append
                p
              ) ;display
              (for-each (lambda (e) (display (%repl-escape-history-entry e) p) (newline p))
                entries
              ) ;for-each
              (close-output-port p)
            ) ;let
          ) ;lambda
          (lambda (tag info) #f)
        ) ;catch
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

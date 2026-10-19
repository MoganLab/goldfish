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

(define-library (liii pi code-review files)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii path)
    (liii pi read)
    (liii pi bash)
    (liii pi code-review language)
  ) ;import
  (export PI_REVIEW_MAX_FILES PI_REVIEW_MAX_FILE_CHARS pi-parse-changed-files
    pi-changed-files-command pi-collect-changed-files pi-review-file-records
  ) ;export
  (begin

    (define PI_REVIEW_MAX_FILES 10)
    (define PI_REVIEW_MAX_FILE_CHARS 4000)

    (define %status-map
      '(("M" . "modified") ("A" . "added") ("R" . "renamed") ("C" . "copied"))
    ) ;define

    (define (%status-name code)
      (let ((pair (assoc code %status-map)))
        (if pair (cdr pair) #f)
      ) ;let
    ) ;define

    (define (%parse-line line)
      (if (string-null? (string-trim-both line))
        #f
        (let* ((parts (string-split line "\t"))
               (raw-status (if (pair? parts) (car parts) ""))
               (code (if (string-null? raw-status) "" (substring raw-status 0 1)))
               (status (%status-name code))
               (index (if (or (string=? code "R") (string=? code "C")) 2 1))
               (path (and (>= (length parts) (+ index 1)) (list-ref parts index)))
              ) ;
          (if (and status path (not (string-null? path)))
            (list (cons 'path path) (cons 'status status))
            #f
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (pi-parse-changed-files output)
      (filter (lambda (x) x) (map %parse-line (string-split (or output "") "\n")))
    ) ;define

    (define (%safe-ref? ref)
      (let loop
        ((i 0))
        (if (>= i (string-length ref))
          #t
          (let ((ch (string-ref ref i)))
            (if (or (char-alphabetic? ch)
                  (char-numeric? ch)
                  (memv ch '(#\. #\_ #\/ #\~ #\- #\^ #\@ #\+))
                ) ;or
              (loop (+ i 1))
              #f
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%shell-quote text)
      (if (%safe-ref? text)
        text
        (string-append "'" (string-replace text "'" "'\\''") "'")
      ) ;if
    ) ;define

    (define (pi-changed-files-command ref staged)
      (if staged
        "git diff --name-status --cached"
        (string-append "git diff --name-status " (%shell-quote ref))
      ) ;if
    ) ;define

    (define (%default-runner command timeout)
      (pi-execute-bash (list (cons "command" command) (cons "timeout" timeout))
        :timeout timeout
      ) ;pi-execute-bash
    ) ;define

    (define (%run-diff runner command timeout)
      (let-values (((text is-error?) (runner command timeout)))
        (if is-error? '() (pi-parse-changed-files text))
      ) ;let-values
    ) ;define

    (define (%take items n)
      (let loop
        ((rest items) (left n) (acc '()))
        (if (or (null? rest) (<= left 0))
          (reverse acc)
          (loop (cdr rest) (- left 1) (cons (car rest) acc))
        ) ;if
      ) ;let
    ) ;define

    (define (%paths-of files)
      (map (lambda (f) (cdr (assoc 'path f))) files)
    ) ;define

    (define (%make-result paths total source)
      (let ((truncated? (> total (length paths))))
        (list (cons 'paths paths)
          (cons 'total total)
          (cons 'source source)
          (cons 'truncated? truncated?)
          (cons 'message
            (if truncated?
              (string-append "Reviewing first "
                (number->string (length paths))
                " of "
                (number->string total)
                " changed files."
              ) ;string-append
              #f
            ) ;if
          ) ;cons
        ) ;list
      ) ;let
    ) ;define

    (define (%collect-diff-paths runner ref staged timeout)
      (let* ((primary (%run-diff runner (pi-changed-files-command ref staged) timeout))
             (primary-paths (%paths-of primary))
            ) ;
        (if (pair? primary-paths)
          (values primary-paths "diff")
          (let ((fallback-paths (%paths-of (%run-diff runner "git diff --name-status HEAD~1" timeout))
                ) ;fallback-paths
               ) ;
            (if (pair? fallback-paths) (values fallback-paths "fallback") (values '() #f))
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-collect-changed-files . opts)
      (let* ((ref (pi-keyword-value :ref opts "HEAD"))
             (staged (pi-keyword-value :staged opts #f))
             (files (pi-keyword-value :files opts #f))
             (runner (pi-keyword-value :runner opts %default-runner))
             (timeout (pi-keyword-value :timeout opts 120))
             (max-files (pi-keyword-value :max-files opts PI_REVIEW_MAX_FILES))
            ) ;
        (if (and (pair? files) (not (null? files)))
          (let ((total (length files)))
            (%make-result (%take files max-files) total "explicit")
          ) ;let
          (let-values (((all source) (%collect-diff-paths runner ref staged timeout)))
            (%make-result (%take all max-files) (length all) source)
          ) ;let-values
        ) ;if
      ) ;let*
    ) ;define

    (define (%truncate-content content)
      (if (<= (string-length content) PI_REVIEW_MAX_FILE_CHARS)
        content
        (string-append (substring content 0 PI_REVIEW_MAX_FILE_CHARS) "\n[truncated]")
      ) ;if
    ) ;define

    (define (%read-file-content file-path)
      (catch #t
        (lambda ()
          (if (path-exists? (path file-path)) (path-read-text (path file-path)) "")
        ) ;lambda
        (lambda _ "")
      ) ;catch
    ) ;define

    (define (pi-review-file-records paths)
      (map
        (lambda (file-path)
          (list (cons 'path file-path)
            (cons 'language (pi-detect-language file-path))
            (cons 'content (%truncate-content (%read-file-content file-path)))
          ) ;list
        ) ;lambda
        paths
      ) ;map
    ) ;define

  ) ;begin
) ;define-library

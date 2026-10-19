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

(define-library (liii pi code-review auto)
  (import (scheme base)
    (liii string)
    (liii pi read)
    (liii pi code-review language)
  ) ;import
  (export pi-build-review-injection pi-edited-files-from-tool-calls)
  (begin

    (define (%field key entry default)
      (let ((pair (assoc key entry)))
        (if pair (cdr pair) default)
      ) ;let
    ) ;define

    (define (%block-value key alt-key blk default)
      (let ((pair (or (assoc key blk) (assoc alt-key blk))))
        (if (pair? pair) (cdr pair) default)
      ) ;let
    ) ;define

    (define (%languages files)
      (let loop
        ((rest files) (acc '()))
        (cond ((null? rest) (reverse acc))
              (else
                (let ((lang (%field 'language (car rest) #f)))
                  (if (and lang (not (member lang acc)))
                    (loop (cdr rest) (cons lang acc))
                    (loop (cdr rest) acc)
                  ) ;if
                ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (%file-list files)
      (string-join (map (lambda (f) (%field 'path f "")) files) ", ")
    ) ;define

    (define (pi-build-review-injection files)
      (if (null? files)
        #f
        (string-append "\n\n## Code Review\n\n"
          "You edited: "
          (%file-list files)
          ". Before proceeding, briefly verify:"
          (pi-build-language-section (%languages files))
          "\n\nFix any issues found, then continue."
        ) ;string-append
      ) ;if
    ) ;define

    (define (%tool-name call)
      (%block-value "name" 'name call "")
    ) ;define

    (define (%tool-args call)
      (let ((args (%block-value "arguments" 'arguments call '())))
        (if (pair? args) args '())
      ) ;let
    ) ;define

    (define (%edited-path call)
      (let* ((name (%tool-name call)) (args (%tool-args call)))
        (if (or (string=? name "write") (string=? name "edit"))
          (let ((path (or (pi-arg-ref args "path" #f) (pi-arg-ref args "file_path" #f))))
            (and (string? path) (not (string-null? (string-trim-both path))) path)
          ) ;let
          #f
        ) ;if
      ) ;let*
    ) ;define

    (define (%has-path? files path)
      (cond ((null? files) #f)
            ((string=? (%field 'path (car files) "") path) #t)
            (else (%has-path? (cdr files) path))
      ) ;cond
    ) ;define

    (define (%push-edited files path)
      (if (%has-path? files path)
        files
        (append files
          (list (list (cons 'path path) (cons 'language (pi-detect-language path))))
        ) ;append
      ) ;if
    ) ;define

    (define (pi-edited-files-from-tool-calls tool-calls)
      (let loop
        ((rest tool-calls) (acc '()))
        (if (null? rest)
          acc
          (let ((path (%edited-path (car rest))))
            (if path (loop (cdr rest) (%push-edited acc path)) (loop (cdr rest) acc))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library

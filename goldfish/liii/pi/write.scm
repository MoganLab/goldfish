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

(define-library (liii pi write)
  (import (scheme base) (liii string) (liii path) (liii os) (liii pi read))
  (export pi-write-schema pi-execute-write)
  (begin

    (define (pi-write-schema)
      (list (cons "type" "object")
        (cons "properties"
          (list (cons "path"
                  (list (cons "type" "string")
                    (cons "description" "Path to the file to write (relative or absolute)")
                  ) ;list
                ) ;cons
            (cons "content"
              (list (cons "type" "string")
                (cons "description" "Content to write to the file")
              ) ;list
            ) ;cons
          ) ;list
        ) ;cons
        (cons "required" #("path" "content"))
      ) ;list
    ) ;define

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

    (define (pi-execute-write args . opts)
      (let* ((timed-out? (%keyword-value :timed-out? opts #f))
             (cwd (%keyword-value :cwd opts (getcwd)))
             (raw-path (pi-arg-ref args "path"))
             (content (pi-arg-ref args "content"))
            ) ;
        (when timed-out?
          (error 'error "Command timed out")
        ) ;when
        (unless (string? raw-path)
          (error 'error "path must be a string")
        ) ;unless
        (unless (string? content)
          (error 'error "content must be a string")
        ) ;unless
        (let* ((abs-path (pi-resolve-path raw-path cwd))
               (parent (path-parent (path abs-path)))
              ) ;
          (unless (path-exists? parent)
            (path-mkdir parent :parents #t :exist-ok #t)
          ) ;unless
          (path-write-text (path abs-path) content)
          (string-append "Successfully wrote to " raw-path)
        ) ;let*
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
